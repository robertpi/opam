(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamProcess.Job.Op
open OpamCompat

exception Process_error of OpamProcess.result
exception Internal_error of string
exception Command_not_found of string
exception File_not_found of string

let log fmt = OpamGlobals.log "SYSTEM" fmt

let internal_error fmt =
  Printf.ksprintf (fun str ->
    log "error: %s" str;
    raise (Internal_error str)
  ) fmt

let process_error r =
  if r.OpamProcess.r_signal = Some Sys.sigint then raise Sys.Break
  else raise (Process_error r)

let raise_on_process_error r =
  if OpamProcess.is_failure r then raise (Process_error r)

let command_not_found cmd =
  raise (Command_not_found cmd)
(*
module Sys2 = struct
  (* same as [Sys.is_directory] except for symlinks, which returns always [false]. *)
  let is_directory file =
    try Unix.( (lstat file).st_kind = S_DIR )
    with Unix.Unix_error _ as e -> raise (Sys_error (Printexc.to_string e))
end
*)

(* - File paths handling and base operations - *)

module FP = FilePath
module F = FileUtil

type filename = FP.filename
type dirname = FP.filename

let (/) = FilePath.concat

(* Separator for the PATH variables *)
let path_sep = match OpamGlobals.os () with
  | OpamGlobals.Win32 -> ';'
  | OpamGlobals.Cygwin | _ -> ':'


let mkdir dir =
  log "mkdir %s" dir;
  F.mkdir ~parent:true dir

let remove_dir dir =
  log "rmdir %s" dir;
  F.rm ~recurse:true [dir]

let remove_file file =
  log "rm %s" file;
  F.rm [file]

let remove file = F.rm ~recurse:true [file]

let list filter dir =
  try F.filter filter (F.ls dir)
  with Sys_error _ -> []

let rec_files dir =
  let rec aux acc dir =
    List.fold_left (fun acc f ->
        if F.test F.Is_dir f then aux acc f
        else f::acc)
      acc (list F.True dir)
  in
  List.sort FP.compare (aux [] dir)

let files dir = list (F.Not F.Is_dir) dir

let rec_dirs dir =
  let rec aux acc dir =
    List.fold_left (fun acc f ->
        if F.test F.Is_dir f then aux (f::acc) f
        else acc)
      acc (list F.True dir)
  in
  List.sort FP.compare
    (List.fold_left aux [] (list F.True dir))

let dirs dir =
  try list F.Is_dir dir with Sys_error _ -> []

let dir_is_empty dir =
  try list F.True dir <> [] with F.FileDoesntExist _ -> false

let real_path p =
  FilePath.reduce ~no_symlink:true (FilePath.make_absolute (FileUtil.pwd ()) p)

let mv src dst =
  if src <> dst then
    (mkdir (FP.dirname dst);
     F.mv src dst)

let copy src dst =
  if src <> dst then
    (mkdir (FP.dirname dst);
     F.cp ~follow:F.Follow ~recurse:false [src] dst)

let is_exec file = F.test (F.And (F.Is_file, F.Is_exec)) file

let install ?exec ~src ~dst () =
  if src <> dst then
    (if F.test F.Is_dir src then
       internal_error "Cannot install %s: it is a directory." src;
     if F.test F.Is_dir dst then
       internal_error "Cannot install to %s: it is a directory." dst;
     let exec = match exec with Some e -> e | None -> F.test F.Is_exec src in
     copy src dst;
     Unix.chmod dst (if exec then 0o755 else 0o644))

let link src dst =
  if src <> dst then
    (if F.test F.Exists src then
       if OpamGlobals.(os () = Win32) then
         copy src dst
       else
         (mkdir (Filename.dirname dst);
          if F.test F.Exists dst then
            remove_file dst;
          try
            log "ln -s %s %s" src dst;
            Unix.symlink src dst
          with Unix.Unix_error (Unix.EXDEV, _, _) ->
            (* Fall back to copy if hard links are not supported *)
            copy src dst)
     else
       internal_error "link: %s does not exist." src)


(* - Temp dir and file handling - *)

let temp_basename prefix =
  Printf.sprintf "%s-%d-%06x" prefix (Unix.getpid ()) (Random.int 0xFFFFFF)

let rec mk_temp_dir () =
  let s = Filename.get_temp_dir_name () / temp_basename "opam" in
  if F.test F.Exists s then
    mk_temp_dir ()
  else
    s

let temp_files = Hashtbl.create 1024
let check_remove_temp_dir = ref true

let rec temp_file ?dir prefix =
  let temp_dir = match dir with
    | None   ->
      let dir = !OpamGlobals.root_dir in
      if !check_remove_temp_dir && dir = OpamGlobals.root_dir_tmp then (
        OpamMisc.at_exit (fun () -> remove_dir OpamGlobals.root_dir_tmp);
        check_remove_temp_dir := false);
      dir / "log"
    | Some d -> d in
  mkdir temp_dir;
  let file = temp_dir / temp_basename prefix in
  if Hashtbl.mem temp_files file then
    temp_file ?dir prefix
  else (
    Hashtbl.add temp_files file true;
    file
  )

let with_tmp_dir fn =
  let dir = mk_temp_dir () in
  try
    mkdir dir;
    let e = fn dir in
    remove_dir dir;
    e
  with e ->
    remove_dir dir;
    raise e

let with_tmp_dir_job fjob =
  let dir = mk_temp_dir () in
  mkdir dir;
  OpamProcess.Job.finally (fun () -> remove_dir dir) (fjob dir)


(* XXX Deprecated: use ~dir argument of make_command instead *)
let chdir dir =
  try Unix.chdir dir
  with Unix.Unix_error _ -> raise (File_not_found dir)
let in_dir dir fn =
  let reset_cwd =
    let cwd =
      try Some (Sys.getcwd ())
      with Sys_error _ -> None in
    fun () ->
      match cwd with
      | None     -> ()
      | Some cwd -> try chdir cwd with File_not_found _ -> () in
  chdir dir;
  try
    let r = fn () in
    reset_cwd ();
    r
  with e ->
    reset_cwd ();
    raise e


(* - I/O operations - *)

let string_of_channel ic =
  let n = 32768 in
  let s = Bytes.create n in
  let b = Buffer.create 1024 in
  let rec iter ic b s =
    let nread =
      try input ic s 0 n
      with End_of_file -> 0 in
    if nread > 0 then (
      Buffer.add_subbytes b s 0 nread;
      iter ic b s
    ) in
  iter ic b s;
  Buffer.contents b

let read file =
  let ic =
    try open_in_bin file
    with Sys_error _ -> raise (File_not_found file) in
  let s = string_of_channel ic in
  close_in ic;
  s

let write file contents =
  mkdir (FP.dirname file);
  let oc =
    try open_out_bin file
    with Sys_error _ -> raise (File_not_found file) in
  output_string oc contents;
  close_out oc

(* - Environment variables - *)

let default_env =
  Unix.environment ()

let reset_env = lazy (
  let env = OpamMisc.env () in
  let env =
    let path_sep_str = String.make 1 path_sep in
    List.rev_map (fun (k,v as c) ->
      match k with
      | "PATH" ->
        k, String.concat path_sep_str
          (OpamMisc.reset_env_value path_sep ~prefix:!OpamGlobals.root_dir v)
      | _      -> c
    ) env in
  let env = List.rev_map (fun (k,v) -> k^"="^v) env in
  Array.of_list env
)

let env_var env var =
  let len = Array.length env in
  let prefix = var^"=" in
  let pfxlen = String.length prefix in
  let rec aux i =
    if i >= len then "" else
    let s = env.(i) in
    if OpamMisc.starts_with ~prefix s then
      String.sub s pfxlen (String.length s - pfxlen)
    else aux (i+1)
  in
  aux 0

(* - Command handling, processes and jobs - *)

let command_exists =
  let is_external_cmd name = Filename.basename name <> name in
  let check_existence ?dir env name =
    let cmd, args = "/bin/sh", ["-c"; Printf.sprintf "command -v %s" name] in
    let r =
      OpamProcess.run
        (OpamProcess.command ~env ?dir ~name:(temp_file "command") ~verbose:false
           cmd args)
    in
    OpamProcess.cleanup ~force:true r;
    if OpamProcess.is_success r then
      match r.OpamProcess.r_stdout with
      | cmdname::_ ->
        (* check that we have permission to execute the command *)
        if is_external_cmd cmdname then
          let cmdname =
            match Filename.is_relative cmdname, dir with
            | true, Some dir -> Filename.concat dir cmdname
            | _ -> cmdname
          in
          (try

             let open Unix in
             let uid = getuid() and groups = Array.to_list(getgroups()) in
             let s = stat cmdname in
             let cmd_uid = s.st_uid and cmd_gid = s.st_gid and cmd_perms = s.st_perm in
             let mask = 0o001
                        lor (if uid = cmd_uid then 0o100 else 0)
                        lor (if List.mem cmd_gid groups then 0o010 else 0) in
             (cmd_perms land mask) <> 0
           with _ -> false)
        else true
      | _ -> false
    else false
  in
  let cached_results = Hashtbl.create 17 in
  fun ?(env=default_env) ?dir name ->
    if dir <> None && is_external_cmd name then
      check_existence env ?dir name (* relative command, no caching *)
    else
    let path = env_var env "PATH" in
    try Hashtbl.find (Hashtbl.find cached_results path) name
    with Not_found ->
      let r = check_existence env name in
      (try Hashtbl.add (Hashtbl.find cached_results path) name r
       with Not_found ->
         let phash = Hashtbl.create 17 in
         Hashtbl.add phash name r;
         Hashtbl.add cached_results path phash);
      r

let runs = ref []
let print_stats () =
  match !runs with
  | [] -> ()
  | l  ->
    OpamGlobals.msg "%d external processes called:\n%s%!"
      (List.length l) (OpamMisc.itemize ~bullet:"  " (String.concat " ") l)

let log_file ?dir name = match name with
  | None   -> temp_file "log"
  | Some n ->
    let dir =
      OpamMisc.Option.default (Filename.concat !OpamGlobals.root_dir "log") dir
    in
    temp_file ~dir n

let make_command
    ?verbose ?(env=default_env) ?name ?text ?metadata ?allow_stdin ?dir ?(check_existence=true)
    cmd args =
  let name = log_file ?dir name in
  let verbose =
    OpamMisc.Option.default (!OpamGlobals.verbose_level >= 2) verbose
  in
  (* Check that the command doesn't contain whitespaces *)
  if None <> try Some (String.index cmd ' ') with Not_found -> None then
    OpamGlobals.warning "Command %S contains 1 space" cmd;
  if not check_existence || command_exists ~env ?dir cmd then
    OpamProcess.command ~env ~name ?text ~verbose ?metadata ?allow_stdin ?dir
      cmd args
  else
    command_not_found cmd

let verbose_for_base_commands () =
  !OpamGlobals.verbose_level >= 3

let sys_command ?(verbose=verbose_for_base_commands()) ?dir
    cmd args =
  OpamProcess.Job.run
    (make_command ~verbose ?dir cmd args
     @@> fun r -> raise_on_process_error r; Done ())

let read_command_output ?verbose ?env ?metadata ?allow_stdin ?dir cmd args =
  let c = make_command ?verbose ?env ?metadata ?allow_stdin ?dir cmd args in
  OpamProcess.Job.(
    run (c @@> fun r ->
         raise_on_process_error r;
         Done r.OpamProcess.r_stdout)
  )

(* Return [None] if the command does not exist *)
let read_command_output_opt ?verbose ?env cmd args =
  try Some (read_command_output ?verbose ?env cmd args)
  with Command_not_found _ -> None

module Tar = struct

  let extensions =
    [ [ "tar.gz" ; "tgz" ], 'z'
    ; [ "tar.bz2" ; "tbz" ], 'j'
    ; [ "tar.xz" ; "txz" ], 'J'
    ; [ "tar.lzma" ; "tlz" ], 'Y'
    ]

  let guess_type f =
    let ic = open_in f in
    let c1 = input_char ic in
    let c2 = input_char ic in
    close_in ic;
    match c1, c2 with
    | '\031', '\139' -> Some 'z'
    | 'B'   , 'Z'    -> Some 'j'
    | '\xfd', '\x37' -> Some 'J'
    | '\x5d', '\x00' -> Some 'Y'
    | _              -> None

  let match_ext file ext =
    List.exists (Filename.check_suffix file) ext

  let is_archive f =
    List.exists
      (fun suff -> Filename.check_suffix f suff)
      (List.concat (List.rev_map fst extensions))

  let extract_function file =
    let command c dir =
      sys_command "tar" [ Printf.sprintf "xf%c" c ; file; "-C" ; dir ] in

    let ext =
      List.fold_left
        (fun acc (ext, c) -> match acc with
          | Some f -> Some f
          | None   ->
            if match_ext file ext
            then Some (command c)
            else None)
        None
        extensions in
    match ext with
    | Some f -> Some f
    | None   ->
      match guess_type file with
      | None   -> None
      | Some c -> Some (command c)

end

module Zip = struct
  let is_archive f = Filename.check_suffix f "zip"

  let extract_function file =
    Some (fun dir -> sys_command "unzip" [file; "-d"; dir ])
end

let is_tar_archive = Tar.is_archive

let extract file dst =
  let kind, extract_function =
    if Zip.is_archive file then "zip", Zip.extract_function
    else "tar", Tar.extract_function
  in
  with_tmp_dir (fun tmp_dir ->
    match extract_function file with
    | None   -> internal_error "%s is not a valid %s archive." file kind
    | Some f ->
      f tmp_dir;
      if Sys.file_exists dst then
        internal_error "Extracting the archive will overwrite %s." dst;
      match list (F.And (F.Is_dir, F.Not F.Is_link)) tmp_dir with
      | [x] ->
        mkdir (Filename.dirname dst);
        sys_command "mv" [x; dst]
      | _   ->
        internal_error "The archive %S contains multiple root directories."
          file
  )

let extract_in file dst =
  if not (Sys.file_exists dst) then
    internal_error "%s does not exist." file;
  match Tar.extract_function file with
  | None   -> internal_error "%s is not a valid tar archive." file
  | Some f -> f dst

type lock = Unix.file_descr * string

let flock ?(read=false) file =
  let max_tries =
    if !OpamGlobals.safe_mode then 1 else !OpamGlobals.lock_retries in
  if not read && !OpamGlobals.safe_mode then
    OpamGlobals.error_and_exit "Write lock attempt in safe mode";
  let open_flags, lock_op =
    if read then [Unix.O_RDONLY], Unix.F_TRLOCK
    else [Unix.O_RDWR], Unix.F_TLOCK
  in
  let fd =
    Unix.openfile file (Unix.O_CREAT::open_flags) 0o666 in
  let rec loop attempt =
    try Unix.lockf fd lock_op 0
    with Unix.Unix_error (Unix.EAGAIN,_,_) ->
      if max_tries > 0 && attempt > max_tries then
        OpamGlobals.error_and_exit
          "Timeout trying to acquire %s lock to %S, \
           is another opam process running ?"
          (if read then "read" else "write") file;
      log
        "Failed to %s-lock %S. (attempt %d/%d)"
        (if read then "read" else "write")
        file attempt max_tries;
      Unix.sleep 1;
      loop (attempt + 1)
  in
  log "locking %s" file;
  loop 1;
  fd, file

let funlock (fd,file) =
  (try (* Unlink file if write lock can be acquired *)
     Unix.lockf fd Unix.F_TLOCK 0;
     Unix.unlink file;
   with Unix.Unix_error _ -> ());
  Unix.close fd; (* implies Unix.lockf fd Unix.F_ULOCK 0 *)
  log "Lock released on %s" file

let ocaml_version = lazy (
  match read_command_output_opt ~verbose:false "ocamlc" ["-version"] with
  | Some (h::_) ->
    let version = OpamMisc.strip h in
    log "ocamlc version: %s" version;
    Some version
  | Some [] -> internal_error "`ocamlc -version` is empty."
  | None    -> None
)

let exists_alongside_ocamlc name =
  try F.test F.Exists (FP.dirname (F.which "ocamlc") / name)
  with Not_found -> false

let ocaml_opt_available = lazy (exists_alongside_ocamlc "ocamlc.opt")
let ocaml_native_available = lazy (exists_alongside_ocamlc "ocamlopt")
let ocaml_natdynlink_available = lazy (
  match
    read_command_output_opt ~verbose:false "ocamlc" ["-where"]
  with
  | Some (h::_) ->
    let libdir = OpamMisc.strip h in
    Sys.file_exists (libdir / "dynlink.cmxa")
  | None | Some []   -> false
)

(* Reset the path to get the system compiler *)
let system command args = lazy (
  let env = Lazy.force reset_env in
  match read_command_output_opt ~verbose:false ~env command args with
  | Some (h::_) -> Some (OpamMisc.strip h)
  | _ -> None
)

let system_ocamlc_where = system "ocamlc" ["-where"]

let system_ocamlc_version = system "ocamlc" ["-version"]

let choose_download_tool () =
  match !OpamGlobals.download_tool with
  | Some t -> t
  | None ->
    if OpamGlobals.os () = OpamGlobals.Darwin && command_exists "wget"
    then `Wget
    else if command_exists OpamGlobals.curl_command then `Curl
    else if command_exists "wget" then `Wget
    else
      OpamGlobals.error_and_exit
        "Could not find a suitable download command. Please make sure you have \
         either \"curl\" or \"wget\" installed, or specify a custom command \
         through variable OPAMFETCH."

let download_command =
  let retry = string_of_int OpamGlobals.download_retry in
  let wget ~compress:_ dir src =
    let wget_args = [
      "--content-disposition"; "--no-check-certificate";
      "-t"; retry;
      src
    ] in
    make_command ~dir "wget" wget_args @@> fun r ->
    raise_on_process_error r;
    Done ()
  in
  let curl command ~compress dir src =
    let curl_args = [
      "--write-out"; "%{http_code}\\n"; "--insecure";
      "--retry"; retry; "--retry-delay"; "2";
    ] @ (if compress then ["--compressed"] else []) @ [
        "-OL"; src
    ] in
    make_command ~dir command curl_args @@> fun r ->
    raise_on_process_error r;
    match r.OpamProcess.r_stdout with
    | [] -> internal_error "curl: empty response while downloading %s" src
    | l  ->
      let code = List.hd (List.rev l) in
      try if int_of_string code >= 400 then raise Exit else Done ()
      with e ->
        OpamMisc.fatal e;
        internal_error "curl: code %s while downloading %s" code src
  in
  let custom dl_cmd ~compress dir src =
    let dst = Filename.basename src in
    match dl_cmd ~url:src ~out:dst ~retry:OpamGlobals.download_retry ~compress
    with
    | cmd::args ->
      make_command ~dir cmd args @@> fun r ->
      raise_on_process_error r;
      Done ()
    | [] -> internal_error "Empty custom download command"
  in
  lazy (
    match choose_download_tool () with
    | `Wget -> wget
    | `Curl -> curl OpamGlobals.curl_command
    | `Custom cust -> custom cust
  )

let really_download ~overwrite ?(compress=false) ~src ~dst =
  let download = (Lazy.force download_command) in
  let aux dir =
    download ~compress dir src @@+ fun () ->
    match list F.True dir with
    | [] ->
      internal_error "Downloaded file not found."
    | _::_::_ ->
      internal_error "Too many downloaded files."
    | [filename] ->
      if Sys.file_exists dst then
        if overwrite then remove dst
        else internal_error "The downloaded file will overwrite %s." dst;
      OpamProcess.command ~dir ~verbose:(verbose_for_base_commands ())
        "mv" [filename; dst ]
      @@> fun r -> raise_on_process_error r; Done dst
  in
  OpamProcess.Job.catch
    (function
      | Internal_error s as e -> OpamGlobals.error "%s" s; raise e
      | e ->
        OpamMisc.fatal e;
        log "Could not download file at %s." src;
        raise e)
    (with_tmp_dir_job aux)

let download ~overwrite ?compress ~filename:src ~dst:dst =
  if dst = src then
    Done dst
  else if Sys.file_exists src then (
    if Sys.file_exists dst then
      if overwrite then remove dst
      else internal_error "The downloaded file will overwrite %s." dst;
    OpamProcess.command ~verbose:(verbose_for_base_commands ()) "cp" [src; dst]
    @@> fun r -> raise_on_process_error r; Done dst
  ) else
    really_download ~overwrite ?compress ~src ~dst

let patch ~dir p =
  let max_trying = 5 in
  if not (Sys.file_exists p) then
    (OpamGlobals.error "Patch file %S not found." p;
     raise Not_found);
  let patch ~dryrun n =
    let opts = if dryrun then
        let open OpamGlobals in
        match OpamGlobals.os () with
        | FreeBSD | OpenBSD | NetBSD | DragonFly -> [ "-t"; "-C" ]
        | Unix | Linux | Darwin -> [ "--dry-run" ]
        | Win32 | Cygwin (* this is probably broken *)
        | Other _               -> [ "--dry-run" ]
      else [] in
    let verbose = if dryrun then Some false else None in
    sys_command ?verbose ~dir
      "patch" (("-p" ^ string_of_int n) :: "-i" :: p :: opts)
  in
  let rec aux n =
    if n = max_trying then
      internal_error "Patch %s does not apply." p
    else if None =
            try Some (patch ~dryrun:true n)
            with e -> OpamMisc.fatal e; None then
      aux (succ n)
    else
      patch ~dryrun:false n in
  aux 0

let () =
  let with_opam_info m =
    let git_version = match OpamVersion.git () with
      | None   -> ""
      | Some v -> Printf.sprintf " (%s)" (OpamVersion.to_string v) in
    let opam_version =
      Printf.sprintf "%s%s" (OpamVersion.to_string OpamVersion.current) git_version in
    let os = OpamGlobals.os_string () in
    Printf.sprintf
      "# %-15s %s\n\
       # %-15s %s\n\
       %s"
      "opam-version" opam_version
      "os" os
      m in
  Printexc.register_printer (function
    | Process_error r     -> Some (OpamProcess.string_of_result r)
    | Internal_error m    -> Some (with_opam_info m)
    | Command_not_found c -> Some (Printf.sprintf "%S: command not found." c)
    | Sys.Break           -> Some "User interruption"
    | Unix.Unix_error (e, fn, msg) ->
      let msg = if msg = "" then "" else " on " ^ msg in
      let error = Printf.sprintf "%s: %S failed%s: %s"
          Sys.argv.(0) fn msg (Unix.error_message e) in
      Some (with_opam_info error)
    | _ -> None
  )
