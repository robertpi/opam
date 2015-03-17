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

module Base = OpamMisc.Base
open OpamProcess.Job.Op

let log fmt = OpamGlobals.log "FILENAME" fmt
let slog = OpamGlobals.slog

module Dir = struct

  include OpamMisc.Base

  let of_string dirname =
    let dirname =
      if dirname = "~" then OpamGlobals.home
      else if OpamMisc.starts_with ~prefix:("~"^Filename.dir_sep) dirname then
        Filename.concat OpamGlobals.home
          (OpamMisc.remove_prefix ~prefix:("~"^Filename.dir_sep) dirname)
      else dirname
    in
    OpamSystem.real_path dirname

  let to_string dirname = dirname

end

let raw_dir s = s

module FP = FilePath
module F = FileUtil

let (/) s1 s2 = FP.concat s1 s2

type t = FP.filename

let dirname t = FP.dirname t

let basename t = FP.basename t

let create dirname basename = FP.make_absolute (Dir.of_string dirname) basename

let of_basename basename = FP.reduce (F.pwd () / basename)

let raw str = str

let to_string t = t

let of_string s = OpamSystem.real_path s

let patch filename dirname =
  OpamSystem.patch ~dir:(Dir.to_string dirname) (to_string filename)

(* Almost pure wrappers over OpamSystem *)

let cleandir dirname =
  log "cleandir %a" (slog Dir.to_string) dirname;
  if F.test F.Is_dir dirname then
    List.iter OpamSystem.remove (F.ls (Dir.to_string dirname))
  else
    OpamSystem.mkdir dirname

let exists_dir dirname = F.test F.Is_dir dirname

let copy_dir ~src ~dst =
  if exists_dir dst then
    OpamSystem.internal_error
      "Cannot create %s as the directory already exists." (Dir.to_string dst);
  List.iter (fun dir ->
      OpamSystem.mkdir (FP.reparent src dst dir);
      let files = OpamSystem.files dir in
      let links,files = List.partition (F.test F.Is_link) files in
      F.cp files (FP.reparent src dst dir);
      List.iter (fun f ->
          (* F.readlink makes absolute, we want the raw link *)
          Unix.symlink (Unix.readlink f) (FP.reparent src dst f)
        ) links
    ) (src::OpamSystem.rec_dirs src)

let link_dir ~src ~dst =
  if exists_dir dst then
    OpamSystem.internal_error "Cannot link: %s already exists." (Dir.to_string dst)
  else (
    OpamSystem.mkdir (Filename.dirname dst);
    OpamSystem.link (Dir.to_string src) (Dir.to_string dst)
  )

let exists filename =
  try (Unix.stat (to_string filename)).Unix.st_kind = Unix.S_REG
  with Unix.Unix_error _ -> false

let readlink src =
  if exists src then
    try of_string (Unix.readlink (to_string src))
    with Unix.Unix_error _ -> src
  else
    OpamSystem.internal_error "%s does not exist." (to_string src)

let download ~overwrite ?compress url dirname =
  OpamSystem.mkdir dirname;
  let dst = to_string (create dirname (OpamMisc.url_basename url)) in
  OpamSystem.download ~overwrite ?compress ~url ~dst
  @@+ fun file -> Done (of_string file)

let download_as ~overwrite ?(compress=false) url dest =
  OpamSystem.mkdir (dirname dest);
  OpamSystem.download ~overwrite ~compress ~url ~dst:(to_string dest)
  @@+ fun file ->
  assert (file = to_string dest);
  Done ()




let cwd () =
  Dir.of_string (Unix.getcwd ())


let env_of_list l = Array.of_list (List.rev_map (fun (k,v) -> k^"="^v) l)


let basename_dir dirname =
  Base.of_string (Filename.basename (Dir.to_string dirname))

let dirname_dir dirname =
  Dir.to_string (Filename.dirname (Dir.of_string dirname))

let to_list_dir dir =
  let base d = Dir.of_string (Filename.basename (Dir.to_string d)) in
  let rec aux acc dir =
    let d = dirname_dir dir in
    if d <> dir then aux (base dir :: acc) d
    else base dir :: acc in
  aux [] dir

let digest t =
  Digest.to_hex (Digest.file (to_string t))

let touch t =
  OpamSystem.write (to_string t) ""

let chmod t p =
  Unix.chmod (to_string t) p

let open_in filename =
  try open_in (to_string filename)
  with Sys_error _ -> raise (OpamSystem.File_not_found (to_string filename))

let open_out filename =
  try open_out (to_string filename)
  with Sys_error _ -> raise (OpamSystem.File_not_found (to_string filename))


let with_contents fn filename =
  fn (OpamSystem.read filename)

let check_suffix filename s =
  Filename.check_suffix (to_string filename) s

let add_extension filename suffix =
  of_string ((to_string filename) ^ "." ^ suffix)

let chop_extension filename =
  of_string (Filename.chop_extension (to_string filename))

let is_symlink src =
  try
    let s = Unix.lstat (to_string src) in
    s.Unix.st_kind = Unix.S_LNK
  with Unix.Unix_error _ ->
    OpamSystem.internal_error "%s does not exist." (to_string src)

let is_exec file =
  try OpamSystem.is_exec (to_string file)
  with Unix.Unix_error _ ->
    OpamSystem.internal_error "%s does not exist." (to_string file)

let starts_with dirname filename =
  OpamMisc.starts_with ~prefix:(Dir.to_string dirname) (to_string filename)

let remove_prefix prefix filename =
  FP.make_relative prefix filename

let copy_in ?root src dstdir =
  let dstfile = match root with
    | Some r -> FP.reparent r dstdir src
    | None -> dstdir / FP.basename src
  in
  OpamSystem.copy src dstfile

type generic_file =
  | D of Dir.t
  | F of t

let extract_generic_file filename dirname =
  match filename with
  | F f ->
    log "extracting %a to %a"
      (slog to_string) f
      (slog Dir.to_string) dirname;
    OpamSystem.extract f dirname
  | D d ->
    if d <> dirname then (
      log "copying %a to %a"
        (slog Dir.to_string) d
        (slog Dir.to_string) dirname;
      copy_dir ~src:d ~dst:dirname
    )


let ends_with suffix filename =
  OpamMisc.ends_with ~suffix (to_string filename)

let remove_suffix suffix filename =
  let suffix = Base.to_string suffix in
  let filename = to_string filename in
  OpamMisc.remove_suffix ~suffix filename


let with_flock ?read file f x =
  let lock = OpamSystem.flock ?read (to_string file) in
  try
    let r = f x in
    OpamSystem.funlock lock;
    r
  with e ->
    OpamMisc.register_backtrace e;
    OpamSystem.funlock lock;
    raise e

let checksum f =
  if exists f then
    [digest f]
  else
    []

let checksum_dir d =
  if exists_dir d then
    List.map digest (OpamSystem.rec_files d)
  else
    []

let prettify_dir d =
  OpamMisc.prettify_path (Dir.to_string d)

let prettify s =
  OpamMisc.prettify_path (to_string s)

let to_json x = `String (to_string x)

module O = struct
  type tmp = t
  type t = tmp
  let compare = compare
  let to_string = to_string
  let to_json = to_json
end

module Map = OpamMisc.Map.Make(O)
module Set = OpamMisc.Set.Make(O)

let copy_files ~src ~dst =
  let files = OpamSystem.rec_files src in
  List.iter (fun file ->
      if not !OpamGlobals.do_not_copy_files then
        let base = remove_prefix src file in
        let dst_file = create dst (Base.of_string base) in
        if !OpamGlobals.verbose_level >= 2 then
        OpamGlobals.msg "Copying %s %s %s/\n"
          (prettify file)
          (if exists dst_file then "over" else "to")
          (prettify_dir dst);
        OpamSystem.copy file dst_file
    ) files

module OP = struct

  let (/) = (/)

  let (//) d1 s2 = FP.reduce (d1 / s2)
end

module Attribute = struct

  type t = {
    base: Base.t;
    md5 : string;
    perm: int option;
  }

  let base t = t.base

  let md5 t = t.md5

  let perm t = t.perm

  let create base md5 perm =
    { base; md5; perm=Some perm }

  let to_string_list t =
    let perm = match t.perm with
      | None   -> []
      | Some p -> [Printf.sprintf "0o%o" p] in
    Base.to_string t.base :: t.md5 :: perm

  let of_string_list = function
    | [base; md5]      -> { base=Base.of_string base; md5; perm=None }
    | [base;md5; perm] -> { base=Base.of_string base; md5;
                            perm=Some (int_of_string perm) }
    | k                -> OpamSystem.internal_error
                            "remote_file: '%s' is not a valid line."
                            (String.concat " " k)

  let to_string t = String.concat " " (to_string_list t)
  let of_string s = of_string_list (OpamMisc.split s ' ')

  let to_json x =
    `O ([ ("base" , Base.to_json x.base);
          ("md5"  , `String x.md5)]
        @ match x. perm with
          | None   -> []
          | Some p -> ["perm", `String (string_of_int p)])

  module O = struct
    type tmp = t
    type t = tmp
    let to_string = to_string
    let compare = compare
    let to_json = to_json
  end

  module Set = OpamMisc.Set.Make(O)

  module Map = OpamMisc.Map.Make(O)

end

let to_attribute root file =
  let basename = Base.of_string (remove_prefix root file) in
  let perm =
    let s = Unix.stat (to_string file) in
    s.Unix.st_perm in
  let digest = digest file in
  Attribute.create basename digest perm
