(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
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

(** Initialises download tool and parameters *)
val init_config: unit OpamRepositoryConfig.options_fun

(** downloads a file from an URL, using Curl, Wget, or a custom configured
    tool, to the given directory. Returns the downloaded filename.
    FIXME: The source OpamFilename.t is indeed a URL. *)
val download:
  overwrite:bool -> ?compress:bool -> OpamFilename.t -> OpamFilename.Dir.t ->
  OpamFilename.t OpamProcess.job

(** As [download], but with a specified output filename. *)
val download_as:
  overwrite:bool -> ?compress:bool -> OpamFilename.t -> OpamFilename.t ->
  unit OpamProcess.job
