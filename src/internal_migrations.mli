(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Private package-owned schema migration runner. *)

val current_package_version : int

val read_version : (module Caqti_eio.CONNECTION) -> (int, string) result

val apply_steps :
  (module Caqti_eio.CONNECTION) ->
  from_version:int ->
  target_version:int ->
  (unit, string) result

val ensure_current_schema :
  (module Caqti_eio.CONNECTION) -> (unit, string) result
