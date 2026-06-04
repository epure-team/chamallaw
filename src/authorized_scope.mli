(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Typed authorized scope passed from the host into standalone law-package
    operations.

    The package never infers authorization from process-global state, request
    locals, token files, or unchecked numeric IDs. *)

type global_scope = {actor_id : string option}

type organization_scope = {org_id : int; actor_id : string option}

type project_scope = {
  project_id : int;
  org_id : int option;
  actor_id : string option;
}

type t =
  | Global of global_scope
  | Organization of organization_scope
  | Project of project_scope

val project_id : t -> int option

val org_id : t -> int option
