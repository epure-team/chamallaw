(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

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

let project_id = function
  | Global _ | Organization _ -> None
  | Project scope -> Some scope.project_id

let org_id = function
  | Global _ -> None
  | Organization scope -> Some scope.org_id
  | Project scope -> scope.org_id
