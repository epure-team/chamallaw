(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

let denied = Error "scope authorization denied"

let same_org actor_org target_org = actor_org = target_org

let authorize ~actor ~target =
  match (actor, target) with
  | Authorized_scope.Global _, _ -> Ok ()
  | ( Authorized_scope.Organization {org_id; _},
      Authorized_scope.Organization target )
    when org_id = target.org_id ->
      Ok ()
  | Authorized_scope.Organization {org_id; _}, Authorized_scope.Project target
    when target.org_id = Some org_id ->
      Ok ()
  | ( Authorized_scope.Project {project_id; org_id; _},
      Authorized_scope.Project
        {project_id = target_project_id; org_id = target_org_id; _} )
    when project_id = target_project_id && same_org org_id target_org_id ->
      Ok ()
  | _ -> denied
