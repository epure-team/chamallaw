(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Package-private law-to-concept link store. *)

type link_role = Law_concept_link_role.link_role =
  | Primary_subject
  | Applicability_context
  | Concern
  | Artifact_scope
  | Phase_scope
  | Agent_scope
  | Suggestion_only

type link_row = {
  id : int;
  law_id : int;
  concept_id : int;
  role : link_role;
  is_active : bool;
  scope : Authorized_scope.t;
  created_at : Ptime.t;
  updated_at : Ptime.t;
}

val create_link :
  (module Caqti_eio.CONNECTION) ->
  scope:Authorized_scope.t ->
  ?target_scope:Authorized_scope.t ->
  law_id:int ->
  concept_id:int ->
  role:link_role ->
  unit ->
  (link_row, string) result

val list_for_law :
  (module Caqti_eio.CONNECTION) ->
  scope:Authorized_scope.t ->
  law_id:int ->
  (link_row list, string) result

val list_for_concept :
  (module Caqti_eio.CONNECTION) ->
  scope:Authorized_scope.t ->
  concept_id:int ->
  (link_row list, string) result

val deactivate_link :
  (module Caqti_eio.CONNECTION) ->
  scope:Authorized_scope.t ->
  ?target_scope:Authorized_scope.t ->
  link_id:int ->
  unit ->
  (unit, string) result
