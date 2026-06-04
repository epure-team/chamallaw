(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Package-private normative metadata store for laws. *)

type force = Law_normative_metadata_kinds.force =
  | Obligation
  | Prohibition
  | Permission
  | Recommendation
  | Exception

type modality = Law_normative_metadata_kinds.modality =
  | Strict
  | Lenient
  | Conditional

type strength = Law_normative_metadata_kinds.strength = Hard | Soft

type severity = Law_normative_metadata_kinds.severity =
  | Critical
  | High
  | Medium
  | Low
  | Informational

type authority = Law_normative_metadata_kinds.authority =
  | Mandatory
  | Advisory
  | Internal
  | External

type role_kind = Law_normative_metadata_kinds.role_kind = Primary | Secondary

type metadata_row = {
  id : int;
  law_id : int;
  role_kind : role_kind;
  force : force;
  modality : modality;
  strength : strength;
  severity : severity;
  authority : authority;
  is_active : bool;
  scope : Authorized_scope.t;
  created_at : Ptime.t;
  updated_at : Ptime.t;
}

val create_metadata :
  (module Caqti_eio.CONNECTION) ->
  scope:Authorized_scope.t ->
  ?target_scope:Authorized_scope.t ->
  law_id:int ->
  role_kind:role_kind ->
  force:force ->
  modality:modality ->
  strength:strength ->
  severity:severity ->
  authority:authority ->
  unit ->
  (metadata_row, string) result

val list_for_law :
  (module Caqti_eio.CONNECTION) ->
  scope:Authorized_scope.t ->
  law_id:int ->
  (metadata_row list, string) result

val set_active :
  (module Caqti_eio.CONNECTION) ->
  scope:Authorized_scope.t ->
  ?target_scope:Authorized_scope.t ->
  metadata_id:int ->
  is_active:bool ->
  unit ->
  (unit, string) result

val replace_for_law :
  (module Caqti_eio.CONNECTION) ->
  scope:Authorized_scope.t ->
  ?target_scope:Authorized_scope.t ->
  law_id:int ->
  rows:metadata_row list ->
  unit ->
  (metadata_row list, string) result
