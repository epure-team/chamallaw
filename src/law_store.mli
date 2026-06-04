(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Package-private canonical law store over the package-owned [laws] table. *)

type relation_kind = Law_relation_kind.relation_kind =
  | Replaces
  | Refines
  | Overrides
  | Exempts

type law_row = {
  id : int;
  statement : string;
  rationale : string option;
  scope : Authorized_scope.t;
  owner_user_id : int option;
  replaces_law_id : int option;
  relation_kind : relation_kind option;
  provenance_note : string option;
  is_archived : bool;
  created_at : Ptime.t;
  updated_at : Ptime.t;
}

val create_law :
  (module Caqti_eio.CONNECTION) ->
  scope:Authorized_scope.t ->
  ?target_scope:Authorized_scope.t ->
  statement:string ->
  ?rationale:string ->
  ?owner_user_id:int ->
  ?replaces:int ->
  ?relation_kind:relation_kind ->
  ?provenance_note:string ->
  unit ->
  (law_row, string) result

val get_law :
  (module Caqti_eio.CONNECTION) ->
  scope:Authorized_scope.t ->
  law_id:int ->
  (law_row option, string) result

val list_visible :
  (module Caqti_eio.CONNECTION) ->
  scope:Authorized_scope.t ->
  (law_row list, string) result

val update_relation :
  (module Caqti_eio.CONNECTION) ->
  scope:Authorized_scope.t ->
  ?target_scope:Authorized_scope.t ->
  law_id:int ->
  replaces:int option ->
  relation_kind:relation_kind option ->
  unit ->
  (law_row, string) result

val archive_law :
  (module Caqti_eio.CONNECTION) ->
  scope:Authorized_scope.t ->
  ?target_scope:Authorized_scope.t ->
  law_id:int ->
  unit ->
  (unit, string) result
