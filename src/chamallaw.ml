(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

module Authorized_scope = Authorized_scope
module Normalized_work_context = Normalized_work_context
module Host_adapter = Host_adapter
module V = Vocabulary_store
module LS = Law_store
module LNM = Law_normative_metadata_store
module LCL = Law_concept_links_store

let ( let* ) = Result.bind

type ctx = {conn : (module Caqti_eio.CONNECTION)}

type provenance = V.Provenance_codec.t = Epure_builtin | User

type init_result =
  | Ready of ctx
  | Requires_migration of {
      from_version : int;
      to_version : int;
      apply : unit -> (ctx, string) result;
    }

let make_ctx conn = {conn}

let init conn =
  let* from_version = Internal_migrations.read_version conn in
  let to_version = Internal_migrations.current_package_version in
  if from_version = to_version then
    let* () = Internal_migrations.ensure_current_schema conn in
    Ok (Ready (make_ctx conn))
  else if from_version < to_version then
    Ok
      (Requires_migration
         {
           from_version;
           to_version;
           apply =
             (fun () ->
               Internal_migrations.apply_steps
                 conn
                 ~from_version
                 ~target_version:to_version
               |> Result.map (fun () -> make_ctx conn));
         })
  else
    Error
      (Printf.sprintf
         "chamallaw package schema version %d is newer than this binary's \
          supported version %d"
         from_version
         to_version)

module Package_metadata_store = struct
  type row = Package_metadata_store.row = {
    package_name : string;
    api_version : int;
    created_at : string;
    updated_at : string;
  }

  let get ctx = Package_metadata_store.get ctx.conn
end

module Concept_scheme_store = struct
  type scheme_row = V.Concept_scheme_store.scheme_row = {
    id : int;
    slug : string;
    display_name : string;
    description : string option;
    provenance : V.Provenance_codec.t;
    scope : Authorized_scope.t;
    created_at : Ptime.t;
  }

  let create ~ctx = V.Concept_scheme_store.create ctx.conn

  let list_visible ~ctx = V.Concept_scheme_store.list_visible ctx.conn

  let get_by_slug ~ctx = V.Concept_scheme_store.get_by_slug ctx.conn
end

module Concept_store = struct
  type concept_row = V.Concept_store.concept_row = {
    id : int;
    scheme_id : int;
    slug : string;
    definition : string option;
    scope_note : string option;
    provenance : V.Provenance_codec.t;
    scope : Authorized_scope.t;
    created_at : Ptime.t;
  }

  let create ~ctx = V.Concept_store.create ctx.conn

  let list_visible_by_scheme ~ctx =
    V.Concept_store.list_visible_by_scheme ctx.conn

  let get_by_scheme_and_slug ~ctx =
    V.Concept_store.get_by_scheme_and_slug ctx.conn
end

module Concept_label_store = struct
  type label_kind = V.Concept_label_store.label_kind =
    | Label_preferred
    | Label_alternate
    | Label_hidden
    | Label_deprecated

  type staleness_status = V.Concept_label_store.staleness_status =
    | Active
    | Deprecated
    | Stale

  type label_row = V.Concept_label_store.label_row = {
    id : int;
    concept_id : int;
    text : string;
    kind : label_kind;
    staleness_status : staleness_status;
    last_marked_at : Ptime.t option;
    created_at : Ptime.t;
  }

  let create ~ctx = V.Concept_label_store.create ctx.conn

  let list_by_concept ~ctx = V.Concept_label_store.list_by_concept ctx.conn

  let mark_stale ~ctx = V.Concept_label_store.mark_stale ctx.conn
end

module Concept_relation_store = struct
  type relation_type_row = V.Concept_relation_store.relation_type_row = {
    id : int;
    slug : string;
    is_hierarchical : bool;
    is_traversal_enabled : bool;
  }

  type relation_row = V.Concept_relation_store.relation_row = {
    id : int;
    from_concept_id : int;
    to_concept_id : int;
    relation_type_id : int;
    is_active : bool;
    created_at : Ptime.t;
  }

  let list_relation_types ~ctx =
    V.Concept_relation_store.list_relation_types ctx.conn

  let create_relation ~ctx = V.Concept_relation_store.create_relation ctx.conn

  let list_relations_by_concept ~ctx =
    V.Concept_relation_store.list_relations_by_concept ctx.conn

  let list_incoming_relations ~ctx =
    V.Concept_relation_store.list_incoming_relations ctx.conn
end

module Concept_search_store = struct
  type search_result = V.Concept_search_store.search_result = {
    concept_id : int;
    rank : float;
  }

  let search ~ctx = V.Concept_search_store.search ctx.conn
end

module Law_store = struct
  type relation_kind = LS.relation_kind =
    | Replaces
    | Refines
    | Overrides
    | Exempts

  type law_row = LS.law_row = {
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

  let create_law ~ctx = LS.create_law ctx.conn

  let get_law ~ctx = LS.get_law ctx.conn

  let list_visible ~ctx = LS.list_visible ctx.conn

  let update_relation ~ctx = LS.update_relation ctx.conn

  let archive_law ~ctx = LS.archive_law ctx.conn
end

module Law_normative_metadata_store = struct
  type force = LNM.force =
    | Obligation
    | Prohibition
    | Permission
    | Recommendation
    | Exception

  type modality = LNM.modality = Strict | Lenient | Conditional

  type strength = LNM.strength = Hard | Soft

  type severity = LNM.severity =
    | Critical
    | High
    | Medium
    | Low
    | Informational

  type authority = LNM.authority = Mandatory | Advisory | Internal | External

  type role_kind = LNM.role_kind = Primary | Secondary

  type metadata_row = LNM.metadata_row = {
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

  let create_metadata ~ctx = LNM.create_metadata ctx.conn

  let list_for_law ~ctx = LNM.list_for_law ctx.conn

  let set_active ~ctx = LNM.set_active ctx.conn

  let replace_for_law ~ctx = LNM.replace_for_law ctx.conn
end

module Law_concept_links_store = struct
  type link_role = LCL.link_role =
    | Primary_subject
    | Applicability_context
    | Concern
    | Artifact_scope
    | Phase_scope
    | Agent_scope
    | Suggestion_only

  type link_row = LCL.link_row = {
    id : int;
    law_id : int;
    concept_id : int;
    role : link_role;
    is_active : bool;
    scope : Authorized_scope.t;
    created_at : Ptime.t;
    updated_at : Ptime.t;
  }

  let create_link ~ctx = LCL.create_link ctx.conn

  let list_for_law ~ctx = LCL.list_for_law ctx.conn

  let list_for_concept ~ctx = LCL.list_for_concept ctx.conn

  let deactivate_link ~ctx = LCL.deactivate_link ctx.conn
end

module Law_resolver = struct
  type applicability_reason = Law_resolver.applicability_reason =
    | Scope_match of Authorized_scope.t
    | Link_match of {
        link_role : LCL.link_role;
        concept_id : int;
        concept_slug : string;
        matched_field : string;
      }

  type applicable_law = Law_resolver.applicable_law = {
    law : LS.law_row;
    effective_force : LNM.force;
    effective_authority : LNM.authority;
    effective_severity : LNM.severity;
    reasons : applicability_reason list;
    overridden_by : int list;
  }

  type resolution = Law_resolver.resolution = {
    applicable : applicable_law list;
    unknown : (LS.law_row * string) list;
    exempted : (LS.law_row * int) list;
  }

  let resolve ~ctx ~scope ~work_context =
    Law_resolver.resolve ctx.conn ~scope ~work_context
end

module Seed = struct
  type t = V.Builtin_vocabulary_seed.seed

  let of_json_string = V.Builtin_vocabulary_seed.parse_seed_json
end

module Builtin_vocabulary_seed = struct
  let run ~ctx = V.Builtin_vocabulary_seed.run ctx.conn

  let run_with_seed ~ctx ~seed =
    V.Builtin_vocabulary_seed.run_with_seed ctx.conn seed
end

let package_name = "chamallaw"

let api_version = 2
