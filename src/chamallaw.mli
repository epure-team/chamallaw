(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Chamallaw standalone law-package scaffold for Epic 89. *)

module Authorized_scope = Authorized_scope
module Normalized_work_context = Normalized_work_context
module Host_adapter = Host_adapter

(** Opaque handle proving the package schema has been checked and is ready.

    Values of this type can only be obtained from [Ready ctx] returned by
    {!init} or from a successful [apply ()] on {!Requires_migration}. Public
    package APIs require this handle so callers cannot access package-owned
    stores before package migrations have been applied. *)
type ctx

(** Provenance of package-owned vocabulary rows. *)
type provenance = Epure_builtin | User

type init_result =
  | Ready of ctx
  | Requires_migration of {
      from_version : int;
      to_version : int;
      apply : unit -> (ctx, string) result;
    }

val init : (module Caqti_eio.CONNECTION) -> (init_result, string) result

module Package_metadata_store : sig
  type row = {
    package_name : string;
    api_version : int;
    created_at : string;
    updated_at : string;
  }

  val get : ctx -> (row option, string) result
end

(** Concept scheme storage. *)
module Concept_scheme_store : sig
  (** Persisted concept-scheme row. *)
  type scheme_row = {
    id : int;
    slug : string;
    display_name : string;
    description : string option;
    provenance : provenance;
    scope : Authorized_scope.t;
    created_at : Ptime.t;
  }

  (** Create one concept scheme in the authorized scope. *)
  val create :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    slug:string ->
    display_name:string ->
    ?description:string ->
    provenance:provenance ->
    unit ->
    (scheme_row, string) result

  (** List schemes visible from the authorized scope. *)
  val list_visible :
    ctx:ctx -> scope:Authorized_scope.t -> (scheme_row list, string) result

  (** Return the most specific visible scheme with the given slug. *)
  val get_by_slug :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    slug:string ->
    (scheme_row option, string) result
end

(** Concept storage. *)
module Concept_store : sig
  (** Persisted concept row. *)
  type concept_row = {
    id : int;
    scheme_id : int;
    slug : string;
    definition : string option;
    scope_note : string option;
    provenance : provenance;
    scope : Authorized_scope.t;
    created_at : Ptime.t;
  }

  (** Create one scheme-qualified concept in the authorized scope. *)
  val create :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    scheme_id:int ->
    slug:string ->
    ?definition:string ->
    ?scope_note:string ->
    provenance:provenance ->
    unit ->
    (concept_row, string) result

  (** List visible concepts that belong to a scheme. *)
  val list_visible_by_scheme :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    scheme_id:int ->
    (concept_row list, string) result

  (** Return the most specific visible concept by scheme and slug. *)
  val get_by_scheme_and_slug :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    scheme_id:int ->
    slug:string ->
    (concept_row option, string) result
end

(** Concept label storage. *)
module Concept_label_store : sig
  (** Kind of label attached to a concept. *)
  type label_kind =
    | Label_preferred
    | Label_alternate
    | Label_hidden
    | Label_deprecated

  (** Staleness state for label-string references. *)
  type staleness_status = Active | Deprecated | Stale

  (** Persisted concept-label row. *)
  type label_row = {
    id : int;
    concept_id : int;
    text : string;
    kind : label_kind;
    staleness_status : staleness_status;
    last_marked_at : Ptime.t option;
    created_at : Ptime.t;
  }

  (** Create one label for a visible concept. *)
  val create :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    concept_id:int ->
    text:string ->
    kind:label_kind ->
    (label_row, string) result

  (** List labels for a visible concept. *)
  val list_by_concept :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    concept_id:int ->
    (label_row list, string) result

  (** Mark a label's staleness status using the package wall clock. *)
  val mark_stale :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    label_id:int ->
    status:staleness_status ->
    (unit, string) result
end

(** Concept relation storage. *)
module Concept_relation_store : sig
  (** Relation-type metadata row. *)
  type relation_type_row = {
    id : int;
    slug : string;
    is_hierarchical : bool;
    is_traversal_enabled : bool;
  }

  (** Persisted concept-relation row. *)
  type relation_row = {
    id : int;
    from_concept_id : int;
    to_concept_id : int;
    relation_type_id : int;
    is_active : bool;
    created_at : Ptime.t;
  }

  (** List all configured relation types. *)
  val list_relation_types : ctx:ctx -> (relation_type_row list, string) result

  (** Create one active relation and reject hierarchical cycles.

      A second active relation with the same [(from_concept_id, to_concept_id,
      relation_type_id)] triple returns [Error "duplicate active relation"]. *)
  val create_relation :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    from_concept_id:int ->
    to_concept_id:int ->
    relation_type_id:int ->
    (relation_row, string) result

  (** List active outgoing relations for a visible concept. *)
  val list_relations_by_concept :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    concept_id:int ->
    (relation_row list, string) result

  (** List active incoming relations for a visible concept. *)
  val list_incoming_relations :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    concept_id:int ->
    (relation_row list, string) result
end

(** FTS5 concept search. *)
module Concept_search_store : sig
  (** BM25-ranked concept-search result. *)
  type search_result = {concept_id : int; rank : float}

  (** Search visible concepts by label, definition, and scope note. *)
  val search :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    query:string ->
    limit:int ->
    (search_result list, string) result
end

(** Canonical package-owned law table (#581). *)
module Law_store : sig
  (** Lifecycle relation between two laws. *)
  type relation_kind = Replaces | Refines | Overrides | Exempts

  (** Persisted law row. *)
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

  (** Create one law in the authorized scope.

      [scope] is the actor; [target_scope] is the layer the row is recorded at
      and defaults to [scope]. A denied pair returns
      [Error "scope authorization denied"]. *)
  val create_law :
    ctx:ctx ->
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

  (** Return one law by id, only if visible from [scope]. *)
  val get_law :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    law_id:int ->
    (law_row option, string) result

  (** List laws visible from [scope] (project ∪ org-of-project ∪ global). *)
  val list_visible :
    ctx:ctx -> scope:Authorized_scope.t -> (law_row list, string) result

  (** Set or clear relation columns on an existing law. *)
  val update_relation :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    ?target_scope:Authorized_scope.t ->
    law_id:int ->
    replaces:int option ->
    relation_kind:relation_kind option ->
    unit ->
    (law_row, string) result

  (** Archive a law with a soft-delete flag. *)
  val archive_law :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    ?target_scope:Authorized_scope.t ->
    law_id:int ->
    unit ->
    (unit, string) result
end

(** Normative metadata storage for laws (#559). *)
module Law_normative_metadata_store : sig
  type force =
    | Obligation
    | Prohibition
    | Permission
    | Recommendation
    | Exception

  type modality = Strict | Lenient | Conditional

  type strength = Hard | Soft

  type severity = Critical | High | Medium | Low | Informational

  type authority = Mandatory | Advisory | Internal | External

  type role_kind = Primary | Secondary

  (** Persisted normative-metadata row. *)
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

  (** Create one normative-metadata row for a package law. *)
  val create_metadata :
    ctx:ctx ->
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

  (** List active metadata rows for a law, visible from [scope]. *)
  val list_for_law :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    law_id:int ->
    (metadata_row list, string) result

  (** Soft-toggle one metadata row. *)
  val set_active :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    ?target_scope:Authorized_scope.t ->
    metadata_id:int ->
    is_active:bool ->
    unit ->
    (unit, string) result

  (** Atomic replace for all active rows for [law_id] in [target_scope]. *)
  val replace_for_law :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    ?target_scope:Authorized_scope.t ->
    law_id:int ->
    rows:metadata_row list ->
    unit ->
    (metadata_row list, string) result
end

(** Law-to-concept link storage (#560 AC1 + AC3). *)
module Law_concept_links_store : sig
  type link_role =
    | Primary_subject
    | Applicability_context
    | Concern
    | Artifact_scope
    | Phase_scope
    | Agent_scope
    | Suggestion_only

  (** Persisted law-concept link row. *)
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

  (** Create one active link. *)
  val create_link :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    ?target_scope:Authorized_scope.t ->
    law_id:int ->
    concept_id:int ->
    role:link_role ->
    unit ->
    (link_row, string) result

  (** List active links for a law, visible from [scope]. *)
  val list_for_law :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    law_id:int ->
    (link_row list, string) result

  (** List active links pointing at a concept, visible from [scope]. *)
  val list_for_concept :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    concept_id:int ->
    (link_row list, string) result

  (** Soft-deactivate one link row. *)
  val deactivate_link :
    ctx:ctx ->
    scope:Authorized_scope.t ->
    ?target_scope:Authorized_scope.t ->
    link_id:int ->
    unit ->
    (unit, string) result
end

(** Built-in vocabulary seed payloads. *)
module Seed : sig
  type t

  (** Parse a seed JSON payload. *)
  val of_json_string : string -> (t, string) result
end

(** Built-in vocabulary seed runner. *)
module Builtin_vocabulary_seed : sig
  (** Idempotently apply built-in vocabulary from the embedded seed JSON. *)
  val run : ctx:ctx -> (unit, string) result

  (** Idempotently apply built-in vocabulary from a parsed seed payload. *)
  val run_with_seed : ctx:ctx -> seed:Seed.t -> (unit, string) result
end

val package_name : string

val api_version : int
