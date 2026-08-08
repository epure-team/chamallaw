(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Package-private contextual-law resolver (CH-01).

    Pure, total, deterministic: given a scope and a normalized work context,
    decides which visible laws are applicable, which are unresolvably unknown
    (fail-closed), and which are exempted by a legitimate [Exempts] relation.
    Never calls an LLM, never uses a clock or randomness, never treats an
    uncertain result as "no law applies". *)

(** Why one law landed in {!resolution.applicable}. Never empty for a given
    entry: applicability is always structurally traceable. *)
type applicability_reason =
  | Scope_match of Authorized_scope.t
    (** The law carries no restrictive applicability link, so it applies to
        every work context visible from [scope] (only used for
        [authority = Mandatory] laws; see [resolve]'s doc). *)
  | Link_match of {
      link_role : Law_concept_links_store.link_role;
      concept_id : int;
      concept_slug : string;
          (** The matched concept's slug, carried forward so CH-02a's
              [Law_to_gate_spec.to_gate_spec] can stay pure (no DB access) --
              see its doc for how a slug becomes a structural anchor. *)
      matched_field : string;
          (** Which [Normalized_work_context.t] field matched, e.g.
              ["artifact_kind"], ["phase"], ["agent_role"], ["domains"],
              ["action"]. *)
    }

type applicable_law = {
  law : Law_store.law_row;
  effective_force : Law_normative_metadata_store.force;
  effective_authority : Law_normative_metadata_store.authority;
  effective_severity : Law_normative_metadata_store.severity;
  reasons : applicability_reason list;
  overridden_by : int list;
      (** Ids of laws this law legitimately overrides (via [Overrides]),
          populated only when the overridden law was itself a candidate this
          resolution round. *)
}

type resolution = {
  applicable : applicable_law list;
  unknown : (Law_store.law_row * string) list;
      (** Fail-closed channel: laws whose applicability or relation
          legitimacy could not be certainly established. Never silently
          treated as "does not apply". *)
  exempted : (Law_store.law_row * int) list;
      (** Laws excluded by a legitimate [Exempts] relation, paired with the
          exempting law's id. *)
}

(** [resolve conn ~scope ~work_context] resolves the laws visible from
    [scope] (via {!Law_store.list_visible}, not reimplemented) against
    [work_context].

    Applicability (before relation-kind post-processing):
    - A law with at least one active, non-[Suggestion_only] concept link
      whose linked concept matches the corresponding [work_context] field
      (exact match on the concept's slug or an active label; never a fuzzy
      FTS score) is applicable, via [Link_match].
    - A law with active, non-[Suggestion_only] links none of which matched
      is excluded from this resolution round (not applicable, not unknown:
      a definite non-match).
    - A law with no restrictive link at all applies to every work context in
      its scope iff its effective authority is [Mandatory] ([Scope_match]);
      otherwise it is excluded (does not apply by default).
    - A law with no active, exactly-one [Primary]-role normative metadata
      row is [Unknown] (fail-closed): "no applicable law" is never inferred
      from missing metadata.
    - A [Suggestion_only] link never makes a law authoritatively applicable.

    Relation-kind post-processing (only laws that reached [applicable] act;
    an [Unknown] or excluded law never exercises governance power over
    another law this round):
    - [Replaces]: the replaced law is dropped entirely (historical).
    - [Overrides]/[Exempts]: legitimate only if the overridden/exempted law's
      effective authority is not [Mandatory], or its scope is not strictly
      superior (by scope kind: Global > Organization > Project) to the
      acting law's scope. An illegitimate attempt never drops the target; it
      instead turns the *acting* law's own verdict into [Unknown]. A
      legitimate [Overrides] drops the target from [applicable] and records
      its id in the acting law's [overridden_by]. A legitimate [Exempts]
      moves the target into [exempted].
    - [Refines]: both laws coexist. If the refining law did not
      independently qualify as applicable but the refined law did, the
      refining law inherits the refined law's applicability reasons (its own
      [effective_force]/[effective_authority]/[effective_severity] still
      come from its own metadata, which may be stricter). *)
val resolve :
  (module Caqti_eio.CONNECTION) ->
  scope:Authorized_scope.t ->
  work_context:Normalized_work_context.t ->
  (resolution, string) result
