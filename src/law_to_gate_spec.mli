(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Package-private compiler from a resolved law to an arch-rules gate spec
    (CH-02a).

    Pure, total, host-neutral: reads an already-resolved
    {!Law_resolver.applicable_law} and its links; never touches the
    database, never depends on cwr or arch-index. Emits data (a record of
    strings), it does not link against either tool.

    The real arch-rules grammar (verified against [epure-team/arch-index]'s
    [bin/arch_rules/arch_rules.ml] and [lib/arch_tools/arch_sel.ml], not
    invented) supports exactly:
    {v
      forbid reach from <sel> to <sel>
      forbid dep from <sel> to <sel>
      forbid exported outside <sel>
      forbid effect from <sel> kind:<VALUE_KIND>
    v}
    where [<sel>] is [file:<glob>], [fn:<glob>], or [module:<glob>]. There is
    no positive/"require" predicate form, and no [concept:] selector kind.
    Both matter here: an [Obligation] law can never compile (arch-rules has
    no way to state a positive requirement), and a chamallaw concept only
    anchors to the architecture index when its slug is itself a valid
    [file:]/[fn:]/[module:] selector -- a concept with no such correspondent
    is honestly non-compilable, not an error to paper over. *)

type origin = {
  force : Law_normative_metadata_store.force;
  authority : Law_normative_metadata_store.authority;
  severity : Law_normative_metadata_store.severity;
}

type gate_spec = {
  law_id : int;
  rule_predicate : string;
      (** One arch-rules.txt rule-body line, e.g.
          ["forbid exported outside file:**/payments_db.ml"]. Does not
          include the enclosing [rule "name"] line. *)
  gate_id : string;  (** ["g-law-<id>"], stable for a given [law_id]. *)
  gate_on : string;
      (** The arch-rules JSON output field a host floor gate should read:
          ["failing"] (the top-level failing-rule count in arch-rules'
          [--format json] output). Host-side (CH-02b, out of scope here)
          maps this to its own workflow-output-binding convention. *)
  origin : origin;
}

(** [to_gate_spec law] emits a gate spec only when: (a) [law]'s applicability
    is certain (it is an {!Law_resolver.applicable_law}, never an unknown or
    exempted entry -- those never reach this function); (b)
    [effective_force = Prohibition] and [effective_authority = Mandatory];
    (c) the law has an [Artifact_scope] link whose matched concept's slug is
    itself a [file:]/[fn:]/[module:] selector. Every other case returns
    [Error "<reason>"], explicitly -- never a vacuous or permissive gate. *)
val to_gate_spec :
  Law_resolver.applicable_law -> (gate_spec, string) result
