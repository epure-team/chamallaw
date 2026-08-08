(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

module LNM = Law_normative_metadata_store
module LCL = Law_concept_links_store

type origin = {force : LNM.force; authority : LNM.authority; severity : LNM.severity}

type gate_spec = {
  law_id : int;
  rule_predicate : string;
  gate_id : string;
  gate_on : string;
  origin : origin;
}

(* Mirrors epure-team/arch-index's Arch_sel.parse grammar (file:/fn:/module:,
   non-empty pattern) as a plain string check -- chamallaw does not and must
   not depend on arch-index itself (AGENTS.md host-neutrality). *)
let structural_anchor_of_slug slug =
  let has_prefix prefix =
    let plen = String.length prefix in
    String.length slug > plen && String.sub slug 0 plen = prefix
  in
  if has_prefix "file:" || has_prefix "fn:" || has_prefix "module:" then Some slug
  else None

let artifact_scope_concept_slug (reasons : Law_resolver.applicability_reason list) =
  List.find_map
    (function
      | Law_resolver.Link_match {link_role = LCL.Artifact_scope; concept_slug; _} ->
          Some concept_slug
      | Law_resolver.Link_match _ | Law_resolver.Scope_match _ -> None)
    reasons

let to_gate_spec (applicable : Law_resolver.applicable_law) =
  match (applicable.effective_force, applicable.effective_authority) with
  | _, (LNM.Advisory | LNM.Internal | LNM.External) -> Error "advisory does not gate"
  | LNM.Obligation, LNM.Mandatory ->
      Error
        "law force Obligation is not compilable: arch-rules has no positive/require \
         predicate form, only forbid reach/dep/exported/effect"
  | (LNM.Permission | LNM.Recommendation | LNM.Exception), LNM.Mandatory ->
      Error
        (Printf.sprintf
           "law force does not gate: only Prohibition/Mandatory compiles")
  | LNM.Prohibition, LNM.Mandatory -> (
      match artifact_scope_concept_slug applicable.reasons with
      | None -> Error "law has no Artifact_scope link to compile a structural predicate from"
      | Some slug -> (
          match structural_anchor_of_slug slug with
          | None ->
              Error
                (Printf.sprintf
                   "concept %S has no structural anchor (file:/fn:/module: selector) \
                    in the architecture index"
                   slug)
          | Some selector ->
              Ok
                {
                  law_id = applicable.law.Law_store.id;
                  rule_predicate = Printf.sprintf "forbid exported outside %s" selector;
                  gate_id = Printf.sprintf "g-law-%d" applicable.law.Law_store.id;
                  gate_on = "failing";
                  origin =
                    {
                      force = LNM.Prohibition;
                      authority = LNM.Mandatory;
                      severity = applicable.effective_severity;
                    };
                }))
