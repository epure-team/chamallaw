(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Chamallaw
module R = Law_resolver
module M = Law_normative_metadata_store
module L = Law_concept_links_store
module G = Law_to_gate_spec

let global = Authorized_scope.Global {actor_id = None}

let mock_law id : Law_store.law_row =
  {
    id;
    statement = "mock law";
    rationale = None;
    scope = global;
    owner_user_id = None;
    replaces_law_id = None;
    relation_kind = None;
    provenance_note = None;
    is_archived = false;
    created_at = Ptime.epoch;
    updated_at = Ptime.epoch;
  }

let artifact_link ~concept_id ~concept_slug =
  R.Link_match
    {
      link_role = L.Artifact_scope;
      concept_id;
      concept_slug;
      matched_field = "artifact_kind";
    }

let phase_link ~concept_id ~concept_slug =
  R.Link_match
    {link_role = L.Phase_scope; concept_id; concept_slug; matched_field = "phase"}

let applicable_law ?(force = M.Prohibition) ?(authority = M.Mandatory)
    ?(severity = M.Critical) ?(reasons = []) law_id : R.applicable_law =
  {
    law = mock_law law_id;
    effective_force = force;
    effective_authority = authority;
    effective_severity = severity;
    reasons;
    overridden_by = [];
  }

let ok_or_fail = function Ok v -> v | Error e -> Alcotest.failf "unexpected Error: %s" e

let expect_error msg = function
  | Ok _ -> Alcotest.failf "expected Error, got Ok (%s)" msg
  | Error _ -> ()

(* T-prohibition-mandatory-compiles *)
let test_prohibition_mandatory_compiles () =
  let law =
    applicable_law
      42
      ~reasons:[artifact_link ~concept_id:1 ~concept_slug:"file:**/payments_db.ml"]
  in
  let spec = ok_or_fail (G.to_gate_spec law) in
  Alcotest.(check string)
    "rule_predicate"
    "forbid exported outside file:**/payments_db.ml"
    spec.rule_predicate ;
  Alcotest.(check string) "gate_id" "g-law-42" spec.gate_id ;
  (* Mutation check: force <> Prohibition must not compile. *)
  let permission_law = applicable_law ~force:M.Permission 42 in
  expect_error "force=Permission should not gate" (G.to_gate_spec permission_law)

(* T-advisory-does-not-gate *)
let test_advisory_does_not_gate () =
  let law =
    applicable_law
      1
      ~authority:M.Advisory
      ~reasons:[artifact_link ~concept_id:1 ~concept_slug:"file:**/x.ml"]
  in
  match G.to_gate_spec law with
  | Ok _ -> Alcotest.fail "an advisory law must never compile to a gate"
  | Error msg -> Alcotest.(check string) "reason" "advisory does not gate" msg

(* T-non-anchored-concept-noncompilable *)
let test_non_anchored_concept_noncompilable () =
  let law =
    applicable_law 2 ~reasons:[artifact_link ~concept_id:1 ~concept_slug:"payments"]
  in
  expect_error "unanchored slug should not compile" (G.to_gate_spec law) ;
  (* A law with no Artifact_scope link at all (e.g. only a Phase_scope
     match) is equally non-compilable, not silently coerced. *)
  let phase_only =
    applicable_law 3 ~reasons:[phase_link ~concept_id:2 ~concept_slug:"file:**/y.ml"]
  in
  expect_error "no Artifact_scope link should not compile" (G.to_gate_spec phase_only)

(* T-spec-shape-golden: byte-stable emitted spec for a reference law, so
   CH-02b (host-side, out of scope here) can rely on its exact shape. *)
let test_spec_shape_golden () =
  let law =
    applicable_law
      7
      ~severity:M.High
      ~reasons:[artifact_link ~concept_id:99 ~concept_slug:"module:Payments.**"]
  in
  let spec = ok_or_fail (G.to_gate_spec law) in
  Alcotest.(check string)
    "rule_predicate golden"
    "forbid exported outside module:Payments.**"
    spec.rule_predicate ;
  Alcotest.(check string) "gate_id golden" "g-law-7" spec.gate_id ;
  Alcotest.(check string) "gate_on golden" "failing" spec.gate_on ;
  Alcotest.(check bool) "origin.force golden" true (spec.origin.force = M.Prohibition) ;
  Alcotest.(check bool) "origin.authority golden" true (spec.origin.authority = M.Mandatory) ;
  Alcotest.(check bool) "origin.severity golden" true (spec.origin.severity = M.High)

let test_obligation_never_compiles () =
  let law =
    applicable_law
      5
      ~force:M.Obligation
      ~reasons:[artifact_link ~concept_id:1 ~concept_slug:"file:**/x.ml"]
  in
  expect_error "Obligation has no positive predicate form in arch-rules" (G.to_gate_spec law)

let () =
  Alcotest.run
    "chamallaw law to gate spec"
    [
      ( "law_to_gate_spec",
        [
          Alcotest.test_case
            "prohibition mandatory compiles"
            `Quick
            test_prohibition_mandatory_compiles;
          Alcotest.test_case "advisory does not gate" `Quick test_advisory_does_not_gate;
          Alcotest.test_case
            "non-anchored concept non-compilable"
            `Quick
            test_non_anchored_concept_noncompilable;
          Alcotest.test_case "spec shape golden" `Quick test_spec_shape_golden;
          Alcotest.test_case "obligation never compiles" `Quick test_obligation_never_compiles;
        ] );
    ]
