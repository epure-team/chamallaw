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

let ok_or_fail = function Ok v -> v | Error e -> Alcotest.failf "%s" e

let with_memory_db f =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  match
    Caqti_eio_unix.connect ~sw ~stdenv (Uri.of_string "sqlite3::memory:")
  with
  | Error e -> Alcotest.failf "DB connect failed: %s" (Caqti_error.show e)
  | Ok conn -> f conn

let init_ctx conn =
  match ok_or_fail (init conn) with
  | Ready ctx -> ctx
  | Requires_migration {apply; _} -> ok_or_fail (apply ())

let global = Authorized_scope.Global {actor_id = None}

let org3 = Authorized_scope.Organization {org_id = 3; actor_id = None}

let project7 =
  Authorized_scope.Project {project_id = 7; org_id = Some 3; actor_id = None}

let project_other_org =
  Authorized_scope.Project {project_id = 20; org_id = Some 4; actor_id = None}

let create_law ?target_scope ?replaces ?relation_kind ctx ~scope statement =
  ok_or_fail
    (Law_store.create_law
       ~ctx
       ~scope
       ?target_scope
       ~statement
       ?replaces
       ?relation_kind
       ())

let create_metadata ?target_scope ?(role_kind = M.Primary)
    ?(force = M.Prohibition) ?(authority = M.Mandatory) ctx ~scope ~law_id ()
    =
  ignore
    (ok_or_fail
       (M.create_metadata
          ~ctx
          ~scope
          ?target_scope
          ~law_id
          ~role_kind
          ~force
          ~modality:M.Strict
          ~strength:M.Hard
          ~severity:M.Critical
          ~authority
          ()))

let create_scheme ctx =
  ok_or_fail
    (Concept_scheme_store.create
       ~ctx
       ~scope:global
       ~slug:"resolver-test-scheme"
       ~display_name:"Resolver Test Scheme"
       ~provenance:User
       ())

let create_concept ctx scheme slug =
  ok_or_fail
    (Concept_store.create
       ~ctx
       ~scope:global
       ~scheme_id:scheme.Concept_scheme_store.id
       ~slug
       ~provenance:User
       ())

let create_link ?target_scope ~role ctx ~scope ~law_id ~concept_id () =
  ignore
    (ok_or_fail (L.create_link ~ctx ~scope ?target_scope ~law_id ~concept_id ~role ()))

let resolve ctx ~scope ~work_context =
  ok_or_fail (R.resolve ~ctx ~scope ~work_context)

let law_ids (applicable : R.applicable_law list) =
  List.map (fun (a : R.applicable_law) -> a.law.Law_store.id) applicable

let unknown_ids (unknown : (Law_store.law_row * string) list) =
  List.map (fun (l, _) -> l.Law_store.id) unknown

(* T-scope-superposition: an org law is applicable from a project scope of
   the same org, not from an unrelated org. Uses Law_store.list_visible
   directly (not reimplemented), so this is really a superposition test of
   the resolver over that existing scope filter. *)
let test_scope_superposition () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let org_law = create_law ctx ~scope:org3 "org-wide law" in
  create_metadata ctx ~scope:org3 ~law_id:org_law.id () ;
  let same_org_result = resolve ctx ~scope:project7 ~work_context:Normalized_work_context.empty in
  Alcotest.(check bool)
    "org law applicable from same-org project"
    true
    (List.mem org_law.id (law_ids same_org_result.applicable)) ;
  let other_org_result =
    resolve ctx ~scope:project_other_org ~work_context:Normalized_work_context.empty
  in
  Alcotest.(check bool)
    "org law not applicable from a different org's project"
    false
    (List.mem org_law.id (law_ids other_org_result.applicable)
    || List.mem org_law.id (unknown_ids other_org_result.unknown))

(* T-link-match: a law linked Artifact_scope -> concept("api") is applicable
   iff work_context.artifact_kind matches that concept. *)
let test_link_match () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law = create_law ctx ~scope:global "api law" in
  create_metadata ctx ~scope:global ~law_id:law.id () ;
  let scheme = create_scheme ctx in
  let concept = create_concept ctx scheme "api" in
  create_link
    ctx
    ~scope:global
    ~law_id:law.id
    ~concept_id:concept.Concept_store.id
    ~role:L.Artifact_scope
    () ;
  let matching =
    resolve
      ctx
      ~scope:global
      ~work_context:{Normalized_work_context.empty with artifact_kind = Some "api"}
  in
  Alcotest.(check bool)
    "law applies when artifact_kind matches the linked concept"
    true
    (List.mem law.id (law_ids matching.applicable)) ;
  let non_matching =
    resolve
      ctx
      ~scope:global
      ~work_context:{Normalized_work_context.empty with artifact_kind = Some "db"}
  in
  Alcotest.(check bool)
    "law disappears from applicable when artifact_kind no longer matches"
    false
    (List.mem law.id (law_ids non_matching.applicable))

(* T-unknown-not-no: a law visible with no normative metadata must not
   silently disappear -- it must land in `unknown`, not be treated as "does
   not apply". This is the fail-closed guarantee the whole milestone rests
   on. *)
let test_unknown_not_no () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law = create_law ctx ~scope:global "law with no metadata" in
  let result = resolve ctx ~scope:global ~work_context:Normalized_work_context.empty in
  Alcotest.(check bool)
    "law with no metadata is not silently absent"
    true
    (List.mem law.id (unknown_ids result.unknown)) ;
  Alcotest.(check bool)
    "law with no metadata never lands in applicable"
    false
    (List.mem law.id (law_ids result.applicable))

let test_reasons_nonempty () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let unlinked = create_law ctx ~scope:global "unlinked mandatory" in
  create_metadata ctx ~scope:global ~law_id:unlinked.id () ;
  let linked = create_law ctx ~scope:global "linked law" in
  create_metadata ctx ~scope:global ~law_id:linked.id () ;
  let scheme = create_scheme ctx in
  let concept = create_concept ctx scheme "phase-design" in
  create_link
    ctx
    ~scope:global
    ~law_id:linked.id
    ~concept_id:concept.Concept_store.id
    ~role:L.Phase_scope
    () ;
  let result =
    resolve
      ctx
      ~scope:global
      ~work_context:{Normalized_work_context.empty with phase = Some "phase-design"}
  in
  Alcotest.(check bool) "at least two applicable laws" true (List.length result.applicable >= 2) ;
  List.iter
    (fun (a : R.applicable_law) ->
      Alcotest.(check bool)
        (Printf.sprintf "law %d has non-empty reasons" a.law.Law_store.id)
        true
        (a.reasons <> []))
    result.applicable

(* T-override-legit: A(project) Overrides B(project, same scope) -> B is
   dropped from applicable, A stays, and A records B's id. *)
let test_override_legit () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let b = create_law ctx ~scope:project7 "B: base rule" in
  create_metadata ctx ~scope:project7 ~law_id:b.id () ;
  let a =
    create_law
      ctx
      ~scope:project7
      ~replaces:b.id
      ~relation_kind:Law_store.Overrides
      "A: overriding rule"
  in
  create_metadata ctx ~scope:project7 ~law_id:a.id () ;
  let result = resolve ctx ~scope:project7 ~work_context:Normalized_work_context.empty in
  Alcotest.(check bool) "B is dropped" false (List.mem b.id (law_ids result.applicable)) ;
  Alcotest.(check bool) "A is applicable" true (List.mem a.id (law_ids result.applicable)) ;
  let a_entry =
    List.find (fun (x : R.applicable_law) -> x.law.Law_store.id = a.id) result.applicable
  in
  Alcotest.(check bool) "A records having overridden B" true (List.mem b.id a_entry.overridden_by)

(* T-override-illegit-blocked: A(project) Overrides B(org, Mandatory) must be
   refused: B stays applicable, A becomes Unknown. *)
let test_override_illegit_blocked () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let b = create_law ctx ~scope:org3 "B: org-mandatory rule" in
  create_metadata ctx ~scope:org3 ~law_id:b.id ~authority:M.Mandatory () ;
  let a =
    create_law
      ctx
      ~scope:project7
      ~replaces:b.id
      ~relation_kind:Law_store.Overrides
      "A: illegitimate override attempt"
  in
  create_metadata ctx ~scope:project7 ~law_id:a.id () ;
  let result = resolve ctx ~scope:project7 ~work_context:Normalized_work_context.empty in
  Alcotest.(check bool)
    "B (superior-scope Mandatory) stays applicable, untouched"
    true
    (List.mem b.id (law_ids result.applicable)) ;
  Alcotest.(check bool) "A never silently applies" false (List.mem a.id (law_ids result.applicable)) ;
  Alcotest.(check bool) "A is reported Unknown" true (List.mem a.id (unknown_ids result.unknown))

let test_exempts_legit () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let b = create_law ctx ~scope:project7 "B: exempted rule" in
  create_metadata ctx ~scope:project7 ~law_id:b.id () ;
  let a =
    create_law
      ctx
      ~scope:project7
      ~replaces:b.id
      ~relation_kind:Law_store.Exempts
      "A: exempting rule"
  in
  create_metadata ctx ~scope:project7 ~law_id:a.id () ;
  let result = resolve ctx ~scope:project7 ~work_context:Normalized_work_context.empty in
  Alcotest.(check bool) "B is not applicable" false (List.mem b.id (law_ids result.applicable)) ;
  Alcotest.(check bool)
    "B is exempted, recording A"
    true
    (List.exists
       (fun (l, exempter) -> l.Law_store.id = b.id && exempter = a.id)
       result.exempted)

(* Refines: both coexist; the refining law inherits the refined law's
   applicability when it did not independently qualify, keeping its own
   (here stricter) force. *)
let test_refines_inherits_applicability () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let scheme = create_scheme ctx in
  let concept = create_concept ctx scheme "phase-review" in
  let b = create_law ctx ~scope:project7 "B: refined rule" in
  create_metadata ctx ~scope:project7 ~law_id:b.id () ;
  let a =
    create_law
      ctx
      ~scope:project7
      ~replaces:b.id
      ~relation_kind:Law_store.Refines
      "A: refining rule"
  in
  create_metadata ctx ~scope:project7 ~law_id:a.id ~force:M.Prohibition ~authority:M.Advisory () ;
  create_link
    ctx
    ~scope:project7
    ~law_id:a.id
    ~concept_id:concept.Concept_store.id
    ~role:L.Phase_scope
    () ;
  let result =
    resolve
      ctx
      ~scope:project7
      ~work_context:{Normalized_work_context.empty with phase = Some "something-else"}
  in
  Alcotest.(check bool) "B applies (unlinked mandatory)" true (List.mem b.id (law_ids result.applicable)) ;
  Alcotest.(check bool)
    "A inherits B's applicability despite its own link not matching"
    true
    (List.mem a.id (law_ids result.applicable)) ;
  let a_entry =
    List.find (fun (x : R.applicable_law) -> x.law.Law_store.id = a.id) result.applicable
  in
  Alcotest.(check bool) "A keeps its own reasons non-empty" true (a_entry.reasons <> []) ;
  Alcotest.(check bool)
    "A's own authority is used, not B's"
    true
    (a_entry.effective_authority = M.Advisory)

let () =
  Alcotest.run
    "chamallaw law resolver"
    [
      ( "law_resolver",
        [
          Alcotest.test_case "scope superposition" `Quick test_scope_superposition;
          Alcotest.test_case "link match" `Quick test_link_match;
          Alcotest.test_case "unknown not no" `Quick test_unknown_not_no;
          Alcotest.test_case "reasons nonempty" `Quick test_reasons_nonempty;
          Alcotest.test_case "override legit" `Quick test_override_legit;
          Alcotest.test_case
            "override illegit blocked"
            `Quick
            test_override_illegit_blocked;
          Alcotest.test_case "exempts legit" `Quick test_exempts_legit;
          Alcotest.test_case
            "refines inherits applicability"
            `Quick
            test_refines_inherits_applicability;
        ] );
    ]
