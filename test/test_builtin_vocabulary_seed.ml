(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Chamallaw

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

let project =
  Authorized_scope.Project {project_id = 1; org_id = Some 1; actor_id = None}

let exec_sql (module Db : Caqti_eio.CONNECTION) sql =
  let req = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) sql in
  match Db.exec req () with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec failed: %s" (Caqti_error.show e)

let count_builtin_rows (module Db : Caqti_eio.CONNECTION) =
  let req =
    Caqti_request.Infix.(Caqti_type.unit ->! Caqti_type.int)
      {|SELECT
          (SELECT COUNT(*) FROM concept_relation_types
            WHERE slug IN ('broader', 'narrower', 'related', 'implies', 'conflicts_with'))
        + (SELECT COUNT(*) FROM concept_schemes WHERE provenance = 'epure_builtin')
        + (SELECT COUNT(*) FROM concepts WHERE provenance = 'epure_builtin')|}
  in
  match Db.find req () with
  | Ok count -> count
  | Error e -> Alcotest.failf "count failed: %s" (Caqti_error.show e)

let create_user_scheme ctx =
  ok_or_fail
    (Concept_scheme_store.create
       ~ctx
       ~scope:project
       ~slug:"user-scheme"
       ~display_name:"User Scheme"
       ~provenance:User
       ())

let get_scheme ctx slug =
  match
    ok_or_fail (Concept_scheme_store.get_by_slug ~ctx ~scope:project ~slug)
  with
  | Some row -> row
  | None -> Alcotest.failf "missing scheme %s" slug

let get_concept ctx scheme slug =
  match
    ok_or_fail
      (Concept_store.get_by_scheme_and_slug
         ~ctx
         ~scope:global
         ~scheme_id:scheme.Concept_scheme_store.id
         ~slug)
  with
  | Some row -> row
  | None -> Alcotest.failf "missing concept %s" slug

let test_seed_idempotency () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  ok_or_fail (Builtin_vocabulary_seed.run ~ctx) ;
  let phases = get_scheme ctx "epure-builtin-phases" in
  Alcotest.(check string)
    "built-in display"
    "Épure Built-in Phases"
    phases.display_name ;
  let user_scheme = create_user_scheme ctx in
  ok_or_fail (Builtin_vocabulary_seed.run ~ctx) ;
  let phases_after = get_scheme ctx "epure-builtin-phases" in
  let user_after = get_scheme ctx "user-scheme" in
  Alcotest.(check string)
    "built-in converged"
    "Épure Built-in Phases"
    phases_after.display_name ;
  Alcotest.(check int) "user row preserved" user_scheme.id user_after.id ;
  Alcotest.(check string)
    "user display preserved"
    "User Scheme"
    user_after.display_name

let test_seed_builtin_update () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  ok_or_fail (Builtin_vocabulary_seed.run ~ctx) ;
  let phases = get_scheme ctx "epure-builtin-phases" in
  let design = get_concept ctx phases "design" in
  Alcotest.(check (option string))
    "initial definition"
    (Some "Architecture and design phase")
    design.definition ;
  exec_sql
    conn
    "UPDATE concepts SET definition = 'OLD DEFINITION' WHERE id IN (SELECT \
     c.id FROM concepts c JOIN concept_schemes s ON s.id = c.scheme_id WHERE \
     s.slug = 'epure-builtin-phases' AND c.slug = 'design')" ;
  ok_or_fail (Builtin_vocabulary_seed.run ~ctx) ;
  let restored = get_concept ctx phases "design" in
  Alcotest.(check (option string))
    "restored definition"
    (Some "Architecture and design phase")
    restored.definition

let test_seed_dimensions_coverage () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  ok_or_fail (Builtin_vocabulary_seed.run ~ctx) ;
  let phases = get_scheme ctx "epure-builtin-phases" in
  let roles = get_scheme ctx "epure-builtin-agent-roles" in
  let languages = get_scheme ctx "epure-builtin-languages" in
  let phase_slugs =
    List.map
      (fun row -> row.Concept_store.slug)
      (ok_or_fail
         (Concept_store.list_visible_by_scheme
            ~ctx
            ~scope:global
            ~scheme_id:phases.id))
  in
  List.iter
    (fun slug ->
      Alcotest.(check bool) (slug ^ " phase") true (List.mem slug phase_slugs))
    ["design"; "implementation"; "testing"; "review"; "deployment"] ;
  let role_slugs =
    List.map
      (fun row -> row.Concept_store.slug)
      (ok_or_fail
         (Concept_store.list_visible_by_scheme
            ~ctx
            ~scope:global
            ~scheme_id:roles.id))
  in
  List.iter
    (fun slug ->
      Alcotest.(check bool) (slug ^ " role") true (List.mem slug role_slugs))
    ["analyst"; "architect"; "builder"; "reviewer"; "critic"] ;
  let language_slugs =
    List.map
      (fun row -> row.Concept_store.slug)
      (ok_or_fail
         (Concept_store.list_visible_by_scheme
            ~ctx
            ~scope:global
            ~scheme_id:languages.id))
  in
  List.iter
    (fun slug ->
      Alcotest.(check bool)
        (slug ^ " language")
        true
        (List.mem slug language_slugs))
    ["ocaml"; "typescript"; "python"]

let test_seed_partial_failure_rolls_back () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let before = count_builtin_rows conn in
  exec_sql
    conn
    {|CREATE TRIGGER fail_builtin_seed_testing
      BEFORE INSERT ON concepts
      WHEN NEW.slug = 'testing'
      BEGIN
        SELECT RAISE(FAIL, 'test seed failure');
      END|} ;
  (match Builtin_vocabulary_seed.run ~ctx with
  | Ok () -> Alcotest.fail "seed run should fail through test trigger"
  | Error _ -> ()) ;
  Alcotest.(check int)
    "no partial built-ins remain"
    before
    (count_builtin_rows conn)

let relation_seed_json =
  {|{
    "relation_types": [
      {
        "slug": "broader",
        "is_hierarchical": true,
        "is_traversal_enabled": true
      }
    ],
    "schemes": [
      {
        "slug": "test-relation-scheme",
        "display_name": "Test Relation Scheme",
        "description": null,
        "scope_kind": "global"
      }
    ],
    "concepts": [
      {
        "scheme_slug": "test-relation-scheme",
        "slug": "parent",
        "definition": "Parent concept",
        "scope_note": null
      },
      {
        "scheme_slug": "test-relation-scheme",
        "slug": "child",
        "definition": "Child concept",
        "scope_note": null
      }
    ],
    "labels": [],
    "relations": [
      {
        "from_scheme_slug": "test-relation-scheme",
        "from_concept_slug": "parent",
        "to_scheme_slug": "test-relation-scheme",
        "to_concept_slug": "child",
        "relation_type_slug": "broader"
      }
    ]
  }|}

let test_seed_relations_are_idempotent () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let seed = ok_or_fail (Seed.of_json_string relation_seed_json) in
  ok_or_fail (Builtin_vocabulary_seed.run_with_seed ~ctx ~seed) ;
  ok_or_fail (Builtin_vocabulary_seed.run_with_seed ~ctx ~seed) ;
  let scheme = get_scheme ctx "test-relation-scheme" in
  let parent = get_concept ctx scheme "parent" in
  let relations =
    ok_or_fail
      (Concept_relation_store.list_relations_by_concept
         ~ctx
         ~scope:global
         ~concept_id:parent.id)
  in
  Alcotest.(check int)
    "one idempotent seeded relation"
    1
    (List.length relations)

let () =
  Alcotest.run
    "built-in vocabulary seed"
    [
      ( "builtin_vocabulary_seed",
        [
          Alcotest.test_case "seed idempotency" `Quick test_seed_idempotency;
          Alcotest.test_case
            "built-in rows converge"
            `Quick
            test_seed_builtin_update;
          Alcotest.test_case
            "seed dimensions coverage"
            `Quick
            test_seed_dimensions_coverage;
          Alcotest.test_case
            "partial failure rolls back built-in rows"
            `Quick
            test_seed_partial_failure_rolls_back;
          Alcotest.test_case
            "relation seed path is idempotent"
            `Quick
            test_seed_relations_are_idempotent;
        ] );
    ]
