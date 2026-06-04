(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Chamallaw
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

let with_file_db f =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let path = Filename.temp_file "chamallaw_link_" ".db" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun suffix -> try Sys.remove (path ^ suffix) with Sys_error _ -> ())
        [""; "-journal"; "-shm"; "-wal"])
    (fun () ->
      let stdenv = (env :> Caqti_eio.stdenv) in
      let connect () =
        match
          Caqti_eio_unix.connect ~sw ~stdenv (Uri.of_string ("sqlite3:" ^ path))
        with
        | Error e -> Alcotest.failf "DB connect failed: %s" (Caqti_error.show e)
        | Ok conn -> conn
      in
      f (connect ()) (connect ()))

let set_busy_timeout (module Db : Caqti_eio.CONNECTION) =
  let req =
    Caqti_request.Infix.(Caqti_type.unit ->? Caqti_type.int)
      "PRAGMA busy_timeout = 5000"
  in
  match Db.find_opt req () with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "busy_timeout failed: %s" (Caqti_error.show e)

let exec_sql (module Db : Caqti_eio.CONNECTION) sql =
  let req = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) sql in
  match Db.exec req () with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec failed: %s" (Caqti_error.show e)

let find_int (module Db : Caqti_eio.CONNECTION) sql =
  let req = Caqti_request.Infix.(Caqti_type.unit ->! Caqti_type.int) sql in
  match Db.find req () with
  | Ok value -> value
  | Error e -> Alcotest.failf "query failed: %s" (Caqti_error.show e)

let init_ctx conn =
  match ok_or_fail (init conn) with
  | Ready ctx -> ctx
  | Requires_migration {apply; _} -> ok_or_fail (apply ())

let global = Authorized_scope.Global {actor_id = None}

let org3 = Authorized_scope.Organization {org_id = 3; actor_id = None}

let project7 =
  Authorized_scope.Project {project_id = 7; org_id = Some 3; actor_id = None}

let project11 =
  Authorized_scope.Project {project_id = 11; org_id = Some 3; actor_id = None}

let create_law ?target_scope ctx ~scope statement =
  ok_or_fail (Law_store.create_law ~ctx ~scope ?target_scope ~statement ())

let create_scheme ctx =
  ok_or_fail
    (Concept_scheme_store.create
       ~ctx
       ~scope:global
       ~slug:"law-link-scheme"
       ~display_name:"Law Link Scheme"
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

let create_link ?target_scope ?(role = L.Primary_subject) ctx ~scope ~law_id
    ~concept_id () =
  L.create_link ~ctx ~scope ?target_scope ~law_id ~concept_id ~role ()

let setup_law_and_concept ctx =
  let law = create_law ctx ~scope:global "law" in
  let scheme = create_scheme ctx in
  let concept = create_concept ctx scheme "concept" in
  (law, concept)

let expect_scope_denied = function
  | Ok _ -> Alcotest.fail "expected scope authorization denial"
  | Error msg ->
      Alcotest.(check string) "scope error" "scope authorization denied" msg

let assert_project_scope row =
  Alcotest.(check (option int))
    "link project id"
    (Some 7)
    (Authorized_scope.project_id row.L.scope) ;
  Alcotest.(check (option int))
    "link org id"
    (Some 3)
    (Authorized_scope.org_id row.scope)

let link_count conn = find_int conn "SELECT count(*) FROM law_concept_links"

let active_count conn =
  find_int conn "SELECT count(*) FROM law_concept_links WHERE is_active = 1"

let concurrent_pair first second =
  let gate, resolver = Eio.Promise.create () in
  let first_result = ref None in
  let second_result = ref None in
  Eio.Fiber.both
    (fun () ->
      Eio.Promise.await gate ;
      first_result := Some (first ()))
    (fun () ->
      Eio.Promise.resolve resolver () ;
      second_result := Some (second ())) ;
  match (!first_result, !second_result) with
  | Some a, Some b -> (a, b)
  | _ -> Alcotest.fail "concurrent link fiber did not record a result"

let all_roles =
  [
    L.Primary_subject;
    L.Applicability_context;
    L.Concern;
    L.Artifact_scope;
    L.Phase_scope;
    L.Agent_scope;
    L.Suggestion_only;
  ]

let test_create_link_typed_roles () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law, concept = setup_law_and_concept ctx in
  let rows =
    List.map
      (fun role ->
        ok_or_fail
          (create_link
             ctx
             ~scope:global
             ~law_id:law.id
             ~concept_id:concept.id
             ~role
             ()))
      all_roles
  in
  Alcotest.(check int) "all roles persisted" 7 (List.length rows) ;
  List.iter2
    (fun expected row ->
      Alcotest.(check bool) "role round-tripped" true (row.L.role = expected))
    all_roles
    rows

let test_cross_scope_link_org_actor_writes_project_target () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law, concept = setup_law_and_concept ctx in
  let row =
    ok_or_fail
      (create_link
         ctx
         ~scope:org3
         ~target_scope:project7
         ~law_id:law.id
         ~concept_id:concept.id
         ())
  in
  assert_project_scope row

let test_cross_scope_link_global_actor_writes_project_target () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law, concept = setup_law_and_concept ctx in
  let row =
    ok_or_fail
      (create_link
         ctx
         ~scope:global
         ~target_scope:project7
         ~law_id:law.id
         ~concept_id:concept.id
         ())
  in
  assert_project_scope row

let test_cross_scope_link_project_actor_blocked_upward () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law, concept = setup_law_and_concept ctx in
  expect_scope_denied
    (create_link
       ctx
       ~scope:project7
       ~target_scope:org3
       ~law_id:law.id
       ~concept_id:concept.id
       ())

let test_cross_scope_link_project_actor_blocked_sideways () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law, concept = setup_law_and_concept ctx in
  expect_scope_denied
    (create_link
       ctx
       ~scope:project7
       ~target_scope:project11
       ~law_id:law.id
       ~concept_id:concept.id
       ())

let test_cross_scope_link_deactivate_org_actor_on_project_row () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law, concept = setup_law_and_concept ctx in
  let row =
    ok_or_fail
      (create_link
         ctx
         ~scope:org3
         ~target_scope:project7
         ~law_id:law.id
         ~concept_id:concept.id
         ())
  in
  ok_or_fail
    (L.deactivate_link
       ~ctx
       ~scope:org3
       ~target_scope:project7
       ~link_id:row.id
       ()) ;
  expect_scope_denied
    (L.deactivate_link
       ~ctx
       ~scope:project11
       ~target_scope:project7
       ~link_id:row.id
       ())

let test_create_link_for_missing_law_rejected_by_fk () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let scheme = create_scheme ctx in
  let concept = create_concept ctx scheme "orphan" in
  match
    create_link ctx ~scope:global ~law_id:999_999 ~concept_id:concept.id ()
  with
  | Ok _ -> Alcotest.fail "link for missing law should fail"
  | Error _ -> ()

let test_create_link_for_missing_concept_rejected_by_fk () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law = create_law ctx ~scope:global "law" in
  match create_link ctx ~scope:global ~law_id:law.id ~concept_id:999_999 () with
  | Ok _ -> Alcotest.fail "link for missing concept should fail"
  | Error _ -> ()

let test_global_and_project_link_with_same_triple_coexist () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law, concept = setup_law_and_concept ctx in
  ignore
    (ok_or_fail
       (create_link ctx ~scope:global ~law_id:law.id ~concept_id:concept.id ())) ;
  ignore
    (ok_or_fail
       (create_link
          ctx
          ~scope:global
          ~target_scope:project7
          ~law_id:law.id
          ~concept_id:concept.id
          ())) ;
  let rows = ok_or_fail (L.list_for_law ~ctx ~scope:project7 ~law_id:law.id) in
  Alcotest.(check int) "global and project links visible" 2 (List.length rows)

let test_duplicate_active_link_within_same_scope_rejected () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law, concept = setup_law_and_concept ctx in
  ignore
    (ok_or_fail
       (create_link ctx ~scope:global ~law_id:law.id ~concept_id:concept.id ())) ;
  match
    create_link ctx ~scope:global ~law_id:law.id ~concept_id:concept.id ()
  with
  | Ok _ -> Alcotest.fail "duplicate active link should fail"
  | Error msg ->
      Alcotest.(check string)
        "duplicate link error"
        "duplicate active law-concept link"
        msg

let test_deactivate_link_then_reinsert_succeeds () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law, concept = setup_law_and_concept ctx in
  let row =
    ok_or_fail
      (create_link ctx ~scope:global ~law_id:law.id ~concept_id:concept.id ())
  in
  ok_or_fail (L.deactivate_link ~ctx ~scope:global ~link_id:row.id ()) ;
  ignore
    (ok_or_fail
       (create_link ctx ~scope:global ~law_id:law.id ~concept_id:concept.id ())) ;
  Alcotest.(check int) "audit rows preserved" 2 (link_count conn) ;
  Alcotest.(check int) "one active row" 1 (active_count conn)

let test_list_for_law_filters_by_scope () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law, concept = setup_law_and_concept ctx in
  ignore
    (ok_or_fail
       (create_link
          ctx
          ~scope:global
          ~target_scope:project7
          ~law_id:law.id
          ~concept_id:concept.id
          ())) ;
  let project_a =
    ok_or_fail (L.list_for_law ~ctx ~scope:project7 ~law_id:law.id)
  in
  let project_b =
    ok_or_fail (L.list_for_law ~ctx ~scope:project11 ~law_id:law.id)
  in
  Alcotest.(check int) "project A sees link" 1 (List.length project_a) ;
  Alcotest.(check int) "project B does not see link" 0 (List.length project_b)

let test_duplicate_active_link_concurrent () =
  with_file_db @@ fun conn_a conn_b ->
  set_busy_timeout conn_a ;
  set_busy_timeout conn_b ;
  let ctx_a = init_ctx conn_a in
  let ctx_b = init_ctx conn_b in
  let law, concept = setup_law_and_concept ctx_a in
  let a, b =
    concurrent_pair
      (fun () ->
        create_link ctx_a ~scope:global ~law_id:law.id ~concept_id:concept.id ())
      (fun () ->
        create_link ctx_b ~scope:global ~law_id:law.id ~concept_id:concept.id ())
  in
  let ok_count =
    List.fold_left
      (fun acc -> function Ok _ -> acc + 1 | Error _ -> acc)
      0
      [a; b]
  in
  let duplicate_count =
    List.fold_left
      (fun acc -> function
        | Ok _ -> acc
        | Error "duplicate active law-concept link" -> acc + 1
        | Error msg -> Alcotest.failf "unexpected link error: %s" msg)
      0
      [a; b]
  in
  Alcotest.(check int) "one link insert succeeds" 1 ok_count ;
  Alcotest.(check int) "one link insert duplicates" 1 duplicate_count ;
  Alcotest.(check int) "one active link row" 1 (active_count conn_a)

let test_hard_delete_law_cascades_links () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law, concept = setup_law_and_concept ctx in
  ignore
    (ok_or_fail
       (create_link ctx ~scope:global ~law_id:law.id ~concept_id:concept.id ())) ;
  exec_sql conn (Printf.sprintf "DELETE FROM laws WHERE id = %d" law.id) ;
  Alcotest.(check int) "links cascaded" 0 (link_count conn)

let () =
  Alcotest.run
    "chamallaw concept links store"
    [
      ( "law_concept_links_store",
        [
          Alcotest.test_case
            "create link typed roles"
            `Quick
            test_create_link_typed_roles;
          Alcotest.test_case
            "cross-scope link org actor writes project target"
            `Quick
            test_cross_scope_link_org_actor_writes_project_target;
          Alcotest.test_case
            "cross-scope link global actor writes project target"
            `Quick
            test_cross_scope_link_global_actor_writes_project_target;
          Alcotest.test_case
            "cross-scope link project actor blocked upward"
            `Quick
            test_cross_scope_link_project_actor_blocked_upward;
          Alcotest.test_case
            "cross-scope link project actor blocked sideways"
            `Quick
            test_cross_scope_link_project_actor_blocked_sideways;
          Alcotest.test_case
            "cross-scope link deactivate org actor on project row"
            `Quick
            test_cross_scope_link_deactivate_org_actor_on_project_row;
          Alcotest.test_case
            "create link for missing law rejected by FK"
            `Quick
            test_create_link_for_missing_law_rejected_by_fk;
          Alcotest.test_case
            "create link for missing concept rejected by FK"
            `Quick
            test_create_link_for_missing_concept_rejected_by_fk;
          Alcotest.test_case
            "global and project link with same triple coexist"
            `Quick
            test_global_and_project_link_with_same_triple_coexist;
          Alcotest.test_case
            "duplicate active link within same scope rejected"
            `Quick
            test_duplicate_active_link_within_same_scope_rejected;
          Alcotest.test_case
            "deactivate link then reinsert succeeds"
            `Quick
            test_deactivate_link_then_reinsert_succeeds;
          Alcotest.test_case
            "list for law filters by scope"
            `Quick
            test_list_for_law_filters_by_scope;
          Alcotest.test_case
            "duplicate active link concurrent"
            `Quick
            test_duplicate_active_link_concurrent;
          Alcotest.test_case
            "hard delete law cascades links"
            `Quick
            test_hard_delete_law_cascades_links;
        ] );
    ]
