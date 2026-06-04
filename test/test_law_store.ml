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

let exec_sql_result (module Db : Caqti_eio.CONNECTION) sql =
  let req = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) sql in
  Db.exec req () |> Result.map_error Caqti_error.show

let expect_sql_error conn sql =
  match exec_sql_result conn sql with
  | Ok () -> Alcotest.fail "expected SQL statement to fail"
  | Error _ -> ()

let global = Authorized_scope.Global {actor_id = None}

let org3 = Authorized_scope.Organization {org_id = 3; actor_id = None}

let org4 = Authorized_scope.Organization {org_id = 4; actor_id = None}

let project7 =
  Authorized_scope.Project {project_id = 7; org_id = Some 3; actor_id = None}

let project7_no_org =
  Authorized_scope.Project {project_id = 7; org_id = None; actor_id = None}

let project11 =
  Authorized_scope.Project {project_id = 11; org_id = Some 3; actor_id = None}

let project_other_org =
  Authorized_scope.Project {project_id = 7; org_id = Some 4; actor_id = None}

let create_law ?target_scope ?replaces ?relation_kind ctx ~scope statement =
  Law_store.create_law
    ~ctx
    ~scope
    ?target_scope
    ~statement
    ?replaces
    ?relation_kind
    ()

let assert_project_scope row =
  Alcotest.(check (option int))
    "project id"
    (Some 7)
    (Authorized_scope.project_id row.Law_store.scope) ;
  Alcotest.(check (option int))
    "org id"
    (Some 3)
    (Authorized_scope.org_id row.scope)

let assert_org_scope row org_id =
  Alcotest.(check (option int))
    "org id"
    (Some org_id)
    (Authorized_scope.org_id row.Law_store.scope) ;
  Alcotest.(check (option int))
    "no project id"
    None
    (Authorized_scope.project_id row.scope)

let assert_global_scope row =
  Alcotest.(check (option int))
    "no project id"
    None
    (Authorized_scope.project_id row.Law_store.scope) ;
  Alcotest.(check (option int))
    "no org id"
    None
    (Authorized_scope.org_id row.scope)

let expect_scope_denied = function
  | Ok _ -> Alcotest.fail "expected scope authorization denial"
  | Error msg ->
      Alcotest.(check string) "scope error" "scope authorization denied" msg

let ids rows = List.map (fun row -> row.Law_store.id) rows

let test_create_law_at_global_scope () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let row = ok_or_fail (create_law ctx ~scope:global "Global law") in
  assert_global_scope row ;
  Alcotest.(check bool) "not archived" false row.is_archived

let test_create_law_at_org_scope_requires_org_id () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  expect_sql_error
    conn
    {|INSERT INTO laws (statement, scope_kind, created_at, updated_at)
      VALUES ('bad org', 'org', '2026-06-02T00:00:00Z', '2026-06-02T00:00:00Z')|} ;
  let row = ok_or_fail (create_law ctx ~scope:org3 "Org law") in
  assert_org_scope row 3

let test_create_law_at_project_scope_requires_project_id () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  expect_sql_error
    conn
    {|INSERT INTO laws (statement, scope_kind, created_at, updated_at)
      VALUES ('bad project', 'project', '2026-06-02T00:00:00Z', '2026-06-02T00:00:00Z')|} ;
  let row = ok_or_fail (create_law ctx ~scope:project7 "Project law") in
  assert_project_scope row

let test_cross_scope_law_default_target_equals_actor () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let row = ok_or_fail (create_law ctx ~scope:project7 "Default target") in
  assert_project_scope row

let test_cross_scope_law_global_actor_authors_org_target () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let row =
    ok_or_fail (create_law ctx ~scope:global ~target_scope:org3 "Global to org")
  in
  assert_org_scope row 3

let test_cross_scope_law_global_actor_authors_project_target () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let row =
    ok_or_fail
      (create_law ctx ~scope:global ~target_scope:project7 "Global to project")
  in
  assert_project_scope row

let test_cross_scope_law_org_actor_authors_org_target () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let row =
    ok_or_fail (create_law ctx ~scope:org3 ~target_scope:org3 "Org to org")
  in
  assert_org_scope row 3

let test_cross_scope_law_org_actor_authors_child_project_target () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let row =
    ok_or_fail
      (create_law ctx ~scope:org3 ~target_scope:project7 "Org to project")
  in
  assert_project_scope row

let test_cross_scope_law_org_actor_blocked_other_org_target () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  expect_scope_denied
    (create_law ctx ~scope:org3 ~target_scope:org4 "Org to other org")

let test_cross_scope_law_org_actor_blocked_to_other_org_project () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  expect_scope_denied
    (create_law
       ctx
       ~scope:org3
       ~target_scope:project_other_org
       "Org to other org project")

let test_cross_scope_law_project_actor_authors_self_target () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let row =
    ok_or_fail
      (create_law ctx ~scope:project7 ~target_scope:project7 "Project self")
  in
  assert_project_scope row

let test_cross_scope_project_actor_none_org_blocked_when_target_has_some_org ()
    =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  expect_scope_denied
    (create_law
       ctx
       ~scope:project7_no_org
       ~target_scope:project7
       "Project none org to project some org")

let test_cross_scope_law_project_actor_blocked_org_target () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  expect_scope_denied
    (create_law ctx ~scope:project7 ~target_scope:org3 "Project to org")

let test_cross_scope_law_project_actor_blocked_global_target () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  expect_scope_denied
    (create_law ctx ~scope:project7 ~target_scope:global "Project to global")

let test_cross_scope_law_project_actor_blocked_sideways () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  expect_scope_denied
    (create_law ctx ~scope:project7 ~target_scope:project11 "Project sideways")

let test_replaces_relation_both_or_neither () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let base = ok_or_fail (create_law ctx ~scope:global "Base") in
  (match create_law ctx ~scope:global ~replaces:base.id "Missing relation" with
  | Ok _ -> Alcotest.fail "replaces without relation_kind should fail"
  | Error _ -> ()) ;
  (match
     create_law
       ctx
       ~scope:global
       ~relation_kind:Law_store.Refines
       "Missing replaces"
   with
  | Ok _ -> Alcotest.fail "relation_kind without replaces should fail"
  | Error _ -> ()) ;
  let replacement =
    ok_or_fail
      (create_law
         ctx
         ~scope:global
         ~replaces:base.id
         ~relation_kind:Law_store.Replaces
         "Replacement")
  in
  Alcotest.(check (option int))
    "replaces id"
    (Some base.id)
    replacement.replaces_law_id ;
  Alcotest.(check bool)
    "relation kind present"
    true
    (Option.is_some replacement.relation_kind) ;
  ignore (ok_or_fail (create_law ctx ~scope:global "No relation"))

let test_replaces_law_id_fk_to_self_table () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  match
    create_law
      ctx
      ~scope:global
      ~replaces:999_999
      ~relation_kind:Law_store.Replaces
      "Broken relation"
  with
  | Ok _ -> Alcotest.fail "missing replacement target should fail"
  | Error _ -> ()

let test_archive_law_idempotent () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let row = ok_or_fail (create_law ctx ~scope:global "Archive me") in
  ok_or_fail (Law_store.archive_law ~ctx ~scope:global ~law_id:row.id ()) ;
  ok_or_fail (Law_store.archive_law ~ctx ~scope:global ~law_id:row.id ()) ;
  match ok_or_fail (Law_store.get_law ~ctx ~scope:global ~law_id:row.id) with
  | None -> Alcotest.fail "archived law should still be reloadable"
  | Some archived -> Alcotest.(check bool) "archived" true archived.is_archived

let test_list_visible_layering () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let global_law = ok_or_fail (create_law ctx ~scope:global "global") in
  let org_law = ok_or_fail (create_law ctx ~scope:org3 "org3") in
  let other_org_law = ok_or_fail (create_law ctx ~scope:org4 "org4") in
  let project_law = ok_or_fail (create_law ctx ~scope:project7 "project7") in
  let other_project_law =
    ok_or_fail (create_law ctx ~scope:project11 "project11")
  in
  let project_ids =
    ids (ok_or_fail (Law_store.list_visible ~ctx ~scope:project7))
  in
  List.iter
    (fun id ->
      Alcotest.(check bool)
        "project can see expected id"
        true
        (List.mem id project_ids))
    [global_law.id; org_law.id; project_law.id] ;
  List.iter
    (fun id ->
      Alcotest.(check bool)
        "project cannot see unexpected id"
        false
        (List.mem id project_ids))
    [other_org_law.id; other_project_law.id] ;
  let org_ids = ids (ok_or_fail (Law_store.list_visible ~ctx ~scope:org3)) in
  Alcotest.(check bool) "org sees global" true (List.mem global_law.id org_ids) ;
  Alcotest.(check bool) "org sees own org" true (List.mem org_law.id org_ids) ;
  Alcotest.(check bool)
    "org does not see project"
    false
    (List.mem project_law.id org_ids) ;
  let global_ids =
    ids (ok_or_fail (Law_store.list_visible ~ctx ~scope:global))
  in
  Alcotest.(check (list int))
    "global sees only global"
    [global_law.id]
    global_ids

let () =
  Alcotest.run
    "chamallaw law store"
    [
      ( "law_store",
        [
          Alcotest.test_case
            "create law at global scope"
            `Quick
            test_create_law_at_global_scope;
          Alcotest.test_case
            "create law at org scope requires org id"
            `Quick
            test_create_law_at_org_scope_requires_org_id;
          Alcotest.test_case
            "create law at project scope requires project id"
            `Quick
            test_create_law_at_project_scope_requires_project_id;
          Alcotest.test_case
            "cross-scope default target equals actor"
            `Quick
            test_cross_scope_law_default_target_equals_actor;
          Alcotest.test_case
            "cross-scope global actor authors org target"
            `Quick
            test_cross_scope_law_global_actor_authors_org_target;
          Alcotest.test_case
            "cross-scope global actor authors project target"
            `Quick
            test_cross_scope_law_global_actor_authors_project_target;
          Alcotest.test_case
            "cross-scope org actor authors org target"
            `Quick
            test_cross_scope_law_org_actor_authors_org_target;
          Alcotest.test_case
            "cross-scope org actor authors child project target"
            `Quick
            test_cross_scope_law_org_actor_authors_child_project_target;
          Alcotest.test_case
            "cross-scope org actor blocked other org target"
            `Quick
            test_cross_scope_law_org_actor_blocked_other_org_target;
          Alcotest.test_case
            "cross-scope org actor blocked other org project"
            `Quick
            test_cross_scope_law_org_actor_blocked_to_other_org_project;
          Alcotest.test_case
            "cross-scope project actor authors self target"
            `Quick
            test_cross_scope_law_project_actor_authors_self_target;
          Alcotest.test_case
            "cross-scope project actor none org blocked when target has some \
             org"
            `Quick
            test_cross_scope_project_actor_none_org_blocked_when_target_has_some_org;
          Alcotest.test_case
            "cross-scope project actor blocked org target"
            `Quick
            test_cross_scope_law_project_actor_blocked_org_target;
          Alcotest.test_case
            "cross-scope project actor blocked global target"
            `Quick
            test_cross_scope_law_project_actor_blocked_global_target;
          Alcotest.test_case
            "cross-scope project actor blocked sideways"
            `Quick
            test_cross_scope_law_project_actor_blocked_sideways;
          Alcotest.test_case
            "replaces relation both or neither"
            `Quick
            test_replaces_relation_both_or_neither;
          Alcotest.test_case
            "replaces law id FK to self table"
            `Quick
            test_replaces_law_id_fk_to_self_table;
          Alcotest.test_case
            "archive law idempotent"
            `Quick
            test_archive_law_idempotent;
          Alcotest.test_case
            "list visible layering"
            `Quick
            test_list_visible_layering;
        ] );
    ]
