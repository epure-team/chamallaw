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

let org = Authorized_scope.Organization {org_id = 1; actor_id = None}

let project =
  Authorized_scope.Project {project_id = 1; org_id = Some 1; actor_id = None}

let table_exists (module Db : Caqti_eio.CONNECTION) name =
  let req =
    Caqti_request.Infix.(Caqti_type.string ->? Caqti_type.int)
      "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?"
  in
  match Db.find_opt req name with
  | Ok (Some _) -> true
  | Ok None -> false
  | Error e ->
      Alcotest.failf "sqlite_master query failed: %s" (Caqti_error.show e)

let exec_sql_result (module Db : Caqti_eio.CONNECTION) sql =
  let req = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) sql in
  Db.exec req ()

let create_scheme ctx scope slug =
  ok_or_fail
    (Concept_scheme_store.create
       ~ctx
       ~scope
       ~slug
       ~display_name:slug
       ~provenance:User
       ())

let slugs rows = List.map (fun row -> row.Concept_scheme_store.slug) rows

let has_slug slug rows = List.exists (String.equal slug) (slugs rows)

let test_schema_creation_idempotent () =
  with_memory_db @@ fun conn ->
  let _ctx = init_ctx conn in
  Alcotest.(check bool)
    "concept_schemes exists"
    true
    (table_exists conn "concept_schemes") ;
  match ok_or_fail (init conn) with
  | Ready _ -> ()
  | Requires_migration _ -> Alcotest.fail "second init must be ready"

let test_scope_layering_project_caller () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  ignore (create_scheme ctx global "global-scheme") ;
  ignore (create_scheme ctx org "org-scheme") ;
  ignore (create_scheme ctx project "project-scheme") ;
  let rows =
    ok_or_fail (Concept_scheme_store.list_visible ~ctx ~scope:project)
  in
  Alcotest.(check bool) "global visible" true (has_slug "global-scheme" rows) ;
  Alcotest.(check bool) "org visible" true (has_slug "org-scheme" rows) ;
  Alcotest.(check bool) "project visible" true (has_slug "project-scheme" rows)

let test_scope_layering_org_caller () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  ignore (create_scheme ctx global "global-scheme") ;
  ignore (create_scheme ctx org "org-scheme") ;
  ignore (create_scheme ctx project "project-scheme") ;
  let rows = ok_or_fail (Concept_scheme_store.list_visible ~ctx ~scope:org) in
  Alcotest.(check bool) "global visible" true (has_slug "global-scheme" rows) ;
  Alcotest.(check bool) "org visible" true (has_slug "org-scheme" rows) ;
  Alcotest.(check bool) "project hidden" false (has_slug "project-scheme" rows)

let test_scope_layering_global_caller () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  ignore (create_scheme ctx global "global-scheme") ;
  ignore (create_scheme ctx org "org-scheme") ;
  ignore (create_scheme ctx project "project-scheme") ;
  let rows =
    ok_or_fail (Concept_scheme_store.list_visible ~ctx ~scope:global)
  in
  Alcotest.(check bool) "global visible" true (has_slug "global-scheme" rows) ;
  Alcotest.(check bool) "org hidden" false (has_slug "org-scheme" rows) ;
  Alcotest.(check bool) "project hidden" false (has_slug "project-scheme" rows)

let test_scheme_scope_check_rejects_mismatched_ids () =
  with_memory_db @@ fun conn ->
  let _ctx = init_ctx conn in
  match
    exec_sql_result
      conn
      {|INSERT INTO concept_schemes
        (slug, display_name, provenance, scope_kind, project_id)
        VALUES ('invalid-global-scheme', 'Invalid', 'user', 'global', 7)|}
  with
  | Ok () -> Alcotest.fail "global scheme with project_id should fail CHECK"
  | Error _ -> ()

let () =
  Alcotest.run
    "concept scheme store"
    [
      ( "concept_scheme_store",
        [
          Alcotest.test_case
            "schema creation idempotent"
            `Quick
            test_schema_creation_idempotent;
          Alcotest.test_case
            "project caller sees global org project"
            `Quick
            test_scope_layering_project_caller;
          Alcotest.test_case
            "org caller sees global org"
            `Quick
            test_scope_layering_org_caller;
          Alcotest.test_case
            "global caller sees global only"
            `Quick
            test_scope_layering_global_caller;
          Alcotest.test_case
            "scheme scope CHECK rejects mismatched ids"
            `Quick
            test_scheme_scope_check_rejects_mismatched_ids;
        ] );
    ]
