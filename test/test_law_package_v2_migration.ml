(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Chamallaw

let ok_or_fail = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected error: %s" e

let with_memory_db f =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  match
    Caqti_eio_unix.connect ~sw ~stdenv (Uri.of_string "sqlite3::memory:")
  with
  | Error e -> Alcotest.failf "DB connect failed: %s" (Caqti_error.show e)
  | Ok conn -> f conn

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

let table_exists conn table =
  let (module Db : Caqti_eio.CONNECTION) = conn in
  let req =
    Caqti_request.Infix.(Caqti_type.string ->! Caqti_type.int)
      "SELECT count(*) FROM sqlite_master WHERE type='table' AND name = ?"
  in
  match Db.find req table with
  | Ok count -> count > 0
  | Error e ->
      Alcotest.failf "sqlite_master query failed: %s" (Caqti_error.show e)

let sqlite_master_snapshot (module Db : Caqti_eio.CONNECTION) =
  let req =
    Caqti_request.Infix.(
      Caqti_type.unit ->* Caqti_type.(t3 string string string))
      {|SELECT type, name, COALESCE(sql, '')
          FROM sqlite_master
         WHERE name NOT LIKE 'sqlite_%'
         ORDER BY type, name|}
  in
  match Db.collect_list req () with
  | Ok rows -> rows
  | Error e ->
      Alcotest.failf "sqlite_master snapshot failed: %s" (Caqti_error.show e)

let assert_v2_metadata ctx =
  match ok_or_fail (Package_metadata_store.get ctx) with
  | None -> Alcotest.fail "expected package metadata row"
  | Some row ->
      Alcotest.(check string) "package name" "chamallaw" row.package_name ;
      Alcotest.(check int) "api version" 2 row.api_version

let metadata_or_fail ctx =
  match ok_or_fail (Package_metadata_store.get ctx) with
  | None -> Alcotest.fail "expected package metadata row"
  | Some row -> row

let assert_v2_tables conn =
  List.iter
    (fun table ->
      Alcotest.(check bool) (table ^ " exists") true (table_exists conn table))
    ["laws"; "law_normative_metadata"; "law_concept_links"]

let assert_foreign_keys conn =
  Alcotest.(check int)
    "foreign_keys pragma"
    1
    (find_int conn "PRAGMA foreign_keys")

let package_version conn =
  find_int
    conn
    {|SELECT version
        FROM law_package_schema_version
       WHERE singleton_key = 'epure-law'|}

let provision_legacy_v1 conn =
  exec_sql
    conn
    {|CREATE TABLE IF NOT EXISTS law_package_schema_version (
        singleton_key TEXT PRIMARY KEY CHECK(singleton_key = 'epure-law'),
        version       INTEGER NOT NULL
      )|} ;
  exec_sql
    conn
    {|CREATE TABLE IF NOT EXISTS law_package_metadata (
        singleton_key TEXT PRIMARY KEY CHECK(singleton_key = 'epure-law'),
        package_name  TEXT NOT NULL,
        api_version   INTEGER NOT NULL,
        created_at    TEXT NOT NULL DEFAULT (datetime('now')),
        updated_at    TEXT NOT NULL DEFAULT (datetime('now'))
      )|} ;
  exec_sql
    conn
    {|CREATE TABLE IF NOT EXISTS concept_schemes (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        slug TEXT NOT NULL,
        display_name TEXT NOT NULL,
        description TEXT,
        provenance TEXT NOT NULL CHECK (provenance IN ('epure_builtin', 'user')),
        scope_kind TEXT NOT NULL CHECK (scope_kind IN ('global', 'organization', 'project')),
        org_id INTEGER,
        project_id INTEGER,
        created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
        CHECK (
          (scope_kind = 'global'       AND org_id IS NULL     AND project_id IS NULL) OR
          (scope_kind = 'organization' AND org_id IS NOT NULL AND project_id IS NULL) OR
          (scope_kind = 'project'                              AND project_id IS NOT NULL)
        )
      )|} ;
  exec_sql
    conn
    {|CREATE TABLE IF NOT EXISTS concepts (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        scheme_id INTEGER NOT NULL REFERENCES concept_schemes(id) ON DELETE CASCADE,
        slug TEXT NOT NULL,
        definition TEXT,
        scope_note TEXT,
        provenance TEXT NOT NULL CHECK (provenance IN ('epure_builtin', 'user')),
        scope_kind TEXT NOT NULL CHECK (scope_kind IN ('global', 'organization', 'project')),
        org_id INTEGER,
        project_id INTEGER,
        created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
        CHECK (
          (scope_kind = 'global'       AND org_id IS NULL     AND project_id IS NULL) OR
          (scope_kind = 'organization' AND org_id IS NOT NULL AND project_id IS NULL) OR
          (scope_kind = 'project'                              AND project_id IS NOT NULL)
        ),
        UNIQUE (scheme_id, slug)
      )|} ;
  exec_sql
    conn
    {|INSERT INTO law_package_metadata (singleton_key, package_name, api_version)
      VALUES ('epure-law', 'epure-law', 1)|} ;
  exec_sql
    conn
    {|INSERT INTO law_package_schema_version (singleton_key, version)
      VALUES ('epure-law', 1)|}

let test_fresh_db_lands_on_v2 () =
  with_memory_db @@ fun conn ->
  (match ok_or_fail (init conn) with
  | Ready _ -> Alcotest.fail "fresh DB must require v2 migration"
  | Requires_migration {from_version; to_version; apply} ->
      Alcotest.(check int) "from version" 0 from_version ;
      Alcotest.(check int) "to version" 2 to_version ;
      let ctx = ok_or_fail (apply ()) in
      assert_v2_metadata ctx) ;
  Alcotest.(check int) "package schema version" 2 (package_version conn) ;
  assert_v2_tables conn ;
  assert_foreign_keys conn

let test_legacy_v1_to_v2_migration () =
  with_memory_db @@ fun conn ->
  provision_legacy_v1 conn ;
  match ok_or_fail (init conn) with
  | Ready _ -> Alcotest.fail "legacy v1 DB must require v2 migration"
  | Requires_migration {from_version; to_version; apply} ->
      Alcotest.(check int) "from version" 1 from_version ;
      Alcotest.(check int) "to version" 2 to_version ;
      let ctx = ok_or_fail (apply ()) in
      assert_v2_metadata ctx ;
      Alcotest.(check int) "package schema version" 2 (package_version conn) ;
      Alcotest.(check bool)
        "v1 table intact"
        true
        (table_exists conn "concepts") ;
      assert_v2_tables conn ;
      assert_foreign_keys conn

let test_already_v2_idempotent () =
  with_memory_db @@ fun conn ->
  let ctx =
    match ok_or_fail (init conn) with
    | Ready ctx -> ctx
    | Requires_migration {apply; _} -> ok_or_fail (apply ())
  in
  assert_v2_metadata ctx ;
  let before = sqlite_master_snapshot conn in
  (match ok_or_fail (init conn) with
  | Ready ctx -> assert_v2_metadata ctx
  | Requires_migration _ -> Alcotest.fail "v2 DB should already be Ready") ;
  let after = sqlite_master_snapshot conn in
  Alcotest.(check int)
    "same schema object count"
    (List.length before)
    (List.length after) ;
  Alcotest.(check (list (triple string string string)))
    "same sqlite_master"
    before
    after

let test_ready_v2_refreshes_legacy_public_package_name () =
  with_memory_db @@ fun conn ->
  let ctx =
    match ok_or_fail (init conn) with
    | Ready ctx -> ctx
    | Requires_migration {apply; _} -> ok_or_fail (apply ())
  in
  assert_v2_metadata ctx ;
  exec_sql
    conn
    {|UPDATE law_package_metadata
         SET package_name = 'epure-law', api_version = 1
       WHERE singleton_key = 'epure-law'|} ;
  match ok_or_fail (init conn) with
  | Ready ctx -> assert_v2_metadata ctx
  | Requires_migration _ -> Alcotest.fail "v2 DB should stay Ready"

let test_ready_v2_does_not_touch_current_metadata () =
  with_memory_db @@ fun conn ->
  let ctx =
    match ok_or_fail (init conn) with
    | Ready ctx -> ctx
    | Requires_migration {apply; _} -> ok_or_fail (apply ())
  in
  assert_v2_metadata ctx ;
  let sentinel_updated_at = "2000-01-01 00:00:00" in
  exec_sql
    conn
    (Printf.sprintf
       {|UPDATE law_package_metadata
            SET updated_at = '%s'
          WHERE singleton_key = 'epure-law'|}
       sentinel_updated_at) ;
  match ok_or_fail (init conn) with
  | Ready ctx ->
      let row = metadata_or_fail ctx in
      Alcotest.(check string)
        "current metadata updated_at unchanged"
        sentinel_updated_at
        row.updated_at
  | Requires_migration _ -> Alcotest.fail "v2 DB should stay Ready"

let () =
  Alcotest.run
    "chamallaw package v2 migration"
    [
      ( "law_package_v2_migration",
        [
          Alcotest.test_case
            "fresh DB lands on v2"
            `Quick
            test_fresh_db_lands_on_v2;
          Alcotest.test_case
            "legacy v1 to v2 migration"
            `Quick
            test_legacy_v1_to_v2_migration;
          Alcotest.test_case
            "already v2 idempotent"
            `Quick
            test_already_v2_idempotent;
          Alcotest.test_case
            "ready v2 refreshes legacy public package name"
            `Quick
            test_ready_v2_refreshes_legacy_public_package_name;
          Alcotest.test_case
            "ready v2 does not touch current metadata"
            `Quick
            test_ready_v2_does_not_touch_current_metadata;
        ] );
    ]
