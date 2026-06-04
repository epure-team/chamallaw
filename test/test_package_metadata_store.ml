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

let exec_sql (module Db : Caqti_eio.CONNECTION) sql =
  let req = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) sql in
  match Db.exec req () with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec failed: %s" (Caqti_error.show e)

let contains needle hay =
  let nlen = String.length needle in
  let hlen = String.length hay in
  if nlen = 0 then true
  else
    let rec loop i =
      if i + nlen > hlen then false
      else if String.sub hay i nlen = needle then true
      else loop (i + 1)
    in
    loop 0

let assert_error_contains ~needle = function
  | Ok _ -> Alcotest.failf "expected error containing %S" needle
  | Error msg ->
      Alcotest.(check bool)
        (Printf.sprintf "error contains %S" needle)
        true
        (contains needle msg)

let with_memory_db f =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  match
    Caqti_eio_unix.connect ~sw ~stdenv (Uri.of_string "sqlite3::memory:")
  with
  | Error e -> Alcotest.failf "DB connect failed: %s" (Caqti_error.show e)
  | Ok conn -> f conn

let connect_file_db ~sw ~stdenv path =
  match
    Caqti_eio_unix.connect ~sw ~stdenv (Uri.of_string ("sqlite3:" ^ path))
  with
  | Error e -> Alcotest.failf "DB connect failed: %s" (Caqti_error.show e)
  | Ok conn -> conn

let with_file_db f =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let path = Filename.temp_file "chamallaw_pkg_" ".db" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun suffix -> try Sys.remove (path ^ suffix) with Sys_error _ -> ())
        [""; "-shm"; "-wal"])
    (fun () -> f ~connect:(fun () -> connect_file_db ~sw ~stdenv path))

let init_or_apply conn =
  match ok_or_fail (init conn) with
  | Ready ctx -> ctx
  | Requires_migration {apply; _} -> ok_or_fail (apply ())

let assert_metadata ctx =
  match ok_or_fail (Package_metadata_store.get ctx) with
  | None -> Alcotest.fail "expected package metadata row"
  | Some row ->
      Alcotest.(check string) "package name" "chamallaw" row.package_name ;
      Alcotest.(check int) "api version" 2 row.api_version

let test_fresh_init_requires_migration_and_apply_returns_ctx () =
  with_memory_db @@ fun conn ->
  match ok_or_fail (init conn) with
  | Ready _ -> Alcotest.fail "fresh DB must require package migration"
  | Requires_migration {from_version; to_version; apply} -> (
      Alcotest.(check int) "from version" 0 from_version ;
      Alcotest.(check int) "to version" 2 to_version ;
      let ctx = ok_or_fail (apply ()) in
      assert_metadata ctx ;
      assert_metadata (ok_or_fail (apply ())) ;
      match ok_or_fail (init conn) with
      | Ready ctx -> assert_metadata ctx
      | Requires_migration _ ->
          Alcotest.fail "second init after apply must be Ready")

let test_already_migrated_db_returns_ready_on_new_connection () =
  with_file_db @@ fun ~connect ->
  let conn = connect () in
  let ctx = init_or_apply conn in
  assert_metadata ctx ;
  let conn2 = connect () in
  match ok_or_fail (init conn2) with
  | Ready ctx -> assert_metadata ctx
  | Requires_migration _ ->
      Alcotest.fail "already migrated file DB must return Ready"

let test_ready_init_does_not_require_write_lock () =
  with_file_db @@ fun ~connect ->
  let conn = connect () in
  let ctx = init_or_apply conn in
  assert_metadata ctx ;
  let blocker = connect () in
  exec_sql blocker "BEGIN IMMEDIATE" ;
  Fun.protect
    ~finally:(fun () -> exec_sql blocker "ROLLBACK")
    (fun () ->
      match ok_or_fail (init conn) with
      | Ready ctx -> assert_metadata ctx
      | Requires_migration _ ->
          Alcotest.fail "already-current DB must stay Ready without write lock")

let test_public_surface_smoke () =
  let open Authorized_scope in
  let scope =
    Project {project_id = 7; org_id = Some 3; actor_id = Some "maintainer"}
  in
  let ctx = Normalized_work_context.empty in
  Alcotest.(check (option int)) "project scope id" (Some 7) (project_id scope) ;
  Alcotest.(check (option int)) "org scope id" (Some 3) (org_id scope) ;
  Alcotest.(check int) "empty context files" 0 (List.length ctx.context_files) ;
  Alcotest.(check string) "package name" "chamallaw" package_name

let test_public_ctx_is_required_for_metadata_get () =
  with_memory_db @@ fun conn ->
  let ctx = init_or_apply conn in
  assert_metadata ctx

let force_package_version conn version =
  exec_sql
    conn
    {|CREATE TABLE IF NOT EXISTS law_package_schema_version (
        singleton_key TEXT PRIMARY KEY CHECK(singleton_key = 'epure-law'),
        version       INTEGER NOT NULL
      )|} ;
  exec_sql
    conn
    (Printf.sprintf
       {|INSERT INTO law_package_schema_version (singleton_key, version)
           VALUES ('epure-law', %d)
           ON CONFLICT(singleton_key) DO UPDATE SET version = excluded.version|}
       version)

let test_init_fails_closed_on_newer_package_version () =
  with_memory_db @@ fun conn ->
  force_package_version conn 3 ;
  assert_error_contains ~needle:"newer" (init conn)

let test_stale_apply_fails_if_version_advanced_past_target () =
  with_memory_db @@ fun conn ->
  match ok_or_fail (init conn) with
  | Ready _ -> Alcotest.fail "fresh DB must require package migration"
  | Requires_migration {apply; _} ->
      force_package_version conn 3 ;
      assert_error_contains ~needle:"newer" (apply ())

let () =
  Alcotest.run
    "chamallaw package metadata store"
    [
      ( "package_metadata_store",
        [
          Alcotest.test_case
            "fresh init requires migration and apply returns ctx"
            `Quick
            test_fresh_init_requires_migration_and_apply_returns_ctx;
          Alcotest.test_case
            "already migrated DB returns Ready on a new connection"
            `Quick
            test_already_migrated_db_returns_ready_on_new_connection;
          Alcotest.test_case
            "ready init does not require write lock"
            `Quick
            test_ready_init_does_not_require_write_lock;
          Alcotest.test_case
            "public surface smoke"
            `Quick
            test_public_surface_smoke;
          Alcotest.test_case
            "metadata get requires initialized ctx"
            `Quick
            test_public_ctx_is_required_for_metadata_get;
          Alcotest.test_case
            "init fails closed on newer package version"
            `Quick
            test_init_fails_closed_on_newer_package_version;
          Alcotest.test_case
            "stale apply fails if package version advanced past target"
            `Quick
            test_stale_apply_fails_if_version_advanced_past_target;
        ] );
    ]
