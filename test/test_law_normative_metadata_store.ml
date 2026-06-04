(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Chamallaw
module M = Law_normative_metadata_store

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
  let path = Filename.temp_file "chamallaw_metadata_" ".db" in
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

let exec_sql_result (module Db : Caqti_eio.CONNECTION) sql =
  let req = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) sql in
  Db.exec req () |> Result.map_error Caqti_error.show

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

let create_metadata ?target_scope ?(role_kind = M.Primary)
    ?(force = M.Obligation) ctx ~scope ~law_id () =
  M.create_metadata
    ~ctx
    ~scope
    ?target_scope
    ~law_id
    ~role_kind
    ~force
    ~modality:M.Strict
    ~strength:M.Hard
    ~severity:M.Critical
    ~authority:M.Mandatory
    ()

let expect_scope_denied = function
  | Ok _ -> Alcotest.fail "expected scope authorization denial"
  | Error msg ->
      Alcotest.(check string) "scope error" "scope authorization denied" msg

let assert_project_scope row =
  Alcotest.(check (option int))
    "metadata project id"
    (Some 7)
    (Authorized_scope.project_id row.M.scope) ;
  Alcotest.(check (option int))
    "metadata org id"
    (Some 3)
    (Authorized_scope.org_id row.scope)

let active_count conn =
  find_int
    conn
    "SELECT count(*) FROM law_normative_metadata WHERE is_active = 1"

let row_count conn = find_int conn "SELECT count(*) FROM law_normative_metadata"

let metadata_template ?(role_kind = M.Primary) ?(force = M.Permission) () =
  let timestamp = Ptime_clock.now () in
  {
    M.id = 0;
    law_id = 0;
    role_kind;
    force;
    modality = M.Conditional;
    strength = M.Soft;
    severity = M.Low;
    authority = M.Advisory;
    is_active = true;
    scope = global;
    created_at = timestamp;
    updated_at = timestamp;
  }

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
  | _ -> Alcotest.fail "concurrent metadata fiber did not record a result"

let test_create_metadata_attaches_to_existing_law () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law = create_law ctx ~scope:global "law" in
  let row = ok_or_fail (create_metadata ctx ~scope:global ~law_id:law.id ()) in
  Alcotest.(check int) "law id" law.id row.law_id ;
  Alcotest.(check bool) "active" true row.is_active

let test_cross_scope_metadata_global_actor_writes_any () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law = create_law ctx ~scope:global "law" in
  let row =
    ok_or_fail
      (create_metadata
         ctx
         ~scope:global
         ~target_scope:project7
         ~law_id:law.id
         ())
  in
  assert_project_scope row

let test_cross_scope_metadata_org_actor_writes_project_target () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law = create_law ctx ~scope:global "law" in
  let row =
    ok_or_fail
      (create_metadata ctx ~scope:org3 ~target_scope:project7 ~law_id:law.id ())
  in
  assert_project_scope row

let test_cross_scope_metadata_project_actor_blocked_upward () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law = create_law ctx ~scope:global "law" in
  expect_scope_denied
    (create_metadata ctx ~scope:project7 ~target_scope:org3 ~law_id:law.id ())

let test_cross_scope_metadata_project_actor_blocked_sideways () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law = create_law ctx ~scope:global "law" in
  expect_scope_denied
    (create_metadata
       ctx
       ~scope:project7
       ~target_scope:project11
       ~law_id:law.id
       ())

let test_cross_scope_metadata_replace_for_law_authorized () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law = create_law ctx ~scope:global "law" in
  let rows =
    [
      metadata_template ~role_kind:M.Primary ();
      metadata_template ~role_kind:M.Secondary ();
    ]
  in
  let inserted =
    ok_or_fail
      (M.replace_for_law
         ~ctx
         ~scope:org3
         ~target_scope:project7
         ~law_id:law.id
         ~rows
         ())
  in
  Alcotest.(check int) "two replacement rows" 2 (List.length inserted) ;
  let before =
    ok_or_fail (M.list_for_law ~ctx ~scope:project7 ~law_id:law.id)
  in
  expect_scope_denied
    (M.replace_for_law
       ~ctx
       ~scope:project11
       ~target_scope:project7
       ~law_id:law.id
       ~rows:[metadata_template ~role_kind:M.Primary ~force:M.Exception ()]
       ()) ;
  let after = ok_or_fail (M.list_for_law ~ctx ~scope:project7 ~law_id:law.id) in
  Alcotest.(check int)
    "denied replace preserves active set"
    (List.length before)
    (List.length after)

let test_create_metadata_for_missing_law_rejected_by_fk () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  match create_metadata ctx ~scope:global ~law_id:999_999 () with
  | Ok _ -> Alcotest.fail "metadata for missing law should fail"
  | Error _ -> ()

let test_force_enum_accepts_exception () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law = create_law ctx ~scope:global "law" in
  let row =
    ok_or_fail
      (create_metadata ctx ~scope:global ~law_id:law.id ~force:M.Exception ())
  in
  match row.force with
  | M.Exception -> ()
  | _ -> Alcotest.fail "Exception force should round-trip"

let test_force_enum_rejects_unknown_slug () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law = create_law ctx ~scope:global "law" in
  let sql slug =
    Printf.sprintf
      {|INSERT INTO law_normative_metadata
        (law_id, role_kind, force, modality, strength, severity, authority,
         scope_kind, created_at, updated_at)
        VALUES (%d, 'primary', '%s', 'strict', 'hard', 'critical', 'mandatory',
                'global', '2026-06-02T00:00:00Z', '2026-06-02T00:00:00Z')|}
      law.id
      slug
  in
  List.iter
    (fun slug ->
      match exec_sql_result conn (sql slug) with
      | Ok () -> Alcotest.failf "slug %s should be rejected" slug
      | Error _ -> ())
    ["default"; "foo"]

let test_unique_active_per_role_kind () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law = create_law ctx ~scope:global "law" in
  ignore (ok_or_fail (create_metadata ctx ~scope:global ~law_id:law.id ())) ;
  (match create_metadata ctx ~scope:global ~law_id:law.id () with
  | Ok _ -> Alcotest.fail "duplicate active primary metadata should fail"
  | Error msg ->
      Alcotest.(check string)
        "duplicate metadata error"
        "duplicate active normative metadata"
        msg) ;
  ignore
    (ok_or_fail
       (create_metadata
          ctx
          ~scope:global
          ~law_id:law.id
          ~role_kind:M.Secondary
          ()))

let test_replace_for_law_atomic_swap () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law = create_law ctx ~scope:global "law" in
  let first =
    ok_or_fail (create_metadata ctx ~scope:global ~law_id:law.id ())
  in
  let second =
    ok_or_fail
      (create_metadata
         ctx
         ~scope:global
         ~law_id:law.id
         ~role_kind:M.Secondary
         ())
  in
  let replacements =
    [
      metadata_template ~role_kind:M.Primary ~force:M.Permission ();
      metadata_template ~role_kind:M.Secondary ~force:M.Recommendation ();
    ]
  in
  let active =
    ok_or_fail
      (M.replace_for_law
         ~ctx
         ~scope:global
         ~law_id:law.id
         ~rows:replacements
         ())
  in
  Alcotest.(check int) "two active replacements" 2 (List.length active) ;
  Alcotest.(check int) "total audit rows" 4 (row_count conn) ;
  Alcotest.(check bool)
    "old rows are inactive"
    false
    (List.exists (fun row -> row.M.id = first.id || row.id = second.id) active)

let test_replace_for_law_rolls_back_on_failure () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law = create_law ctx ~scope:global "law" in
  let original =
    ok_or_fail (create_metadata ctx ~scope:global ~law_id:law.id ())
  in
  let duplicate_rows =
    [
      metadata_template ~role_kind:M.Primary ();
      metadata_template ~role_kind:M.Primary ();
    ]
  in
  (match
     M.replace_for_law ~ctx ~scope:global ~law_id:law.id ~rows:duplicate_rows ()
   with
  | Ok _ -> Alcotest.fail "duplicate replacement rows should fail"
  | Error _ -> ()) ;
  let active = ok_or_fail (M.list_for_law ~ctx ~scope:global ~law_id:law.id) in
  Alcotest.(check int) "one active row after rollback" 1 (List.length active) ;
  Alcotest.(check int) "original row preserved" original.id (List.hd active).id

let test_duplicate_active_metadata_concurrent () =
  with_file_db @@ fun conn_a conn_b ->
  set_busy_timeout conn_a ;
  set_busy_timeout conn_b ;
  let ctx_a = init_ctx conn_a in
  let ctx_b = init_ctx conn_b in
  let law = create_law ctx_a ~scope:global "concurrent law" in
  let a, b =
    concurrent_pair
      (fun () -> create_metadata ctx_a ~scope:global ~law_id:law.id ())
      (fun () -> create_metadata ctx_b ~scope:global ~law_id:law.id ())
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
        | Error "duplicate active normative metadata" -> acc + 1
        | Error msg -> Alcotest.failf "unexpected metadata error: %s" msg)
      0
      [a; b]
  in
  Alcotest.(check int) "one metadata insert succeeds" 1 ok_count ;
  Alcotest.(check int) "one metadata insert duplicates" 1 duplicate_count ;
  Alcotest.(check int) "one active metadata row" 1 (active_count conn_a)

let test_hard_delete_law_cascades_metadata () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let law = create_law ctx ~scope:global "cascade law" in
  ignore (ok_or_fail (create_metadata ctx ~scope:global ~law_id:law.id ())) ;
  exec_sql conn (Printf.sprintf "DELETE FROM laws WHERE id = %d" law.id) ;
  Alcotest.(check int) "metadata cascaded" 0 (row_count conn)

let () =
  Alcotest.run
    "chamallaw normative metadata store"
    [
      ( "law_normative_metadata_store",
        [
          Alcotest.test_case
            "create metadata attaches to existing law"
            `Quick
            test_create_metadata_attaches_to_existing_law;
          Alcotest.test_case
            "cross-scope metadata global actor writes any"
            `Quick
            test_cross_scope_metadata_global_actor_writes_any;
          Alcotest.test_case
            "cross-scope metadata org actor writes project target"
            `Quick
            test_cross_scope_metadata_org_actor_writes_project_target;
          Alcotest.test_case
            "cross-scope metadata project actor blocked upward"
            `Quick
            test_cross_scope_metadata_project_actor_blocked_upward;
          Alcotest.test_case
            "cross-scope metadata project actor blocked sideways"
            `Quick
            test_cross_scope_metadata_project_actor_blocked_sideways;
          Alcotest.test_case
            "cross-scope metadata replace for law authorized"
            `Quick
            test_cross_scope_metadata_replace_for_law_authorized;
          Alcotest.test_case
            "create metadata for missing law rejected by FK"
            `Quick
            test_create_metadata_for_missing_law_rejected_by_fk;
          Alcotest.test_case
            "force enum accepts exception"
            `Quick
            test_force_enum_accepts_exception;
          Alcotest.test_case
            "force enum rejects unknown slug"
            `Quick
            test_force_enum_rejects_unknown_slug;
          Alcotest.test_case
            "unique active per role kind"
            `Quick
            test_unique_active_per_role_kind;
          Alcotest.test_case
            "replace for law atomic swap"
            `Quick
            test_replace_for_law_atomic_swap;
          Alcotest.test_case
            "replace for law rolls back on failure"
            `Quick
            test_replace_for_law_rolls_back_on_failure;
          Alcotest.test_case
            "duplicate active metadata concurrent"
            `Quick
            test_duplicate_active_metadata_concurrent;
          Alcotest.test_case
            "hard delete law cascades metadata"
            `Quick
            test_hard_delete_law_cascades_metadata;
        ] );
    ]
