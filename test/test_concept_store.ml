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

let create_scheme ctx slug =
  ok_or_fail
    (Concept_scheme_store.create
       ~ctx
       ~scope:global
       ~slug
       ~display_name:slug
       ~provenance:User
       ())

let create_concept ctx scheme slug =
  Concept_store.create
    ~ctx
    ~scope:global
    ~scheme_id:scheme.Concept_scheme_store.id
    ~slug
    ~provenance:User
    ()

let exec_sql_result (module Db : Caqti_eio.CONNECTION) sql =
  let req = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) sql in
  Db.exec req ()

let test_cross_scheme_same_slug_coexists () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let scheme_a = create_scheme ctx "scheme-a" in
  let scheme_b = create_scheme ctx "scheme-b" in
  let concept_a = ok_or_fail (create_concept ctx scheme_a "design") in
  let concept_b = ok_or_fail (create_concept ctx scheme_b "design") in
  Alcotest.(check bool) "different ids" true (concept_a.id <> concept_b.id)

let test_same_scheme_duplicate_rejected () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let scheme = create_scheme ctx "scheme-unique" in
  ignore (ok_or_fail (create_concept ctx scheme "foo")) ;
  (match create_concept ctx scheme "foo" with
  | Ok _ -> Alcotest.fail "duplicate concept slug in same scheme should fail"
  | Error _ -> ()) ;
  let rows =
    ok_or_fail
      (Concept_store.list_visible_by_scheme
         ~ctx
         ~scope:global
         ~scheme_id:scheme.id)
  in
  Alcotest.(check int) "only first concept remains" 1 (List.length rows)

let test_concept_scope_check_rejects_mismatched_ids () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let scheme = create_scheme ctx "scope-check-scheme" in
  match
    exec_sql_result
      conn
      (Printf.sprintf
         {|INSERT INTO concepts
           (scheme_id, slug, provenance, scope_kind, project_id)
           VALUES (%d, 'invalid-global-concept', 'user', 'global', 7)|}
         scheme.id)
  with
  | Ok () -> Alcotest.fail "global concept with project_id should fail CHECK"
  | Error _ -> ()

let () =
  Alcotest.run
    "concept store"
    [
      ( "concept_store",
        [
          Alcotest.test_case
            "cross-scheme same slug coexists"
            `Quick
            test_cross_scheme_same_slug_coexists;
          Alcotest.test_case
            "same-scheme duplicate rejected"
            `Quick
            test_same_scheme_duplicate_rejected;
          Alcotest.test_case
            "concept scope CHECK rejects mismatched ids"
            `Quick
            test_concept_scope_check_rejects_mismatched_ids;
        ] );
    ]
