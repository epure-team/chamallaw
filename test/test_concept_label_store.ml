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

let create_concept ctx =
  let scheme =
    ok_or_fail
      (Concept_scheme_store.create
         ~ctx
         ~scope:global
         ~slug:"label-scheme"
         ~display_name:"Label Scheme"
         ~provenance:User
         ())
  in
  ok_or_fail
    (Concept_store.create
       ~ctx
       ~scope:global
       ~scheme_id:scheme.id
       ~slug:"label-concept"
       ~provenance:User
       ())

let test_label_crud_all_kinds () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let concept = create_concept ctx in
  let kinds =
    Concept_label_store.
      [Label_preferred; Label_alternate; Label_hidden; Label_deprecated]
  in
  List.iteri
    (fun i kind ->
      ignore
        (ok_or_fail
           (Concept_label_store.create
              ~ctx
              ~scope:global
              ~concept_id:concept.id
              ~text:(Printf.sprintf "label-%d" i)
              ~kind)))
    kinds ;
  let labels =
    ok_or_fail
      (Concept_label_store.list_by_concept
         ~ctx
         ~scope:global
         ~concept_id:concept.id)
  in
  Alcotest.(check int) "all label kinds persisted" 4 (List.length labels)

let test_staleness_transitions () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let concept = create_concept ctx in
  let label =
    ok_or_fail
      (Concept_label_store.create
         ~ctx
         ~scope:global
         ~concept_id:concept.id
         ~text:"canonical"
         ~kind:Label_preferred)
  in
  Alcotest.(check bool) "initial active" true (label.staleness_status = Active) ;
  Alcotest.(check bool)
    "no last_marked_at"
    true
    (Option.is_none label.last_marked_at) ;
  ok_or_fail
    (Concept_label_store.mark_stale
       ~ctx
       ~scope:global
       ~label_id:label.id
       ~status:Deprecated) ;
  let after_deprecated =
    match
      ok_or_fail
        (Concept_label_store.list_by_concept
           ~ctx
           ~scope:global
           ~concept_id:concept.id)
    with
    | row :: _ -> row
    | [] -> Alcotest.fail "expected label after deprecated transition"
  in
  Alcotest.(check bool)
    "deprecated"
    true
    (after_deprecated.staleness_status = Deprecated) ;
  Alcotest.(check bool)
    "marked timestamp"
    true
    (Option.is_some after_deprecated.last_marked_at) ;
  ok_or_fail
    (Concept_label_store.mark_stale
       ~ctx
       ~scope:global
       ~label_id:label.id
       ~status:Stale) ;
  let after_stale =
    match
      ok_or_fail
        (Concept_label_store.list_by_concept
           ~ctx
           ~scope:global
           ~concept_id:concept.id)
    with
    | row :: _ -> row
    | [] -> Alcotest.fail "expected label after stale transition"
  in
  Alcotest.(check bool) "stale" true (after_stale.staleness_status = Stale)

let () =
  Alcotest.run
    "concept label store"
    [
      ( "concept_label_store",
        [
          Alcotest.test_case
            "label CRUD all kinds"
            `Quick
            test_label_crud_all_kinds;
          Alcotest.test_case
            "staleness transitions"
            `Quick
            test_staleness_transitions;
        ] );
    ]
