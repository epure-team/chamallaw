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

let project =
  Authorized_scope.Project {project_id = 1; org_id = Some 1; actor_id = None}

let relation_type_id ctx slug =
  match
    List.find_opt
      (fun row -> String.equal row.Concept_relation_store.slug slug)
      (ok_or_fail (Concept_relation_store.list_relation_types ~ctx))
  with
  | Some row -> row.id
  | None -> Alcotest.failf "missing relation type %s" slug

let test_full_vocabulary_lifecycle () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  ok_or_fail (Builtin_vocabulary_seed.run ~ctx) ;
  let scheme =
    ok_or_fail
      (Concept_scheme_store.create
         ~ctx
         ~scope:project
         ~slug:"user-proj-scheme"
         ~display_name:"User Project Scheme"
         ~provenance:User
         ())
  in
  let concept =
    ok_or_fail
      (Concept_store.create
         ~ctx
         ~scope:project
         ~scheme_id:scheme.id
         ~slug:"user-concept"
         ~definition:"User integration concept"
         ~provenance:User
         ())
  in
  let second_concept =
    ok_or_fail
      (Concept_store.create
         ~ctx
         ~scope:project
         ~scheme_id:scheme.id
         ~slug:"second-user-concept"
         ~definition:"Second user integration concept"
         ~provenance:User
         ())
  in
  let label =
    ok_or_fail
      (Concept_label_store.create
         ~ctx
         ~scope:project
         ~concept_id:concept.id
         ~text:"user-label"
         ~kind:Label_preferred)
  in
  let related_id = relation_type_id ctx "related" in
  ignore
    (ok_or_fail
       (Concept_relation_store.create_relation
          ~ctx
          ~scope:project
          ~from_concept_id:concept.id
          ~to_concept_id:second_concept.id
          ~relation_type_id:related_id)) ;
  let search_results =
    ok_or_fail
      (Concept_search_store.search
         ~ctx
         ~scope:project
         ~query:"user-label"
         ~limit:10)
  in
  Alcotest.(check bool)
    "search finds user concept"
    true
    (List.exists
       (fun row -> row.Concept_search_store.concept_id = concept.id)
       search_results) ;
  ok_or_fail (Builtin_vocabulary_seed.run ~ctx) ;
  let user_scheme =
    match
      ok_or_fail
        (Concept_scheme_store.get_by_slug
           ~ctx
           ~scope:project
           ~slug:"user-proj-scheme")
    with
    | Some row -> row
    | None -> Alcotest.fail "user scheme missing after re-seed"
  in
  let labels =
    ok_or_fail
      (Concept_label_store.list_by_concept
         ~ctx
         ~scope:project
         ~concept_id:concept.id)
  in
  Alcotest.(check int) "user scheme preserved" scheme.id user_scheme.id ;
  Alcotest.(check bool)
    "user label preserved"
    true
    (List.exists (fun row -> row.Concept_label_store.id = label.id) labels)

let () =
  Alcotest.run
    "vocabulary integration"
    [
      ( "vocabulary_integration",
        [
          Alcotest.test_case
            "full vocabulary lifecycle"
            `Quick
            test_full_vocabulary_lifecycle;
        ] );
    ]
