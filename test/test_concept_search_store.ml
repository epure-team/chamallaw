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

let create_scheme ctx scope slug =
  ok_or_fail
    (Concept_scheme_store.create
       ~ctx
       ~scope
       ~slug
       ~display_name:slug
       ~provenance:User
       ())

let create_concept ctx scope scheme slug definition =
  ok_or_fail
    (Concept_store.create
       ~ctx
       ~scope
       ~scheme_id:scheme.Concept_scheme_store.id
       ~slug
       ~definition
       ~provenance:User
       ())

let add_label ctx scope concept text =
  ignore
    (ok_or_fail
       (Concept_label_store.create
          ~ctx
          ~scope
          ~concept_id:concept.Concept_store.id
          ~text
          ~kind:Label_preferred))

let sorted_by_rank rows =
  let rec loop = function
    | [] | [_] -> true
    | a :: (b :: _ as rest) ->
        a.Concept_search_store.rank <= b.rank && loop rest
  in
  loop rows

let result_ids rows =
  List.map (fun row -> row.Concept_search_store.concept_id) rows

let test_fts5_search () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  ok_or_fail (Builtin_vocabulary_seed.run ~ctx) ;
  let scheme = create_scheme ctx global "search-scheme" in
  let custom =
    create_concept ctx global scheme "custom" "Custom design pattern"
  in
  add_label ctx global custom "pattern" ;
  let design_scheme =
    match
      ok_or_fail
        (Concept_scheme_store.get_by_slug
           ~ctx
           ~scope:global
           ~slug:"epure-builtin-phases")
    with
    | Some row -> row
    | None -> Alcotest.fail "missing built-in phases scheme"
  in
  let design =
    match
      ok_or_fail
        (Concept_store.get_by_scheme_and_slug
           ~ctx
           ~scope:global
           ~scheme_id:design_scheme.id
           ~slug:"design")
    with
    | Some row -> row
    | None -> Alcotest.fail "missing built-in design concept"
  in
  let results =
    ok_or_fail
      (Concept_search_store.search ~ctx ~scope:global ~query:"design" ~limit:10)
  in
  let ids = result_ids results in
  Alcotest.(check bool) "built-in design result" true (List.mem design.id ids) ;
  Alcotest.(check bool) "custom definition result" true (List.mem custom.id ids) ;
  Alcotest.(check bool) "BM25 ordered" true (sorted_by_rank results)

let test_fts5_scope_filtering () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let global_scheme = create_scheme ctx global "global-search" in
  let org_scheme = create_scheme ctx org "org-search" in
  let project_scheme = create_scheme ctx project "project-search" in
  let g = create_concept ctx global global_scheme "g" "scopefilter global" in
  let o = create_concept ctx org org_scheme "o" "scopefilter organization" in
  let p = create_concept ctx project project_scheme "p" "scopefilter project" in
  add_label ctx global g "scopefilter" ;
  add_label ctx org o "scopefilter" ;
  add_label ctx project p "scopefilter" ;
  let project_ids =
    result_ids
      (ok_or_fail
         (Concept_search_store.search
            ~ctx
            ~scope:project
            ~query:"scopefilter"
            ~limit:10))
  in
  Alcotest.(check bool) "project sees global" true (List.mem g.id project_ids) ;
  Alcotest.(check bool) "project sees org" true (List.mem o.id project_ids) ;
  Alcotest.(check bool) "project sees project" true (List.mem p.id project_ids) ;
  let org_ids =
    result_ids
      (ok_or_fail
         (Concept_search_store.search
            ~ctx
            ~scope:org
            ~query:"scopefilter"
            ~limit:10))
  in
  Alcotest.(check bool) "org sees global" true (List.mem g.id org_ids) ;
  Alcotest.(check bool) "org sees org" true (List.mem o.id org_ids) ;
  Alcotest.(check bool) "org hides project" false (List.mem p.id org_ids)

let test_blank_fts5_queries_return_empty () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let assert_empty query =
    Alcotest.(check int)
      (Printf.sprintf "blank query %S" query)
      0
      (List.length
         (ok_or_fail
            (Concept_search_store.search ~ctx ~scope:global ~query ~limit:10)))
  in
  assert_empty "" ;
  assert_empty "   \t\n  "

let test_fts5_edge_inputs_are_clean () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  let edge_queries =
    ["embedded \" quote"; "nul\000byte"; "AND"; "OR"; "NEAR"; "*"]
  in
  List.iter
    (fun query ->
      try
        match
          Concept_search_store.search ~ctx ~scope:global ~query ~limit:10
        with
        | Ok rows ->
            Alcotest.(check int)
              (Printf.sprintf "edge query %S" query)
              0
              (List.length rows)
        | Error _ -> ()
      with exn ->
        Alcotest.failf "search raised for %S: %s" query (Printexc.to_string exn))
    edge_queries

let () =
  Alcotest.run
    "concept search store"
    [
      ( "concept_search_store",
        [
          Alcotest.test_case "FTS5 search" `Quick test_fts5_search;
          Alcotest.test_case
            "FTS5 scope filtering"
            `Quick
            test_fts5_scope_filtering;
          Alcotest.test_case
            "blank FTS5 queries return empty"
            `Quick
            test_blank_fts5_queries_return_empty;
          Alcotest.test_case
            "FTS5 edge inputs are clean"
            `Quick
            test_fts5_edge_inputs_are_clean;
        ] );
    ]
