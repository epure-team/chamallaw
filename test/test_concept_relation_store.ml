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

let with_file_db f =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let path = Filename.temp_file "chamallaw_relation_" ".db" in
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
      let conn_a = connect () in
      let conn_b = connect () in
      f conn_a conn_b)

let set_busy_timeout (module Db : Caqti_eio.CONNECTION) =
  let req =
    Caqti_request.Infix.(Caqti_type.unit ->? Caqti_type.int)
      "PRAGMA busy_timeout = 5000"
  in
  match Db.find_opt req () with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "busy_timeout failed: %s" (Caqti_error.show e)

let init_ctx conn =
  match ok_or_fail (init conn) with
  | Ready ctx -> ctx
  | Requires_migration {apply; _} -> ok_or_fail (apply ())

let global = Authorized_scope.Global {actor_id = None}

let create_scheme ctx =
  ok_or_fail
    (Concept_scheme_store.create
       ~ctx
       ~scope:global
       ~slug:"relation-scheme"
       ~display_name:"Relation Scheme"
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

let relation_type_id ctx slug =
  match
    List.find_opt
      (fun row -> String.equal row.Concept_relation_store.slug slug)
      (ok_or_fail (Concept_relation_store.list_relation_types ~ctx))
  with
  | Some row -> row.id
  | None -> Alcotest.failf "missing relation type %s" slug

let contains_substring haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop index =
    if needle_len = 0 then true
    else if index + needle_len > haystack_len then false
    else if String.sub haystack index needle_len = needle then true
    else loop (index + 1)
  in
  loop 0

let assert_no_nested_tx_error = function
  | Ok _ -> ()
  | Error msg ->
      Alcotest.(check bool)
        "no nested transaction error"
        false
        (contains_substring msg "cannot start a transaction")

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
  | _ -> Alcotest.fail "concurrent relation fiber did not record a result"

let test_hierarchical_cycle_rejected () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  ok_or_fail (Builtin_vocabulary_seed.run ~ctx) ;
  let scheme = create_scheme ctx in
  let a = create_concept ctx scheme "a" in
  let b = create_concept ctx scheme "b" in
  let c = create_concept ctx scheme "c" in
  let broader_id = relation_type_id ctx "broader" in
  ignore
    (ok_or_fail
       (Concept_relation_store.create_relation
          ~ctx
          ~scope:global
          ~from_concept_id:a.id
          ~to_concept_id:b.id
          ~relation_type_id:broader_id)) ;
  ignore
    (ok_or_fail
       (Concept_relation_store.create_relation
          ~ctx
          ~scope:global
          ~from_concept_id:b.id
          ~to_concept_id:c.id
          ~relation_type_id:broader_id)) ;
  (match
     Concept_relation_store.create_relation
       ~ctx
       ~scope:global
       ~from_concept_id:c.id
       ~to_concept_id:a.id
       ~relation_type_id:broader_id
   with
  | Ok _ -> Alcotest.fail "cycle insert should fail"
  | Error msg ->
      Alcotest.(check bool)
        "cycle error"
        true
        (String.starts_with ~prefix:"Cycle detected" msg)) ;
  let outgoing =
    ok_or_fail
      (Concept_relation_store.list_relations_by_concept
         ~ctx
         ~scope:global
         ~concept_id:c.id)
  in
  Alcotest.(check int) "cycle row rolled back" 0 (List.length outgoing)

let test_traversal_enabled_flag () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  ok_or_fail (Builtin_vocabulary_seed.run ~ctx) ;
  let types = ok_or_fail (Concept_relation_store.list_relation_types ~ctx) in
  let find_type slug =
    match
      List.find_opt
        (fun row -> String.equal row.Concept_relation_store.slug slug)
        types
    with
    | Some row -> row
    | None -> Alcotest.failf "missing relation type %s" slug
  in
  let broader = find_type "broader" in
  let related = find_type "related" in
  Alcotest.(check bool) "broader traversal" true broader.is_traversal_enabled ;
  Alcotest.(check bool)
    "related no traversal"
    false
    related.is_traversal_enabled ;
  let scheme = create_scheme ctx in
  let a = create_concept ctx scheme "related-a" in
  let b = create_concept ctx scheme "related-b" in
  ignore
    (ok_or_fail
       (Concept_relation_store.create_relation
          ~ctx
          ~scope:global
          ~from_concept_id:a.id
          ~to_concept_id:b.id
          ~relation_type_id:related.id))

let test_duplicate_active_relation_rejected () =
  with_memory_db @@ fun conn ->
  let ctx = init_ctx conn in
  ok_or_fail (Builtin_vocabulary_seed.run ~ctx) ;
  let scheme = create_scheme ctx in
  let a = create_concept ctx scheme "duplicate-a" in
  let b = create_concept ctx scheme "duplicate-b" in
  let broader_id = relation_type_id ctx "broader" in
  ignore
    (ok_or_fail
       (Concept_relation_store.create_relation
          ~ctx
          ~scope:global
          ~from_concept_id:a.id
          ~to_concept_id:b.id
          ~relation_type_id:broader_id)) ;
  match
    Concept_relation_store.create_relation
      ~ctx
      ~scope:global
      ~from_concept_id:a.id
      ~to_concept_id:b.id
      ~relation_type_id:broader_id
  with
  | Ok _ -> Alcotest.fail "duplicate active relation should be rejected"
  | Error msg ->
      Alcotest.(check string)
        "duplicate message"
        "duplicate active relation"
        msg

let test_concurrent_relation_writers_serialize () =
  with_file_db @@ fun conn_a conn_b ->
  set_busy_timeout conn_a ;
  set_busy_timeout conn_b ;
  let ctx_a = init_ctx conn_a in
  let ctx_b = init_ctx conn_b in
  ok_or_fail (Builtin_vocabulary_seed.run ~ctx:ctx_a) ;
  let scheme = create_scheme ctx_a in
  let a = create_concept ctx_a scheme "concurrent-a" in
  let b = create_concept ctx_a scheme "concurrent-b" in
  let c = create_concept ctx_a scheme "concurrent-c" in
  let d = create_concept ctx_a scheme "concurrent-d" in
  let e = create_concept ctx_a scheme "concurrent-e" in
  let broader_id = relation_type_id ctx_a "broader" in
  let create ctx from_concept_id to_concept_id =
    Concept_relation_store.create_relation
      ~ctx
      ~scope:global
      ~from_concept_id
      ~to_concept_id
      ~relation_type_id:broader_id
  in
  let non_conflicting_a, non_conflicting_b =
    concurrent_pair
      (fun () -> create ctx_a a.id b.id)
      (fun () -> create ctx_b b.id c.id)
  in
  assert_no_nested_tx_error non_conflicting_a ;
  assert_no_nested_tx_error non_conflicting_b ;
  ignore (ok_or_fail non_conflicting_a) ;
  ignore (ok_or_fail non_conflicting_b) ;
  let conflicting_a, conflicting_b =
    concurrent_pair
      (fun () -> create ctx_a d.id e.id)
      (fun () -> create ctx_b e.id d.id)
  in
  assert_no_nested_tx_error conflicting_a ;
  assert_no_nested_tx_error conflicting_b ;
  let ok_count =
    List.fold_left
      (fun acc -> function Ok _ -> acc + 1 | Error _ -> acc)
      0
      [conflicting_a; conflicting_b]
  in
  let cycle_count =
    List.fold_left
      (fun acc -> function
        | Ok _ -> acc
        | Error msg when String.starts_with ~prefix:"Cycle detected" msg ->
            acc + 1
        | Error msg -> Alcotest.failf "unexpected relation error: %s" msg)
      0
      [conflicting_a; conflicting_b]
  in
  Alcotest.(check int) "one conflicting insert succeeds" 1 ok_count ;
  Alcotest.(check int) "one conflicting insert cycles" 1 cycle_count

let () =
  Alcotest.run
    "concept relation store"
    [
      ( "concept_relation_store",
        [
          Alcotest.test_case
            "hierarchical cycle rejected"
            `Quick
            test_hierarchical_cycle_rejected;
          Alcotest.test_case
            "traversal-enabled flag persisted"
            `Quick
            test_traversal_enabled_flag;
          Alcotest.test_case
            "duplicate active relation rejected"
            `Quick
            test_duplicate_active_relation_rejected;
          Alcotest.test_case
            "concurrent relation writers serialize"
            `Quick
            test_concurrent_relation_writers_serialize;
        ] );
    ]
