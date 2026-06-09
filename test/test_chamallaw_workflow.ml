(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let write_file path content =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let contains haystack needle =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  let rec loop i =
    nlen = 0
    || i + nlen <= hlen
       && (String.sub haystack i nlen = needle || loop (i + 1))
  in
  loop 0

let check_contains source needle =
  Alcotest.(check bool)
    ("example contains " ^ needle)
    true
    (contains source needle)

let check_absent source needle =
  Alcotest.(check bool)
    ("example omits " ^ needle)
    false
    (contains source needle)

let ok_or_fail = function Ok value -> value | Error msg -> Alcotest.fail msg

let remove_if_exists path = try Sys.remove path with Sys_error _ -> ()

let create_temp_dir prefix =
  let marker = Filename.temp_file prefix "" in
  remove_if_exists marker ;
  Unix.mkdir marker 0o700 ;
  marker

let cleanup_project_dir dir =
  List.iter
    (fun name -> remove_if_exists (Filename.concat dir name))
    [
      "curator-context.json";
      "curator-output.log";
      "curator-output.fixture.json";
      "chamallaw-demo.db";
      "chamallaw-demo.db-journal";
      "chamallaw-demo.db-shm";
      "chamallaw-demo.db-wal";
    ] ;
  try Unix.rmdir dir with Unix.Unix_error _ -> ()

type command_result = {
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

let run_capture exe args =
  let stdout_path = Filename.temp_file "chamallaw_example_stdout_" ".log" in
  let stderr_path = Filename.temp_file "chamallaw_example_stderr_" ".log" in
  Fun.protect
    ~finally:(fun () ->
      remove_if_exists stdout_path ;
      remove_if_exists stderr_path)
    (fun () ->
      let stdout_fd =
        Unix.openfile stdout_path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
      in
      let stderr_fd =
        Unix.openfile stderr_path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
      in
      let argv = Array.of_list (exe :: args) in
      let pid = Unix.create_process exe argv Unix.stdin stdout_fd stderr_fd in
      List.iter Unix.close [stdout_fd; stderr_fd] ;
      let _pid, status = Unix.waitpid [] pid in
      {status; stdout = read_file stdout_path; stderr = read_file stderr_path})

let check_exit_code expected result =
  match result.status with
  | Unix.WEXITED code -> Alcotest.(check int) "exit code" expected code
  | Unix.WSIGNALED signal -> Alcotest.failf "process signaled: %d" signal
  | Unix.WSTOPPED signal -> Alcotest.failf "process stopped: %d" signal

let example_exe () =
  match Sys.getenv_opt "CHAMALLAW_EXAMPLE_EXE" with
  | Some path -> path
  | None -> Alcotest.fail "missing CHAMALLAW_EXAMPLE_EXE"

let fixture_json =
  {|{
  "summary": "Fixture curator summary",
  "concept_suggestions": [
    {
      "scheme_slug": "workflow-governance",
      "scheme_display_name": "Workflow Governance",
      "concept_slug": "curator-fixture",
      "preferred_label": "curator fixture",
      "definition": "A deterministic local curator suggestion used by the example test.",
      "scope_note": "Exercises local parsing and application without calling a live backend."
    }
  ],
  "law_suggestions": [
    {
      "statement": "Fixture curator laws must be applied through Chamallaw stores.",
      "rationale": "The example should prove the local DB workflow independently of live backend availability.",
      "concept_links": [
        {
          "scheme_slug": "workflow-governance",
          "concept_slug": "curator-fixture",
          "role": "primary-subject"
        }
      ]
    }
  ],
  "link_suggestions": [
    {
      "law_statement": "Curated ontology suggestions must be logged before being applied to the local database.",
      "scheme_slug": "workflow-governance",
      "concept_slug": "curator-fixture",
      "role": "concern",
      "reason": "Fixture confirms parsed links can target manually seeded laws."
    }
  ]
}|}

let test_help_executes_without_live_backend () =
  let result = run_capture (example_exe ()) ["--help"] in
  check_exit_code 0 result ;
  Alcotest.(check bool)
    "help mentions local curator fixture option"
    true
    (contains result.stderr "--curator-output-json") ;
  Alcotest.(check bool)
    "help mentions Cabal debug option"
    true
    (contains result.stderr "--debug-cabal") ;
  Alcotest.(check bool)
    "help mentions backend option"
    true
    (contains result.stderr "--backend BACKEND") ;
  Alcotest.(check bool)
    "help mentions supported codex backend"
    true
    (contains result.stderr "codex")

let init_ctx conn =
  match ok_or_fail (Chamallaw.init conn) with
  | Chamallaw.Ready ctx -> ctx
  | Chamallaw.Requires_migration {apply; _} -> ok_or_fail (apply ())

let connect_file_db ~sw ~stdenv path =
  match
    Caqti_eio_unix.connect ~sw ~stdenv (Uri.of_string ("sqlite3:" ^ path))
  with
  | Error err -> Alcotest.failf "DB connect failed: %s" (Caqti_error.show err)
  | Ok conn -> conn

let assert_db_contains_fixture db_path =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let conn = connect_file_db ~sw ~stdenv db_path in
  let ctx = init_ctx conn in
  let scope =
    Chamallaw.Authorized_scope.Project
      {project_id = 89; org_id = Some 1; actor_id = Some "chamallaw-example"}
  in
  let scheme =
    match
      ok_or_fail
        (Chamallaw.Concept_scheme_store.get_by_slug
           ~ctx
           ~scope
           ~slug:"workflow-governance")
    with
    | Some row -> row
    | None -> Alcotest.fail "missing workflow-governance scheme"
  in
  let concept =
    match
      ok_or_fail
        (Chamallaw.Concept_store.get_by_scheme_and_slug
           ~ctx
           ~scope
           ~scheme_id:scheme.Chamallaw.Concept_scheme_store.id
           ~slug:"curator-fixture")
    with
    | Some row -> row
    | None -> Alcotest.fail "missing curator-fixture concept"
  in
  let laws = ok_or_fail (Chamallaw.Law_store.list_visible ~ctx ~scope) in
  let fixture_law =
    match
      List.find_opt
        (fun row ->
          row.Chamallaw.Law_store.statement
          = "Fixture curator laws must be applied through Chamallaw stores.")
        laws
    with
    | Some row -> row
    | None -> Alcotest.fail "missing fixture law"
  in
  let links =
    ok_or_fail
      (Chamallaw.Law_concept_links_store.list_for_law
         ~ctx
         ~scope
         ~law_id:fixture_law.Chamallaw.Law_store.id)
  in
  Alcotest.(check bool)
    "fixture law links to fixture concept"
    true
    (List.exists
       (fun row ->
         row.Chamallaw.Law_concept_links_store.concept_id
         = concept.Chamallaw.Concept_store.id)
       links)

let test_local_fixture_workflow_executes_db_flow () =
  let project_dir = create_temp_dir "chamallaw_example_project_" in
  Fun.protect
    ~finally:(fun () -> cleanup_project_dir project_dir)
    (fun () ->
      let db_path = Filename.concat project_dir "chamallaw-demo.db" in
      let log_path = Filename.concat project_dir "curator-output.log" in
      let fixture_path =
        Filename.concat project_dir "curator-output.fixture.json"
      in
      write_file fixture_path fixture_json ;
      let result =
        run_capture
          (example_exe ())
          [
            "--project-dir";
            project_dir;
            "--db";
            db_path;
            "--log";
            log_path;
            "--curator-output-json";
            fixture_path;
          ]
      in
      check_exit_code 0 result ;
      Alcotest.(check bool)
        "stdout prints DB path"
        true
        (contains result.stdout ("DB path: " ^ db_path)) ;
      Alcotest.(check bool) "DB file created" true (Sys.file_exists db_path) ;
      Alcotest.(check bool) "log file created" true (Sys.file_exists log_path) ;
      Alcotest.(check bool)
        "context file created"
        true
        (Sys.file_exists (Filename.concat project_dir "curator-context.json")) ;
      let log = read_file log_path in
      List.iter
        (fun needle -> Alcotest.(check bool) needle true (contains log needle))
        [
          "DB path: " ^ db_path;
          "Project work dir: " ^ project_dir;
          "Curator log path: " ^ log_path;
          "Curator context file: ";
          "Curator fixture file: " ^ fixture_path;
          "## Curator raw output";
          "Fixture curator summary";
          "ensured curator concept workflow-governance:curator-fixture";
          "Fixture curator laws must be applied through Chamallaw stores.";
          "## Final concepts table";
          "## Final laws table";
          "## Queries and results";
        ] ;
      assert_db_contains_fixture db_path)

let test_debug_cabal_flag_accepts_fixture_workflow () =
  let project_dir = create_temp_dir "chamallaw_example_project_" in
  Fun.protect
    ~finally:(fun () -> cleanup_project_dir project_dir)
    (fun () ->
      let db_path = Filename.concat project_dir "chamallaw-demo.db" in
      let log_path = Filename.concat project_dir "curator-output.log" in
      let fixture_path =
        Filename.concat project_dir "curator-output.fixture.json"
      in
      write_file fixture_path fixture_json ;
      let result =
        run_capture
          (example_exe ())
          [
            "--debug-cabal";
            "--project-dir";
            project_dir;
            "--db";
            db_path;
            "--log";
            log_path;
            "--curator-output-json";
            fixture_path;
          ]
      in
      check_exit_code 0 result ;
      Alcotest.(check bool)
        "debug flag run still prints DB path"
        true
        (contains result.stdout ("DB path: " ^ db_path)))

let test_codex_backend_flag_accepts_fixture_workflow () =
  let project_dir = create_temp_dir "chamallaw_example_project_" in
  Fun.protect
    ~finally:(fun () -> cleanup_project_dir project_dir)
    (fun () ->
      let db_path = Filename.concat project_dir "chamallaw-demo.db" in
      let log_path = Filename.concat project_dir "curator-output.log" in
      let fixture_path =
        Filename.concat project_dir "curator-output.fixture.json"
      in
      write_file fixture_path fixture_json ;
      let result =
        run_capture
          (example_exe ())
          [
            "--backend";
            "codex";
            "--debug-cabal";
            "--project-dir";
            project_dir;
            "--db";
            db_path;
            "--log";
            log_path;
            "--curator-output-json";
            fixture_path;
          ]
      in
      check_exit_code 0 result ;
      Alcotest.(check bool)
        "codex fixture run prints selected backend"
        true
        (contains result.stdout "Selected backend: codex") ;
      Alcotest.(check bool)
        "codex fixture run still prints DB path"
        true
        (contains result.stdout ("DB path: " ^ db_path)))

let test_unknown_backend_is_rejected () =
  let result = run_capture (example_exe ()) ["--backend"; "not-real"] in
  check_exit_code 2 result ;
  Alcotest.(check bool)
    "unknown backend error lists supported backends"
    true
    (contains result.stderr "unsupported backend \"not-real\"") ;
  Alcotest.(check bool)
    "unknown backend error mentions codex"
    true
    (contains result.stderr "codex")

let run_fixture_failure fixture_content =
  let project_dir = create_temp_dir "chamallaw_example_project_" in
  Fun.protect
    ~finally:(fun () -> cleanup_project_dir project_dir)
    (fun () ->
      let db_path = Filename.concat project_dir "chamallaw-demo.db" in
      let log_path = Filename.concat project_dir "curator-output.log" in
      let fixture_path =
        Filename.concat project_dir "curator-output.fixture.json"
      in
      write_file fixture_path fixture_content ;
      let result =
        run_capture
          (example_exe ())
          [
            "--project-dir";
            project_dir;
            "--db";
            db_path;
            "--log";
            log_path;
            "--curator-output-json";
            fixture_path;
          ]
      in
      check_exit_code 1 result ;
      Alcotest.(check bool) "log file created" true (Sys.file_exists log_path) ;
      (fixture_path, result, read_file log_path))

let test_invalid_curator_json_preserves_raw_output () =
  let raw = "{ invalid-json sentinel: malformed curator output" in
  let fixture_path, result, log = run_fixture_failure raw in
  List.iter
    (fun needle -> Alcotest.(check bool) needle true (contains log needle))
    [
      "## Curator raw output";
      raw;
      "## Curator error";
      "curator output file " ^ fixture_path;
      "was not valid JSON";
    ] ;
  List.iter
    (fun needle ->
      Alcotest.(check bool) needle true (contains result.stderr needle))
    ["Error:"; raw; "curator output file " ^ fixture_path; "was not valid JSON"]

let test_contract_invalid_curator_json_preserves_raw_output () =
  let raw =
    {|{"summary":"contract invalid sentinel","concept_suggestions":"not-an-array","law_suggestions":[],"link_suggestions":[]}|}
  in
  let fixture_path, result, log = run_fixture_failure raw in
  List.iter
    (fun needle -> Alcotest.(check bool) needle true (contains log needle))
    [
      "## Curator raw output";
      "contract invalid sentinel";
      "## Curator error";
      "curator output file " ^ fixture_path;
      "did not match the curator output contract";
      "concept_suggestions";
    ] ;
  List.iter
    (fun needle ->
      Alcotest.(check bool) needle true (contains result.stderr needle))
    [
      "Error:";
      "contract invalid sentinel";
      "curator output file " ^ fixture_path;
      "did not match the curator output contract";
      "concept_suggestions";
    ]

let test_example_uses_real_cabal_backend_paths () =
  let source_path =
    match Sys.getenv_opt "CHAMALLAW_EXAMPLE_SOURCE" with
    | Some path -> path
    | None -> Alcotest.fail "missing CHAMALLAW_EXAMPLE_SOURCE"
  in
  let source = read_file source_path in
  check_contains source "Cabal.Registry.register (module Cabal.Claude_code)" ;
  check_contains source "Cabal.Registry.register (module Cabal.Codex_cli)" ;
  check_contains source "backend_id config.backend" ;
  check_contains source "Cabal.Backend_types.make_task_spec" ;
  check_contains source "Cabal.Agentic_backend.run_task" ;
  check_contains source "~json_schema:curator_schema" ;
  check_contains source "curator_error_of_result" ;
  check_contains source "debug_cabal : bool" ;
  check_contains source "Cabal.Diagnostics.set_handler" ;
  check_contains source "Cabal.Diagnostics.Debug -> \"debug\"" ;
  check_contains
    source
    "if config.debug_cabal then install_debug_cabal_handler ()" ;
  check_absent source "Mock_agent" ;
  check_absent source "CABAL_RUNNER" ;
  check_absent source "canned"

let () =
  Alcotest.run
    "chamallaw workflow"
    [
      ( "real backend path",
        [("source", `Quick, test_example_uses_real_cabal_backend_paths)] );
      ("help", [("exec", `Quick, test_help_executes_without_live_backend)]);
      ( "local workflow",
        [
          ("fixture", `Quick, test_local_fixture_workflow_executes_db_flow);
          ( "debug cabal flag",
            `Quick,
            test_debug_cabal_flag_accepts_fixture_workflow );
          ( "codex backend fixture",
            `Quick,
            test_codex_backend_flag_accepts_fixture_workflow );
        ] );
      ( "backend selection",
        [("unknown", `Quick, test_unknown_backend_is_rejected)] );
      ( "curator fixture failures",
        [
          ( "invalid json",
            `Quick,
            test_invalid_curator_json_preserves_raw_output );
          ( "contract invalid json",
            `Quick,
            test_contract_invalid_curator_json_preserves_raw_output );
        ] );
    ]
