(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

open Chamallaw
module CS = Concept_scheme_store
module C = Concept_store
module CL = Concept_label_store
module LS = Law_store
module LCL = Law_concept_links_store
module BT = Cabal.Backend_types

let ( let* ) = Result.bind

type config = {
  project_dir : string;
  db_path : string;
  log_path : string;
  model : string option;
  curator_output_json_path : string option;
}

type concept_ref = {scheme_slug : string; concept : C.concept_row}

type link_ref = {
  link_scheme_slug : string;
  link_concept_slug : string;
  link_role : LCL.link_role;
}

type concept_suggestion = {
  suggested_scheme_slug : string;
  suggested_scheme_display_name : string;
  suggested_concept_slug : string;
  suggested_preferred_label : string;
  suggested_definition : string;
  suggested_scope_note : string;
}

type law_suggestion = {
  suggested_statement : string;
  suggested_rationale : string;
  suggested_links : link_ref list;
}

type link_suggestion = {
  target_law_statement : string;
  target_scheme_slug : string;
  target_concept_slug : string;
  target_role : LCL.link_role;
  target_reason : string;
}

type curator_output = {
  summary : string;
  concept_suggestions : concept_suggestion list;
  law_suggestions : law_suggestion list;
  link_suggestions : link_suggestion list;
}

type curator_run =
  | Claude_curator of {
      result : BT.task_result;
      structured_json : Yojson.Safe.t;
      parsed : curator_output;
    }
  | File_curator of {
      path : string;
      raw : string;
      structured_json : Yojson.Safe.t;
      parsed : curator_output;
    }

type curator_error = {
  message : string;
  stdout : string;
  stderr : string;
  agent_text : string;
}

type manual_setup = {
  seeded_concepts : (string * C.concept_row) list;
  manual_concepts : (string * C.concept_row) list;
  manual_laws : LS.law_row list;
}

let default_project_dir () =
  let rng = Random.State.make_self_init () in
  let rec loop attempts =
    if attempts = 0 then
      failwith "could not create a unique Chamallaw workflow temp directory"
    else
      let suffix =
        Printf.sprintf
          "%06x%06x"
          (Random.State.bits rng land 0xFFFFFF)
          (Random.State.bits rng land 0xFFFFFF)
      in
      let dir =
        Filename.concat
          (Filename.get_temp_dir_name ())
          ("chamallaw-claude-workflow-" ^ suffix)
      in
      try
        Unix.mkdir dir 0o700 ;
        dir
      with Unix.Unix_error (Unix.EEXIST, _, _) -> loop (attempts - 1)
  in
  loop 100

let default_db_path project_dir =
  Filename.concat project_dir "chamallaw-demo.db"

let default_log_path project_dir =
  Filename.concat project_dir "curator-output.log"

let usage () =
  Printf.sprintf
    "Usage: %s [--project-dir DIR] [--db PATH] [--log PATH] [--model MODEL] \
     [--curator-output-json PATH]\n\n\
     Runs a local Chamallaw law/concept workflow and invokes the real Claude \
     Code backend through Cabal. Claude Code must be installed and \
     authenticated unless --curator-output-json is supplied.\n"
    Sys.argv.(0)

let take_value args i flag =
  if i + 1 >= Array.length args then Error (flag ^ " requires a value")
  else Ok args.(i + 1)

let parse_args () =
  let args = Sys.argv in
  let rec loop i project_dir db_path log_path model curator_output_json_path =
    if i >= Array.length args then
      let project_dir =
        Option.value project_dir ~default:(default_project_dir ())
      in
      let db_path =
        Option.value db_path ~default:(default_db_path project_dir)
      in
      let log_path =
        Option.value log_path ~default:(default_log_path project_dir)
      in
      let model =
        match model with
        | Some _ -> model
        | None -> Sys.getenv_opt "CHAMALLAW_CLAUDE_MODEL"
      in
      Ok {project_dir; db_path; log_path; model; curator_output_json_path}
    else
      match args.(i) with
      | "--help" | "-h" -> Error (usage ())
      | "--project-dir" ->
          let* value = take_value args i "--project-dir" in
          loop
            (i + 2)
            (Some value)
            db_path
            log_path
            model
            curator_output_json_path
      | "--db" ->
          let* value = take_value args i "--db" in
          loop
            (i + 2)
            project_dir
            (Some value)
            log_path
            model
            curator_output_json_path
      | "--log" ->
          let* value = take_value args i "--log" in
          loop
            (i + 2)
            project_dir
            db_path
            (Some value)
            model
            curator_output_json_path
      | "--model" ->
          let* value = take_value args i "--model" in
          loop
            (i + 2)
            project_dir
            db_path
            log_path
            (Some value)
            curator_output_json_path
      | "--curator-output-json" ->
          let* value = take_value args i "--curator-output-json" in
          loop (i + 2) project_dir db_path log_path model (Some value)
      | other -> Error ("unknown argument: " ^ other ^ "\n" ^ usage ())
  in
  loop 1 None None None None None

let is_directory path = try Sys.is_directory path with Sys_error _ -> false

let rec mkdir_p dir =
  if dir = "" || dir = "." then ()
  else if Sys.file_exists dir then
    if is_directory dir then ()
    else invalid_arg (Printf.sprintf "%s exists but is not a directory" dir)
  else
    let parent = Filename.dirname dir in
    if parent <> dir then mkdir_p parent ;
    try Unix.mkdir dir 0o700
    with Unix.Unix_error (Unix.EEXIST, _, _) ->
      if not (is_directory dir) then
        invalid_arg (Printf.sprintf "%s exists but is not a directory" dir)

let ensure_parent path = mkdir_p (Filename.dirname path)

let write_file path content =
  ensure_parent path ;
  let oc =
    open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o600 path
  in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let emit oc fmt =
  Printf.ksprintf
    (fun text ->
      print_string text ;
      output_string oc text ;
      flush stdout ;
      flush oc)
    fmt

let emit_section oc title body = emit oc "\n## %s\n%s\n" title body

let scope =
  Authorized_scope.Project
    {project_id = 89; org_id = Some 1; actor_id = Some "chamallaw-example"}

let scope_to_string = function
  | Authorized_scope.Global _ -> "global"
  | Authorized_scope.Organization {org_id; _} -> Printf.sprintf "org:%d" org_id
  | Authorized_scope.Project {project_id; org_id; _} ->
      Printf.sprintf
        "project:%d%s"
        project_id
        (Option.fold ~none:"" ~some:(Printf.sprintf " org:%d") org_id)

let provenance_to_string = function
  | Epure_builtin -> "epure_builtin"
  | User -> "user"

let label_kind_to_string = function
  | CL.Label_preferred -> "preferred"
  | CL.Label_alternate -> "alternate"
  | CL.Label_hidden -> "hidden"
  | CL.Label_deprecated -> "deprecated"

let role_to_string = function
  | LCL.Primary_subject -> "primary-subject"
  | LCL.Applicability_context -> "applicability-context"
  | LCL.Concern -> "concern"
  | LCL.Artifact_scope -> "artifact-scope"
  | LCL.Phase_scope -> "phase-scope"
  | LCL.Agent_scope -> "agent-scope"
  | LCL.Suggestion_only -> "suggestion-only"

let role_of_string = function
  | "primary-subject" -> Ok LCL.Primary_subject
  | "applicability-context" -> Ok LCL.Applicability_context
  | "concern" -> Ok LCL.Concern
  | "artifact-scope" -> Ok LCL.Artifact_scope
  | "phase-scope" -> Ok LCL.Phase_scope
  | "agent-scope" -> Ok LCL.Agent_scope
  | "suggestion-only" -> Ok LCL.Suggestion_only
  | value -> Error (Printf.sprintf "unknown curator link role %S" value)

let scheme_id (row : CS.scheme_row) = row.id

let scheme_slug (row : CS.scheme_row) = row.slug

let concept_id (row : C.concept_row) = row.id

let concept_slug (row : C.concept_row) = row.slug

let concept_definition (row : C.concept_row) = row.definition

let concept_scope_note (row : C.concept_row) = row.scope_note

let concept_provenance (row : C.concept_row) = row.provenance

let concept_scope (row : C.concept_row) = row.scope

let law_id (row : LS.law_row) = row.id

let law_statement (row : LS.law_row) = row.statement

let law_rationale (row : LS.law_row) = row.rationale

let law_scope (row : LS.law_row) = row.scope

let link_law_id (row : LCL.link_row) = row.law_id

let link_concept_id (row : LCL.link_row) = row.concept_id

let link_role (row : LCL.link_row) = row.role

let label_text (row : CL.label_row) = row.text

let label_kind (row : CL.label_row) = row.kind

let rec fold_result f acc = function
  | [] -> Ok acc
  | x :: xs ->
      let* acc = f acc x in
      fold_result f acc xs

let init_ctx conn =
  let* init_result = init conn in
  match init_result with
  | Ready ctx -> Ok (ctx, "Chamallaw.init: schema ready")
  | Requires_migration {from_version; to_version; apply} ->
      let* ctx = apply () in
      Ok
        ( ctx,
          Printf.sprintf
            "Chamallaw.init: applied package migration %d -> %d"
            from_version
            to_version )

let get_required_scheme ctx slug =
  let* row_opt = CS.get_by_slug ~ctx ~scope ~slug in
  match row_opt with
  | Some row -> Ok row
  | None -> Error (Printf.sprintf "required concept scheme %S is missing" slug)

let get_required_concept ctx scheme slug =
  let* row_opt =
    C.get_by_scheme_and_slug ~ctx ~scope ~scheme_id:(scheme_id scheme) ~slug
  in
  match row_opt with
  | Some row -> Ok row
  | None ->
      Error
        (Printf.sprintf
           "required concept %s:%s is missing"
           (scheme_slug scheme)
           slug)

let get_or_create_scheme ?description ctx ~slug ~display_name () =
  let* existing = CS.get_by_slug ~ctx ~scope ~slug in
  match existing with
  | Some row -> Ok row
  | None ->
      CS.create ~ctx ~scope ~slug ~display_name ?description ~provenance:User ()

let get_or_create_concept ?definition ?scope_note ctx scheme ~slug () =
  let* existing =
    C.get_by_scheme_and_slug ~ctx ~scope ~scheme_id:(scheme_id scheme) ~slug
  in
  match existing with
  | Some row -> Ok row
  | None ->
      C.create
        ~ctx
        ~scope
        ~scheme_id:(scheme_id scheme)
        ~slug
        ?definition
        ?scope_note
        ~provenance:User
        ()

let ensure_label ctx concept ~text ~kind =
  let* labels =
    CL.list_by_concept ~ctx ~scope ~concept_id:(concept_id concept)
  in
  if
    List.exists
      (fun row -> label_text row = text && label_kind row = kind)
      labels
  then Ok ()
  else
    let* _row =
      CL.create ~ctx ~scope ~concept_id:(concept_id concept) ~text ~kind
    in
    Ok ()

let find_law_by_statement ctx statement =
  let* laws = LS.list_visible ~ctx ~scope in
  Ok (List.find_opt (fun row -> law_statement row = statement) laws)

let get_or_create_law ?rationale ctx statement =
  let* existing = find_law_by_statement ctx statement in
  match existing with
  | Some row -> Ok row
  | None ->
      LS.create_law
        ~ctx
        ~scope
        ~statement
        ?rationale
        ~provenance_note:"chamallaw claude workflow example"
        ()

let ensure_link ctx ~law ~concept ~role =
  match
    LCL.create_link
      ~ctx
      ~scope
      ~law_id:(law_id law)
      ~concept_id:(concept_id concept)
      ~role
      ()
  with
  | Ok _row -> Ok ()
  | Error "duplicate active law-concept link" -> Ok ()
  | Error msg -> Error msg

let find_concept_by_ref ctx ~scheme_slug ~concept_slug =
  let* scheme_opt = CS.get_by_slug ~ctx ~scope ~slug:scheme_slug in
  match scheme_opt with
  | None -> Ok None
  | Some scheme ->
      C.get_by_scheme_and_slug
        ~ctx
        ~scope
        ~scheme_id:(scheme_id scheme)
        ~slug:concept_slug

let setup_manual_data ctx =
  let* () = Builtin_vocabulary_seed.run ~ctx in
  let* phases = get_required_scheme ctx "epure-builtin-phases" in
  let* roles = get_required_scheme ctx "epure-builtin-agent-roles" in
  let* implementation = get_required_concept ctx phases "implementation" in
  let* review = get_required_concept ctx phases "review" in
  let* builder = get_required_concept ctx roles "builder" in
  let* workflow =
    get_or_create_scheme
      ctx
      ~slug:"workflow-governance"
      ~display_name:"Workflow Governance"
      ~description:
        "Local Chamallaw workflow and human/agent governance concepts"
      ()
  in
  let* artifacts =
    get_or_create_scheme
      ctx
      ~slug:"artifact-surfaces"
      ~display_name:"Artifact Surfaces"
      ~description:"Artifacts produced or inspected by local law workflows"
      ()
  in
  let* local_state =
    get_or_create_concept
      ctx
      workflow
      ~slug:"local-state"
      ~definition:
        "File-backed local SQLite state used by a standalone law workflow"
      ~scope_note:
        "Applies to Chamallaw demo databases and local package migrations"
      ()
  in
  let* agentic_curation =
    get_or_create_concept
      ctx
      workflow
      ~slug:"agentic-curation"
      ~definition:"A real agentic backend proposes ontology and law refinements"
      ~scope_note:
        "Applies to curator prompts, structured outputs, and accepted \
         suggestions"
      ()
  in
  let* manual_approval =
    get_or_create_concept
      ctx
      workflow
      ~slug:"manual-approval"
      ~definition:"A human operator reviews curator output before relying on it"
      ~scope_note:"Applies before treating suggested laws as authoritative"
      ()
  in
  let* sqlite_database =
    get_or_create_concept
      ctx
      artifacts
      ~slug:"sqlite-database"
      ~definition:
        "The file-backed SQLite database owned by the Chamallaw example"
      ~scope_note:"Stores schemes, concepts, laws, and links"
      ()
  in
  let* curator_log =
    get_or_create_concept
      ctx
      artifacts
      ~slug:"curator-log"
      ~definition:"The durable log containing raw and structured curator output"
      ~scope_note:"Used for auditability of agentic curation"
      ()
  in
  let* () =
    ensure_label ctx local_state ~text:"local state" ~kind:CL.Label_preferred
  in
  let* () =
    ensure_label
      ctx
      agentic_curation
      ~text:"agentic curation"
      ~kind:CL.Label_preferred
  in
  let* () =
    ensure_label
      ctx
      manual_approval
      ~text:"manual approval"
      ~kind:CL.Label_preferred
  in
  let* () =
    ensure_label
      ctx
      sqlite_database
      ~text:"SQLite database"
      ~kind:CL.Label_preferred
  in
  let* () =
    ensure_label ctx curator_log ~text:"curator log" ~kind:CL.Label_preferred
  in
  let* init_law =
    get_or_create_law
      ctx
      ~rationale:
        "Schema readiness is the precondition for package-owned stores."
      "Local law workflows must initialize and migrate the Chamallaw package \
       schema before storing laws."
  in
  let* log_law =
    get_or_create_law
      ctx
      ~rationale:
        "Agentic suggestions must be auditable before they affect local state."
      "Curated ontology suggestions must be logged before being applied to the \
       local database."
  in
  let* review_law =
    get_or_create_law
      ctx
      ~rationale:
        "The example demonstrates curation, not unchecked policy authority."
      "A human operator should inspect curator-created concepts before \
       treating them as authoritative."
  in
  let* () =
    ensure_link ctx ~law:init_law ~concept:implementation ~role:LCL.Phase_scope
  in
  let* () =
    ensure_link ctx ~law:init_law ~concept:local_state ~role:LCL.Primary_subject
  in
  let* () =
    ensure_link
      ctx
      ~law:init_law
      ~concept:sqlite_database
      ~role:LCL.Artifact_scope
  in
  let* () =
    ensure_link ctx ~law:log_law ~concept:review ~role:LCL.Phase_scope
  in
  let* () =
    ensure_link
      ctx
      ~law:log_law
      ~concept:agentic_curation
      ~role:LCL.Primary_subject
  in
  let* () =
    ensure_link ctx ~law:log_law ~concept:curator_log ~role:LCL.Artifact_scope
  in
  let* () =
    ensure_link ctx ~law:review_law ~concept:builder ~role:LCL.Agent_scope
  in
  let* () =
    ensure_link
      ctx
      ~law:review_law
      ~concept:manual_approval
      ~role:LCL.Primary_subject
  in
  Ok
    {
      seeded_concepts =
        [
          ("epure-builtin-phases:implementation", implementation);
          ("epure-builtin-phases:review", review);
          ("epure-builtin-agent-roles:builder", builder);
        ];
      manual_concepts =
        [
          ("workflow-governance:local-state", local_state);
          ("workflow-governance:agentic-curation", agentic_curation);
          ("workflow-governance:manual-approval", manual_approval);
          ("artifact-surfaces:sqlite-database", sqlite_database);
          ("artifact-surfaces:curator-log", curator_log);
        ];
      manual_laws = [init_law; log_law; review_law];
    }

let visible_concepts ctx =
  let* schemes = CS.list_visible ~ctx ~scope in
  fold_result
    (fun acc scheme ->
      let* concepts =
        C.list_visible_by_scheme ~ctx ~scope ~scheme_id:(scheme_id scheme)
      in
      Ok
        (acc
        @ List.map
            (fun concept -> {scheme_slug = scheme_slug scheme; concept})
            concepts))
    []
    schemes

let find_ref refs wanted_id =
  List.find_opt (fun ref_ -> concept_id ref_.concept = wanted_id) refs

let concept_ref_to_string ref_ =
  ref_.scheme_slug ^ ":" ^ concept_slug ref_.concept

let law_links_text ctx refs law =
  let* links = LCL.list_for_law ~ctx ~scope ~law_id:(law_id law) in
  let labels =
    List.map
      (fun link ->
        let concept_text =
          match find_ref refs (link_concept_id link) with
          | Some ref_ -> concept_ref_to_string ref_
          | None -> Printf.sprintf "concept#%d" (link_concept_id link)
        in
        Printf.sprintf "%s[%s]" concept_text (role_to_string (link_role link)))
      links
  in
  Ok (String.concat ", " labels)

let concepts_table ctx =
  let* schemes = CS.list_visible ~ctx ~scope in
  let header =
    "id | scheme | concept | provenance | scope | labels | definition"
  in
  let* lines =
    fold_result
      (fun acc scheme ->
        let* concepts =
          C.list_visible_by_scheme ~ctx ~scope ~scheme_id:(scheme_id scheme)
        in
        fold_result
          (fun acc concept ->
            let* labels =
              CL.list_by_concept ~ctx ~scope ~concept_id:(concept_id concept)
            in
            let labels_text =
              labels
              |> List.map (fun row ->
                  label_text row ^ ":" ^ label_kind_to_string (label_kind row))
              |> String.concat ", "
            in
            let definition =
              Option.value (concept_definition concept) ~default:""
            in
            Ok
              (acc
              @ [
                  Printf.sprintf
                    "%d | %s | %s | %s | %s | %s | %s"
                    (concept_id concept)
                    (scheme_slug scheme)
                    (concept_slug concept)
                    (provenance_to_string (concept_provenance concept))
                    (scope_to_string (concept_scope concept))
                    labels_text
                    definition;
                ]))
          acc
          concepts)
      []
      schemes
  in
  Ok (String.concat "\n" (header :: lines))

let laws_table ctx =
  let* refs = visible_concepts ctx in
  let* laws = LS.list_visible ~ctx ~scope in
  let header = "id | scope | statement | linked concepts" in
  let* lines =
    fold_result
      (fun acc law ->
        let* links = law_links_text ctx refs law in
        Ok
          (acc
          @ [
              Printf.sprintf
                "%d | %s | %s | %s"
                (law_id law)
                (scope_to_string (law_scope law))
                (law_statement law)
                links;
            ]))
      []
      laws
  in
  Ok (String.concat "\n" (header :: lines))

let laws_for_concept ctx concept =
  let* links =
    LCL.list_for_concept ~ctx ~scope ~concept_id:(concept_id concept)
  in
  let law_ids = links |> List.map link_law_id |> List.sort_uniq Int.compare in
  fold_result
    (fun acc law_id ->
      let* law_opt = LS.get_law ~ctx ~scope ~law_id in
      match law_opt with None -> Ok acc | Some law -> Ok (acc @ [law]))
    []
    law_ids

let query_result_text ctx ~scheme_slug ~concept_slug =
  let* concept_opt = find_concept_by_ref ctx ~scheme_slug ~concept_slug in
  match concept_opt with
  | None ->
      Ok
        (Printf.sprintf
           "Query: %s:%s\nResult laws: <concept not found>"
           scheme_slug
           concept_slug)
  | Some concept ->
      let* laws = laws_for_concept ctx concept in
      let results =
        match laws with
        | [] -> "  <none>"
        | rows ->
            rows
            |> List.map (fun row ->
                Printf.sprintf "  - #%d %s" (law_id row) (law_statement row))
            |> String.concat "\n"
      in
      Ok
        (Printf.sprintf
           "Query: %s:%s\nResult laws:\n%s"
           scheme_slug
           concept_slug
           results)

let context_snapshot_json ctx =
  let* refs = visible_concepts ctx in
  let* laws = LS.list_visible ~ctx ~scope in
  let concept_json ref_ =
    `Assoc
      [
        ("id", `Int (concept_id ref_.concept));
        ("scheme_slug", `String ref_.scheme_slug);
        ("concept_slug", `String (concept_slug ref_.concept));
        ( "definition",
          `String (Option.value (concept_definition ref_.concept) ~default:"")
        );
        ( "scope_note",
          `String (Option.value (concept_scope_note ref_.concept) ~default:"")
        );
      ]
  in
  let* law_items =
    fold_result
      (fun acc law ->
        let* links = LCL.list_for_law ~ctx ~scope ~law_id:(law_id law) in
        let link_json link =
          let concept_ref =
            match find_ref refs (link_concept_id link) with
            | Some ref_ -> concept_ref_to_string ref_
            | None -> Printf.sprintf "concept#%d" (link_concept_id link)
          in
          `Assoc
            [
              ("concept", `String concept_ref);
              ("role", `String (role_to_string (link_role link)));
            ]
        in
        Ok
          (acc
          @ [
              `Assoc
                [
                  ("id", `Int (law_id law));
                  ("statement", `String (law_statement law));
                  ( "rationale",
                    `String (Option.value (law_rationale law) ~default:"") );
                  ("links", `List (List.map link_json links));
                ];
            ]))
      []
      laws
  in
  Ok
    (`Assoc
       [
         ("workflow", `String "curate_ontology");
         ("authorized_scope", `String (scope_to_string scope));
         ("visible_concepts", `List (List.map concept_json refs));
         ("visible_laws", `List law_items);
       ])

let curator_schema : Yojson.Safe.t =
  let string_field =
    `Assoc [("type", `String "string"); ("minLength", `Int 1)]
  in
  let role_enum =
    `Assoc
      [
        ("type", `String "string");
        ( "enum",
          `List
            [
              `String "primary-subject";
              `String "applicability-context";
              `String "concern";
              `String "artifact-scope";
              `String "phase-scope";
              `String "agent-scope";
              `String "suggestion-only";
            ] );
      ]
  in
  let link_ref_schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ("scheme_slug", string_field);
              ("concept_slug", string_field);
              ("role", role_enum);
            ] );
        ( "required",
          `List [`String "scheme_slug"; `String "concept_slug"; `String "role"]
        );
        ("additionalProperties", `Bool false);
      ]
  in
  `Assoc
    [
      ("$schema", `String "https://json-schema.org/draft/2020-12/schema");
      ("type", `String "object");
      ( "properties",
        `Assoc
          [
            ("summary", string_field);
            ( "concept_suggestions",
              `Assoc
                [
                  ("type", `String "array");
                  ( "items",
                    `Assoc
                      [
                        ("type", `String "object");
                        ( "properties",
                          `Assoc
                            [
                              ("scheme_slug", string_field);
                              ("scheme_display_name", string_field);
                              ("concept_slug", string_field);
                              ("preferred_label", string_field);
                              ("definition", string_field);
                              ("scope_note", string_field);
                            ] );
                        ( "required",
                          `List
                            [
                              `String "scheme_slug";
                              `String "scheme_display_name";
                              `String "concept_slug";
                              `String "preferred_label";
                              `String "definition";
                              `String "scope_note";
                            ] );
                        ("additionalProperties", `Bool false);
                      ] );
                ] );
            ( "law_suggestions",
              `Assoc
                [
                  ("type", `String "array");
                  ( "items",
                    `Assoc
                      [
                        ("type", `String "object");
                        ( "properties",
                          `Assoc
                            [
                              ("statement", string_field);
                              ("rationale", string_field);
                              ( "concept_links",
                                `Assoc
                                  [
                                    ("type", `String "array");
                                    ("items", link_ref_schema);
                                  ] );
                            ] );
                        ( "required",
                          `List
                            [
                              `String "statement";
                              `String "rationale";
                              `String "concept_links";
                            ] );
                        ("additionalProperties", `Bool false);
                      ] );
                ] );
            ( "link_suggestions",
              `Assoc
                [
                  ("type", `String "array");
                  ( "items",
                    `Assoc
                      [
                        ("type", `String "object");
                        ( "properties",
                          `Assoc
                            [
                              ("law_statement", string_field);
                              ("scheme_slug", string_field);
                              ("concept_slug", string_field);
                              ("role", role_enum);
                              ("reason", string_field);
                            ] );
                        ( "required",
                          `List
                            [
                              `String "law_statement";
                              `String "scheme_slug";
                              `String "concept_slug";
                              `String "role";
                              `String "reason";
                            ] );
                        ("additionalProperties", `Bool false);
                      ] );
                ] );
          ] );
      ( "required",
        `List
          [
            `String "summary";
            `String "concept_suggestions";
            `String "law_suggestions";
            `String "link_suggestions";
          ] );
      ("additionalProperties", `Bool false);
    ]

let member name = function
  | `Assoc fields -> Option.value (List.assoc_opt name fields) ~default:`Null
  | _ -> `Null

let string_member field json =
  match member field json with
  | `String value -> Ok value
  | other ->
      Error
        (Printf.sprintf
           "curator output field %S expected string, got %s"
           field
           (Yojson.Safe.to_string other))

let list_member field json =
  match member field json with
  | `List values -> Ok values
  | other ->
      Error
        (Printf.sprintf
           "curator output field %S expected array, got %s"
           field
           (Yojson.Safe.to_string other))

let parse_link_ref json =
  let* link_scheme_slug = string_member "scheme_slug" json in
  let* link_concept_slug = string_member "concept_slug" json in
  let* role_slug = string_member "role" json in
  let* link_role = role_of_string role_slug in
  Ok {link_scheme_slug; link_concept_slug; link_role}

let parse_concept_suggestion json =
  let* suggested_scheme_slug = string_member "scheme_slug" json in
  let* suggested_scheme_display_name =
    string_member "scheme_display_name" json
  in
  let* suggested_concept_slug = string_member "concept_slug" json in
  let* suggested_preferred_label = string_member "preferred_label" json in
  let* suggested_definition = string_member "definition" json in
  let* suggested_scope_note = string_member "scope_note" json in
  Ok
    {
      suggested_scheme_slug;
      suggested_scheme_display_name;
      suggested_concept_slug;
      suggested_preferred_label;
      suggested_definition;
      suggested_scope_note;
    }

let parse_law_suggestion json =
  let* suggested_statement = string_member "statement" json in
  let* suggested_rationale = string_member "rationale" json in
  let* link_items = list_member "concept_links" json in
  let* suggested_links =
    fold_result
      (fun acc item ->
        parse_link_ref item |> Result.map (fun link -> acc @ [link]))
      []
      link_items
  in
  Ok {suggested_statement; suggested_rationale; suggested_links}

let parse_link_suggestion json =
  let* target_law_statement = string_member "law_statement" json in
  let* target_scheme_slug = string_member "scheme_slug" json in
  let* target_concept_slug = string_member "concept_slug" json in
  let* role_slug = string_member "role" json in
  let* target_role = role_of_string role_slug in
  let* target_reason = string_member "reason" json in
  Ok
    {
      target_law_statement;
      target_scheme_slug;
      target_concept_slug;
      target_role;
      target_reason;
    }

let parse_curator_output json =
  let* summary = string_member "summary" json in
  let* concept_items = list_member "concept_suggestions" json in
  let* law_items = list_member "law_suggestions" json in
  let* link_items = list_member "link_suggestions" json in
  let* concept_suggestions =
    fold_result
      (fun acc item ->
        parse_concept_suggestion item |> Result.map (fun value -> acc @ [value]))
      []
      concept_items
  in
  let* law_suggestions =
    fold_result
      (fun acc item ->
        parse_law_suggestion item |> Result.map (fun value -> acc @ [value]))
      []
      law_items
  in
  let* link_suggestions =
    fold_result
      (fun acc item ->
        parse_link_suggestion item |> Result.map (fun value -> acc @ [value]))
      []
      link_items
  in
  Ok {summary; concept_suggestions; law_suggestions; link_suggestions}

let apply_link_ref ctx law link =
  let* concept_opt =
    find_concept_by_ref
      ctx
      ~scheme_slug:link.link_scheme_slug
      ~concept_slug:link.link_concept_slug
  in
  match concept_opt with
  | None ->
      Ok
        [
          Printf.sprintf
            "skipped link for law #%d: missing concept %s:%s"
            (law_id law)
            link.link_scheme_slug
            link.link_concept_slug;
        ]
  | Some concept ->
      let* () = ensure_link ctx ~law ~concept ~role:link.link_role in
      Ok
        [
          Printf.sprintf
            "ensured link law #%d -> %s:%s (%s)"
            (law_id law)
            link.link_scheme_slug
            link.link_concept_slug
            (role_to_string link.link_role);
        ]

let apply_curator_output ctx output =
  let* concept_notes =
    fold_result
      (fun notes suggestion ->
        let* scheme =
          get_or_create_scheme
            ctx
            ~slug:suggestion.suggested_scheme_slug
            ~display_name:suggestion.suggested_scheme_display_name
            ~description:"Created or reused from Claude curate_ontology output"
            ()
        in
        let* concept =
          get_or_create_concept
            ctx
            scheme
            ~slug:suggestion.suggested_concept_slug
            ~definition:suggestion.suggested_definition
            ~scope_note:suggestion.suggested_scope_note
            ()
        in
        let* () =
          ensure_label
            ctx
            concept
            ~text:suggestion.suggested_preferred_label
            ~kind:CL.Label_preferred
        in
        Ok
          (notes
          @ [
              Printf.sprintf
                "ensured curator concept %s:%s"
                suggestion.suggested_scheme_slug
                suggestion.suggested_concept_slug;
            ]))
      []
      output.concept_suggestions
  in
  let* law_notes =
    fold_result
      (fun notes suggestion ->
        let* law =
          get_or_create_law
            ctx
            ~rationale:suggestion.suggested_rationale
            suggestion.suggested_statement
        in
        let* link_notes =
          fold_result
            (fun acc link ->
              let* notes = apply_link_ref ctx law link in
              Ok (acc @ notes))
            []
            suggestion.suggested_links
        in
        Ok
          (notes
          @ [
              Printf.sprintf
                "ensured curator law #%d: %s"
                (law_id law)
                (law_statement law);
            ]
          @ link_notes))
      []
      output.law_suggestions
  in
  let* direct_link_notes =
    fold_result
      (fun notes suggestion ->
        let* law_opt =
          find_law_by_statement ctx suggestion.target_law_statement
        in
        match law_opt with
        | None ->
            Ok
              (notes
              @ [
                  Printf.sprintf
                    "skipped curator direct link: missing law statement %S"
                    suggestion.target_law_statement;
                ])
        | Some law ->
            let link =
              {
                link_scheme_slug = suggestion.target_scheme_slug;
                link_concept_slug = suggestion.target_concept_slug;
                link_role = suggestion.target_role;
              }
            in
            let* link_notes = apply_link_ref ctx law link in
            Ok
              (notes @ link_notes
              @ [
                  Printf.sprintf
                    "curator link reason: %s"
                    suggestion.target_reason;
                ]))
      []
      output.link_suggestions
  in
  Ok (concept_notes @ law_notes @ direct_link_notes)

let curator_request context_path =
  {
    Host_adapter.workflow_name = "curate_ontology";
    prompt_summary =
      "Curate the local Chamallaw ontology and law/concept links.";
    attachments = [{label = "context_snapshot"; path = context_path}];
    output_schema = Some curator_schema;
  }

let file_index_preamble attachments =
  let lines =
    attachments
    |> List.map (fun (attachment : Host_adapter.runner_attachment) ->
        Printf.sprintf "- %s: %s" attachment.label attachment.path)
    |> String.concat "\n"
  in
  "## Files available for this invocation\n" ^ lines ^ "\n"

let prompt_of_request (request : Host_adapter.runner_request) =
  file_index_preamble request.attachments
  ^ "\nYou are the Chamallaw ontology and law curator. This is workflow `"
  ^ request.workflow_name
  ^ "`.\n\
     Read the context snapshot file listed above. Do not edit files and do not \
     modify the database. Return only JSON matching the required schema.\n\n\
     Curation goals:\n\
     - suggest zero to two useful project-scoped concepts, preferably under an \
     existing scheme when appropriate;\n\
     - suggest zero to two concise local workflow laws when the current laws \
     miss an important guardrail;\n\
     - suggest law/concept links for seeded concepts and newly-created concepts;\n\
     - use exact law_statement values from the context for direct link \
     suggestions.\n"

let register_claude_backend () =
  Cabal.Registry.clear () ;
  Cabal.Registry.register (module Cabal.Claude_code)

let managed_namespace =
  BT.
    {
      id = "chamallaw";
      display_name = "Chamallaw";
      config_dir = ".chamallaw/cabal-config";
    }

let curator_error ?(stdout = "") ?(stderr = "") ?(agent_text = "") message =
  {message; stdout; stderr; agent_text}

let curator_error_of_result message (result : BT.task_result) =
  curator_error
    ~stdout:result.stdout
    ~stderr:result.stderr
    ~agent_text:result.agent_text
    message

let curator_error_to_string error =
  let sections =
    [
      Some error.message;
      (if String.length error.agent_text = 0 then None
       else Some ("agent_text:\n" ^ error.agent_text));
      (if String.length error.stdout = 0 then None
       else Some ("stdout:\n" ^ error.stdout));
      (if String.length error.stderr = 0 then None
       else Some ("stderr:\n" ^ error.stderr));
    ]
  in
  sections |> List.filter_map Fun.id |> String.concat "\n"

let parse_curator_json_text ~source raw =
  match Yojson.Safe.from_string raw with
  | exception Yojson.Json_error msg ->
      Error (Printf.sprintf "%s was not valid JSON: %s" source msg)
  | structured_json -> (
      match parse_curator_output structured_json with
      | Ok parsed -> Ok (structured_json, parsed)
      | Error msg ->
          Error
            (Printf.sprintf
               "%s did not match the curator output contract: %s"
               source
               msg))

let load_curator_output_json path =
  match read_file path with
  | exception exn ->
      Error
        (curator_error
           (Printf.sprintf
              "could not read curator output JSON file %s: %s"
              path
              (Printexc.to_string exn)))
  | raw -> (
      match
        parse_curator_json_text ~source:("curator output file " ^ path) raw
      with
      | Ok (structured_json, parsed) ->
          Ok (File_curator {path; raw; structured_json; parsed})
      | Error msg -> Error (curator_error ~stdout:raw msg))

let run_curator ~sw ~env config context_path =
  register_claude_backend () ;
  let backend =
    match Cabal.Registry.get "claude-code" with
    | Some backend -> backend
    | None -> failwith "claude-code backend was not registered"
  in
  if not (Cabal.Agentic_backend.available ~sw ~env backend) then
    Error
      (curator_error
         "Claude Code CLI is not available on PATH. Install/authenticate the \
          `claude` CLI and rerun the example.")
  else
    let request = curator_request context_path in
    let prompt = prompt_of_request request in
    let spec =
      Cabal.Backend_types.make_task_spec
        ~prompt
        ~instructions:
          "Return raw JSON only; no markdown fences or prose outside the JSON \
           object."
        ~working_dir:config.project_dir
        ~timeout:240.0
        ~expected_outputs:[]
        ~managed_namespace
        ?model:config.model
        ~read_only:true
        ~json_schema:curator_schema
        ()
    in
    let result = Cabal.Agentic_backend.run_task ~sw ~env backend spec in
    match result.Cabal.Backend_types.status with
    | BT.Success -> (
        match Yojson.Safe.from_string result.BT.agent_text with
        | exception Yojson.Json_error msg ->
            Error
              (curator_error_of_result
                 (Printf.sprintf
                    "Claude returned success but the structured output was not \
                     valid JSON: %s"
                    msg)
                 result)
        | structured_json -> (
            match parse_curator_output structured_json with
            | Ok parsed -> Ok (Claude_curator {result; structured_json; parsed})
            | Error msg ->
                Error
                  (curator_error_of_result
                     (Printf.sprintf
                        "Claude returned success but the structured output did \
                         not match the curator output contract: %s"
                        msg)
                     result)))
    | BT.Failed msg ->
        Error
          (curator_error_of_result
             (Printf.sprintf "Claude Code returned failure: %s" msg)
             result)
    | BT.Timeout ->
        Error (curator_error_of_result "Claude Code timed out" result)
    | BT.Cancelled ->
        Error
          (curator_error_of_result
             "Claude Code invocation was cancelled"
             result)

let load_or_run_curator ~sw ~env config context_path =
  match config.curator_output_json_path with
  | Some path -> load_curator_output_json path
  | None -> run_curator ~sw ~env config context_path

let log_curator_error oc error =
  if String.length error.stdout > 0 then
    emit_section oc "Curator raw output" error.stdout ;
  if String.length error.stderr > 0 then
    emit_section oc "Curator stderr" error.stderr ;
  if String.length error.agent_text > 0 then
    emit_section oc "Curator agent text" error.agent_text ;
  emit_section oc "Curator error" error.message

let log_curator_success oc = function
  | Claude_curator {result; structured_json; parsed} ->
      emit_section oc "Curator raw output" result.BT.stdout ;
      if String.length result.BT.stderr > 0 then
        emit_section oc "Curator stderr" result.BT.stderr ;
      emit_section
        oc
        "Curator structured output"
        (Yojson.Safe.pretty_to_string structured_json) ;
      emit_section oc "Curator summary" parsed.summary
  | File_curator {path; raw; structured_json; parsed} ->
      emit oc "Curator fixture file: %s\n" path ;
      emit_section oc "Curator raw output" raw ;
      emit_section
        oc
        "Curator structured output"
        (Yojson.Safe.pretty_to_string structured_json) ;
      emit_section oc "Curator summary" parsed.summary

let curator_parsed = function
  | Claude_curator {parsed; _} | File_curator {parsed; _} -> parsed

let run_queries ctx =
  let queries =
    [
      ("epure-builtin-phases", "implementation");
      ("workflow-governance", "agentic-curation");
      ("artifact-surfaces", "curator-log");
    ]
  in
  fold_result
    (fun acc (scheme_slug, concept_slug) ->
      let* text = query_result_text ctx ~scheme_slug ~concept_slug in
      Ok (acc @ [text]))
    []
    queries

let run_with_connection ~sw ~env config oc conn =
  let* ctx, init_message = init_ctx conn in
  emit oc "%s\n" init_message ;
  let* setup = setup_manual_data ctx in
  emit oc "Builtin_vocabulary_seed: applied\n" ;
  emit_section
    oc
    "Seeded concepts used"
    (setup.seeded_concepts
    |> List.map (fun (name, concept) ->
        Printf.sprintf "- %s (#%d)" name (concept_id concept))
    |> String.concat "\n") ;
  emit_section
    oc
    "Manually inserted concepts"
    (setup.manual_concepts
    |> List.map (fun (name, concept) ->
        Printf.sprintf "- %s (#%d)" name (concept_id concept))
    |> String.concat "\n") ;
  emit_section
    oc
    "Manually inserted laws"
    (setup.manual_laws
    |> List.map (fun law ->
        Printf.sprintf "- #%d %s" (law_id law) (law_statement law))
    |> String.concat "\n") ;
  let context_path =
    Filename.concat config.project_dir "curator-context.json"
  in
  let* context_json = context_snapshot_json ctx in
  write_file context_path (Yojson.Safe.pretty_to_string context_json ^ "\n") ;
  emit oc "Curator context file: %s\n" context_path ;
  let* curator_run =
    match load_or_run_curator ~sw ~env config context_path with
    | Ok value -> Ok value
    | Error error ->
        log_curator_error oc error ;
        Error (curator_error_to_string error)
  in
  log_curator_success oc curator_run ;
  let* apply_notes = apply_curator_output ctx (curator_parsed curator_run) in
  emit_section oc "Applied curator changes" (String.concat "\n" apply_notes) ;
  let* concepts = concepts_table ctx in
  emit_section oc "Final concepts table" concepts ;
  let* laws = laws_table ctx in
  emit_section oc "Final laws table" laws ;
  let* queries = run_queries ctx in
  emit_section oc "Queries and results" (String.concat "\n\n" queries) ;
  Ok ()

let run ~sw ~env config =
  try
    mkdir_p config.project_dir ;
    ensure_parent config.db_path ;
    ensure_parent config.log_path ;
    let oc =
      open_out_gen
        [Open_wronly; Open_creat; Open_trunc; Open_text]
        0o600
        config.log_path
    in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () ->
        emit oc "DB path: %s\n" config.db_path ;
        emit oc "Project work dir: %s\n" config.project_dir ;
        emit oc "Curator log path: %s\n" config.log_path ;
        Option.iter (emit oc "Claude model override: %s\n") config.model ;
        let stdenv = (env :> Caqti_eio.stdenv) in
        match
          Caqti_eio_unix.connect
            ~sw
            ~stdenv
            (Uri.of_string ("sqlite3:" ^ config.db_path))
        with
        | Error err -> Error ("DB connect failed: " ^ Caqti_error.show err)
        | Ok conn -> run_with_connection ~sw ~env config oc conn)
  with exn -> Error (Printexc.to_string exn)

let () =
  match parse_args () with
  | Error msg ->
      prerr_string msg ;
      if String.starts_with ~prefix:"Usage:" msg then exit 0 else exit 2
  | Ok config ->
      let exit_code = ref 0 in
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      (match run ~sw ~env config with
      | Ok () -> ()
      | Error msg ->
          prerr_endline ("Error: " ^ msg) ;
          exit_code := 1) ;
      exit !exit_code
