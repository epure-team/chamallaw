(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

let ( let* ) = Result.bind

let current_package_version = 2

let current_package_name = "chamallaw"

type migration = {
  from_version : int;
  to_version : int;
  description : string;
  up : (module Caqti_eio.CONNECTION) -> (unit, string) result;
}

let exec_sql (module Db : Caqti_eio.CONNECTION) sql =
  let req = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) sql in
  Db.exec req () |> Result.map_error Caqti_error.show

let ensure_version_table conn =
  exec_sql conn Schema_fragments.create_package_schema_version_sql

let read_version_without_ensure (module Db : Caqti_eio.CONNECTION) =
  let req =
    Caqti_request.Infix.(Caqti_type.unit ->? Caqti_type.int)
      {|SELECT version
          FROM law_package_schema_version
         WHERE singleton_key = 'epure-law'|}
  in
  Db.find_opt req ()
  |> Result.map_error Caqti_error.show
  |> Result.map (Option.value ~default:0)

let read_version conn =
  let* () = ensure_version_table conn in
  read_version_without_ensure conn

let set_version (module Db : Caqti_eio.CONNECTION) version =
  let req =
    Caqti_request.Infix.(Caqti_type.int ->. Caqti_type.unit)
      {|INSERT INTO law_package_schema_version (singleton_key, version)
          VALUES ('epure-law', ?)
          ON CONFLICT(singleton_key) DO UPDATE SET
            version = excluded.version|}
  in
  Db.exec req version |> Result.map_error Caqti_error.show

let rollback conn = ignore (exec_sql conn "ROLLBACK" : (unit, string) result)

let ensure_foreign_keys conn = exec_sql conn "PRAGMA foreign_keys = ON"

let with_begin_immediate conn f =
  let* () = exec_sql conn "BEGIN IMMEDIATE" in
  match f () with
  | Ok value -> (
      match exec_sql conn "COMMIT" with
      | Ok () -> Ok value
      | Error e ->
          rollback conn ;
          Error e)
  | Error e ->
      rollback conn ;
      Error e

let run_ddl_list conn ddl_list =
  List.fold_left
    (fun acc sql ->
      let* () = acc in
      exec_sql conn sql)
    (Ok ())
    ddl_list

let split_ddl ddl =
  ddl |> String.split_on_char ';' |> List.map String.trim
  |> List.filter (fun sql -> not (String.equal sql ""))

let table_exists (module Db : Caqti_eio.CONNECTION) table_name =
  let req =
    Caqti_request.Infix.(Caqti_type.string ->! Caqti_type.int)
      "SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = ?"
  in
  Db.find req table_name
  |> Result.map_error Caqti_error.show
  |> Result.map (fun count -> count > 0)

let laws_table_info (module Db : Caqti_eio.CONNECTION) =
  let row_type = Caqti_type.(t6 int string string int (option string) int) in
  let req =
    Caqti_request.Infix.(Caqti_type.unit ->* row_type) "PRAGMA table_info(laws)"
  in
  Db.collect_list req ()
  |> Result.map_error Caqti_error.show
  |> Result.map
       (List.map (fun (_, name, type_name, notnull, default_value, pk) ->
            (name, type_name, notnull, default_value, pk)))

let expected_laws_columns =
  [
    ("id", "INTEGER", 0, None, 1);
    ("statement", "TEXT", 1, None, 0);
    ("rationale", "TEXT", 0, None, 0);
    ("scope_kind", "TEXT", 1, None, 0);
    ("org_id", "INTEGER", 0, None, 0);
    ("project_id", "INTEGER", 0, None, 0);
    ("owner_user_id", "INTEGER", 0, None, 0);
    ("replaces_law_id", "INTEGER", 0, None, 0);
    ("relation_kind", "TEXT", 0, None, 0);
    ("provenance_note", "TEXT", 0, None, 0);
    ("is_archived", "INTEGER", 1, Some "0", 0);
    ("created_at", "TEXT", 1, None, 0);
    ("updated_at", "TEXT", 1, None, 0);
  ]

let host_legacy_collision_error =
  "chamallaw: a 'laws' table is already present in this database with a \
   non-package shape (likely host-owned). Iter4 of epic 89 keeps host laws \
   frozen; coexistence will be handled by the future host->package import \
   epic. Refusing to migrate."

let ensure_no_host_legacy_laws_collision conn =
  let* exists = table_exists conn "laws" in
  if not exists then Ok ()
  else
    let* columns = laws_table_info conn in
    if columns = expected_laws_columns then Ok ()
    else Error host_legacy_collision_error

let law_v2_schema_ddl =
  List.concat_map
    split_ddl
    [
      Schema_fragments.laws_ddl;
      Schema_fragments.law_normative_metadata_ddl;
      Schema_fragments.law_concept_links_ddl;
    ]
  @ [Schema_fragments.seed_package_metadata_sql]

let migrate_v1_to_v2 conn =
  let* () = ensure_no_host_legacy_laws_collision conn in
  run_ddl_list conn law_v2_schema_ddl

let concept_schemes_v2_table_ddl =
  {|CREATE TABLE concept_schemes_v2 (
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
    )|}

let concepts_v2_table_ddl =
  {|CREATE TABLE concepts_v2 (
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
    )|}

let tighten_vocabulary_constraints conn =
  let* () = exec_sql conn "PRAGMA defer_foreign_keys = ON" in
  let* () = exec_sql conn "DROP TRIGGER IF EXISTS concept_labels_fts_insert" in
  let* () = exec_sql conn "DROP TRIGGER IF EXISTS concept_labels_fts_update" in
  let* () = exec_sql conn "DROP TRIGGER IF EXISTS concept_labels_fts_delete" in
  let* () = exec_sql conn "DROP TRIGGER IF EXISTS concepts_fts_insert" in
  let* () = exec_sql conn "DROP TRIGGER IF EXISTS concepts_fts_update" in
  let* () = exec_sql conn "DROP TRIGGER IF EXISTS concepts_fts_delete" in
  let* () = exec_sql conn "DROP TABLE IF EXISTS concept_schemes_v2" in
  let* () = exec_sql conn concept_schemes_v2_table_ddl in
  let* () =
    exec_sql
      conn
      {|INSERT INTO concept_schemes_v2
        (id, slug, display_name, description, provenance, scope_kind, org_id, project_id, created_at)
        SELECT id, slug, display_name, description, provenance, scope_kind,
               org_id, project_id, created_at
          FROM concept_schemes|}
  in
  let* () = exec_sql conn "DROP TABLE concept_schemes" in
  let* () =
    exec_sql conn "ALTER TABLE concept_schemes_v2 RENAME TO concept_schemes"
  in
  let* () = exec_sql conn "DROP TABLE IF EXISTS concepts_v2" in
  let* () = exec_sql conn concepts_v2_table_ddl in
  let* () =
    exec_sql
      conn
      {|INSERT INTO concepts_v2
        (id, scheme_id, slug, definition, scope_note, provenance, scope_kind,
         org_id, project_id, created_at)
        SELECT id, scheme_id, slug, definition, scope_note, provenance,
               scope_kind, org_id, project_id, created_at
          FROM concepts|}
  in
  let* () = exec_sql conn "DROP TABLE concepts" in
  let* () = exec_sql conn "ALTER TABLE concepts_v2 RENAME TO concepts" in
  run_ddl_list conn Schema_fragments.initial_schema_ddl

let active_relation_unique_index_exists (module Db : Caqti_eio.CONNECTION) =
  let req =
    Caqti_request.Infix.(Caqti_type.string ->? Caqti_type.int)
      "SELECT 1 FROM sqlite_master WHERE type = 'index' AND name = ?"
  in
  Db.find_opt req "uniq_concept_relations_active"
  |> Result.map_error Caqti_error.show
  |> Result.map Option.is_some

let package_metadata_is_current (module Db : Caqti_eio.CONNECTION) =
  let req =
    Caqti_request.Infix.(Caqti_type.(t2 string int) ->! Caqti_type.int)
      {|SELECT count(*)
          FROM law_package_metadata
         WHERE singleton_key = 'epure-law'
           AND package_name = ?
           AND api_version = ?|}
  in
  Db.find req (current_package_name, current_package_version)
  |> Result.map_error Caqti_error.show
  |> Result.map (fun count -> count = 1)

let ensure_current_schema conn =
  let* () = ensure_foreign_keys conn in
  let* has_active_relation_unique = active_relation_unique_index_exists conn in
  let* metadata_current = package_metadata_is_current conn in
  if has_active_relation_unique && metadata_current then Ok ()
  else
    with_begin_immediate conn @@ fun () ->
    let* () =
      if has_active_relation_unique then Ok ()
      else tighten_vocabulary_constraints conn
    in
    exec_sql conn Schema_fragments.seed_package_metadata_sql

let migrations =
  [
    {
      from_version = 0;
      to_version = 1;
      description = "Create chamallaw package metadata scaffold";
      up = Package_metadata_store.provision;
    };
    {
      from_version = 1;
      to_version = 2;
      description = "Create package-owned laws and law metadata tables";
      up = migrate_v1_to_v2;
    };
  ]

let apply_steps conn ~from_version ~target_version =
  let* () = ensure_foreign_keys conn in
  with_begin_immediate conn @@ fun () ->
  let* () = ensure_version_table conn in
  let* actual_version = read_version_without_ensure conn in
  if actual_version = target_version then Ok ()
  else if actual_version > target_version then
    Error
      (Printf.sprintf
         "chamallaw package schema version %d is newer than this binary's \
          supported version %d"
         actual_version
         target_version)
  else if actual_version <> from_version then
    Error
      (Printf.sprintf
         "chamallaw package schema version changed from %d to %d before \
          migration could apply"
         from_version
         actual_version)
  else
    let rec apply_from current =
      if current = target_version then Ok ()
      else
        match List.find_opt (fun m -> m.from_version = current) migrations with
        | None ->
            Error
              (Printf.sprintf
                 "missing chamallaw package migration from version %d to %d"
                 current
                 target_version)
        | Some m ->
            if m.to_version <> current + 1 then
              Error
                (Printf.sprintf
                   "chamallaw package migration %S has to_version=%d, expected \
                    %d"
                   m.description
                   m.to_version
                   (current + 1))
            else
              let* () = m.up conn in
              let* () = set_version conn m.to_version in
              apply_from m.to_version
    in
    apply_from actual_version
