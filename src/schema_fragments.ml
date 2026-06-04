(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

let create_package_schema_version_sql =
  {|CREATE TABLE IF NOT EXISTS law_package_schema_version (
      singleton_key TEXT PRIMARY KEY CHECK(singleton_key = 'epure-law'),
      version       INTEGER NOT NULL
    )|}

let create_package_metadata_sql =
  {|CREATE TABLE IF NOT EXISTS law_package_metadata (
      singleton_key TEXT PRIMARY KEY CHECK(singleton_key = 'epure-law'),
      package_name  TEXT NOT NULL,
      api_version   INTEGER NOT NULL,
      created_at    TEXT NOT NULL DEFAULT (datetime('now')),
      updated_at    TEXT NOT NULL DEFAULT (datetime('now'))
    )|}

let seed_package_metadata_sql =
  {|INSERT INTO law_package_metadata (singleton_key, package_name, api_version)
      VALUES ('epure-law', 'chamallaw', 2)
      ON CONFLICT(singleton_key) DO UPDATE SET
        package_name = excluded.package_name,
        api_version = excluded.api_version,
        updated_at = datetime('now')
      WHERE law_package_metadata.package_name <> excluded.package_name
         OR law_package_metadata.api_version <> excluded.api_version|}

let concept_schemes_table_ddl =
  {|CREATE TABLE IF NOT EXISTS concept_schemes (
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

let concept_schemes_unique_index_ddl =
  {|CREATE UNIQUE INDEX IF NOT EXISTS idx_concept_schemes_slug_scope
      ON concept_schemes (slug, scope_kind, COALESCE(org_id, -1), COALESCE(project_id, -1))|}

let concept_schemes_scope_index_ddl =
  {|CREATE INDEX IF NOT EXISTS idx_concept_schemes_scope
      ON concept_schemes (scope_kind, org_id, project_id)|}

let concept_schemes_provenance_index_ddl =
  {|CREATE INDEX IF NOT EXISTS idx_concept_schemes_provenance
      ON concept_schemes (provenance)|}

let concept_schemes_ddl =
  String.concat
    ";\n"
    [
      concept_schemes_table_ddl;
      concept_schemes_unique_index_ddl;
      concept_schemes_scope_index_ddl;
      concept_schemes_provenance_index_ddl;
    ]

let concepts_table_ddl =
  {|CREATE TABLE IF NOT EXISTS concepts (
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

let concepts_scheme_index_ddl =
  {|CREATE INDEX IF NOT EXISTS idx_concepts_scheme ON concepts (scheme_id)|}

let concepts_scope_index_ddl =
  {|CREATE INDEX IF NOT EXISTS idx_concepts_scope
      ON concepts (scope_kind, org_id, project_id)|}

let concepts_provenance_index_ddl =
  {|CREATE INDEX IF NOT EXISTS idx_concepts_provenance ON concepts (provenance)|}

let concepts_ddl =
  String.concat
    ";\n"
    [
      concepts_table_ddl;
      concepts_scheme_index_ddl;
      concepts_scope_index_ddl;
      concepts_provenance_index_ddl;
    ]

let concept_labels_table_ddl =
  {|CREATE TABLE IF NOT EXISTS concept_labels (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      concept_id INTEGER NOT NULL REFERENCES concepts(id) ON DELETE CASCADE,
      text TEXT NOT NULL,
      kind TEXT NOT NULL CHECK (kind IN ('preferred', 'alternate', 'hidden', 'deprecated')),
      staleness_status TEXT NOT NULL DEFAULT 'active'
        CHECK (staleness_status IN ('active', 'deprecated', 'stale')),
      last_marked_at TEXT,
      created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now'))
    )|}

let concept_labels_concept_index_ddl =
  {|CREATE INDEX IF NOT EXISTS idx_concept_labels_concept
      ON concept_labels (concept_id)|}

let concept_labels_text_index_ddl =
  {|CREATE INDEX IF NOT EXISTS idx_concept_labels_text ON concept_labels (text)|}

let concept_labels_kind_index_ddl =
  {|CREATE INDEX IF NOT EXISTS idx_concept_labels_kind ON concept_labels (kind)|}

let concept_labels_staleness_index_ddl =
  {|CREATE INDEX IF NOT EXISTS idx_concept_labels_staleness
      ON concept_labels (staleness_status)|}

let concept_labels_ddl =
  String.concat
    ";\n"
    [
      concept_labels_table_ddl;
      concept_labels_concept_index_ddl;
      concept_labels_text_index_ddl;
      concept_labels_kind_index_ddl;
      concept_labels_staleness_index_ddl;
    ]

let concept_relation_types_table_ddl =
  {|CREATE TABLE IF NOT EXISTS concept_relation_types (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      slug TEXT NOT NULL UNIQUE,
      is_hierarchical BOOLEAN NOT NULL DEFAULT 0,
      is_traversal_enabled BOOLEAN NOT NULL DEFAULT 1
    )|}

let concept_relation_types_hierarchical_index_ddl =
  {|CREATE INDEX IF NOT EXISTS idx_concept_relation_types_hierarchical
      ON concept_relation_types (is_hierarchical)|}

let concept_relation_types_traversal_index_ddl =
  {|CREATE INDEX IF NOT EXISTS idx_concept_relation_types_traversal
      ON concept_relation_types (is_traversal_enabled)|}

let concept_relation_types_ddl =
  String.concat
    ";\n"
    [
      concept_relation_types_table_ddl;
      concept_relation_types_hierarchical_index_ddl;
      concept_relation_types_traversal_index_ddl;
    ]

let concept_relations_table_ddl =
  {|CREATE TABLE IF NOT EXISTS concept_relations (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      from_concept_id INTEGER NOT NULL REFERENCES concepts(id) ON DELETE CASCADE,
      to_concept_id INTEGER NOT NULL REFERENCES concepts(id) ON DELETE CASCADE,
      relation_type_id INTEGER NOT NULL REFERENCES concept_relation_types(id) ON DELETE RESTRICT,
      is_active BOOLEAN NOT NULL DEFAULT 1,
      created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
      CHECK (from_concept_id != to_concept_id)
    )|}

let concept_relations_from_index_ddl =
  {|CREATE INDEX IF NOT EXISTS idx_concept_relations_from
      ON concept_relations (from_concept_id)|}

let concept_relations_to_index_ddl =
  {|CREATE INDEX IF NOT EXISTS idx_concept_relations_to
      ON concept_relations (to_concept_id)|}

let concept_relations_type_index_ddl =
  {|CREATE INDEX IF NOT EXISTS idx_concept_relations_type
      ON concept_relations (relation_type_id)|}

let concept_relations_active_index_ddl =
  {|CREATE INDEX IF NOT EXISTS idx_concept_relations_active
      ON concept_relations (is_active)|}

let concept_relations_traverse_index_ddl =
  {|CREATE INDEX IF NOT EXISTS idx_concept_relations_traverse
      ON concept_relations (relation_type_id, from_concept_id, is_active)|}

let concept_relations_active_unique_index_ddl =
  {|CREATE UNIQUE INDEX IF NOT EXISTS uniq_concept_relations_active
      ON concept_relations (from_concept_id, to_concept_id, relation_type_id)
      WHERE is_active = 1|}

let concept_relations_ddl =
  String.concat
    ";\n"
    [
      concept_relations_table_ddl;
      concept_relations_from_index_ddl;
      concept_relations_to_index_ddl;
      concept_relations_type_index_ddl;
      concept_relations_active_index_ddl;
      concept_relations_traverse_index_ddl;
      concept_relations_active_unique_index_ddl;
    ]

let concept_search_fts_ddl =
  {|CREATE VIRTUAL TABLE IF NOT EXISTS concept_search_fts USING fts5(
      concept_id UNINDEXED,
      labels,
      definition,
      scope_note,
      tokenize='porter unicode61'
    )|}

let concept_labels_fts_insert_trigger_ddl =
  {|CREATE TRIGGER IF NOT EXISTS concept_labels_fts_insert
    AFTER INSERT ON concept_labels
    BEGIN
      DELETE FROM concept_search_fts WHERE concept_id = NEW.concept_id;
      INSERT INTO concept_search_fts (concept_id, labels, definition, scope_note)
      SELECT c.id, GROUP_CONCAT(cl.text, ' '), c.definition, c.scope_note
      FROM concepts c
      LEFT JOIN concept_labels cl ON cl.concept_id = c.id
      WHERE c.id = NEW.concept_id
      GROUP BY c.id;
    END|}

let concept_labels_fts_update_trigger_ddl =
  {|CREATE TRIGGER IF NOT EXISTS concept_labels_fts_update
    AFTER UPDATE ON concept_labels
    BEGIN
      DELETE FROM concept_search_fts WHERE concept_id = NEW.concept_id;
      INSERT INTO concept_search_fts (concept_id, labels, definition, scope_note)
      SELECT c.id, GROUP_CONCAT(cl.text, ' '), c.definition, c.scope_note
      FROM concepts c
      LEFT JOIN concept_labels cl ON cl.concept_id = c.id
      WHERE c.id = NEW.concept_id
      GROUP BY c.id;
    END|}

let concept_labels_fts_delete_trigger_ddl =
  {|CREATE TRIGGER IF NOT EXISTS concept_labels_fts_delete
    AFTER DELETE ON concept_labels
    BEGIN
      DELETE FROM concept_search_fts WHERE concept_id = OLD.concept_id;
      INSERT INTO concept_search_fts (concept_id, labels, definition, scope_note)
      SELECT c.id, GROUP_CONCAT(cl.text, ' '), c.definition, c.scope_note
      FROM concepts c
      LEFT JOIN concept_labels cl ON cl.concept_id = c.id
      WHERE c.id = OLD.concept_id
      GROUP BY c.id;
    END|}

let concept_labels_fts_triggers_ddl =
  String.concat
    ";\n"
    [
      concept_labels_fts_insert_trigger_ddl;
      concept_labels_fts_update_trigger_ddl;
      concept_labels_fts_delete_trigger_ddl;
    ]

let concepts_fts_insert_trigger_ddl =
  {|CREATE TRIGGER IF NOT EXISTS concepts_fts_insert
    AFTER INSERT ON concepts
    BEGIN
      DELETE FROM concept_search_fts WHERE concept_id = NEW.id;
      INSERT INTO concept_search_fts (concept_id, labels, definition, scope_note)
      SELECT c.id, GROUP_CONCAT(cl.text, ' '), c.definition, c.scope_note
      FROM concepts c
      LEFT JOIN concept_labels cl ON cl.concept_id = c.id
      WHERE c.id = NEW.id
      GROUP BY c.id;
    END|}

let concepts_fts_update_trigger_ddl =
  {|CREATE TRIGGER IF NOT EXISTS concepts_fts_update
    AFTER UPDATE OF definition, scope_note ON concepts
    BEGIN
      DELETE FROM concept_search_fts WHERE concept_id = NEW.id;
      INSERT INTO concept_search_fts (concept_id, labels, definition, scope_note)
      SELECT c.id, GROUP_CONCAT(cl.text, ' '), c.definition, c.scope_note
      FROM concepts c
      LEFT JOIN concept_labels cl ON cl.concept_id = c.id
      WHERE c.id = NEW.id
      GROUP BY c.id;
    END|}

let concepts_fts_delete_trigger_ddl =
  {|CREATE TRIGGER IF NOT EXISTS concepts_fts_delete
    AFTER DELETE ON concepts
    BEGIN
      DELETE FROM concept_search_fts WHERE concept_id = OLD.id;
    END|}

let concepts_fts_triggers_ddl =
  String.concat
    ";\n"
    [
      concepts_fts_insert_trigger_ddl;
      concepts_fts_update_trigger_ddl;
      concepts_fts_delete_trigger_ddl;
    ]

(* Legacy-table awareness (D-iter4-13): the host table at
   src/db/schema_sql_tables.ml:981 is legacy and is not referenced from this
   package as a host API. A one-way import tool lands later per D-PIVOT-04. *)
let laws_table_ddl =
  {|CREATE TABLE IF NOT EXISTS laws (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      statement TEXT NOT NULL,
      rationale TEXT,
      scope_kind TEXT NOT NULL
        CHECK (scope_kind IN ('global','org','project')),
      org_id INTEGER,
      project_id INTEGER,
      owner_user_id INTEGER,
      replaces_law_id INTEGER REFERENCES laws(id) ON DELETE SET NULL,
      relation_kind TEXT
        CHECK (relation_kind IS NULL
               OR relation_kind IN ('replaces','refines','overrides','exempts')),
      provenance_note TEXT,
      is_archived INTEGER NOT NULL DEFAULT 0,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      CHECK (
        (scope_kind = 'global'  AND org_id IS NULL    AND project_id IS NULL)
        OR (scope_kind = 'org'     AND org_id IS NOT NULL AND project_id IS NULL)
        OR (scope_kind = 'project' AND project_id IS NOT NULL)
      ),
      CHECK (
        (replaces_law_id IS NULL AND relation_kind IS NULL)
        OR (replaces_law_id IS NOT NULL AND relation_kind IS NOT NULL)
      )
    )|}

let laws_scope_index_ddl =
  {|CREATE INDEX IF NOT EXISTS laws_scope_idx
      ON laws (scope_kind, org_id, project_id)|}

let laws_replaces_index_ddl =
  {|CREATE INDEX IF NOT EXISTS laws_replaces_idx
      ON laws (replaces_law_id)|}

let laws_owner_index_ddl =
  {|CREATE INDEX IF NOT EXISTS laws_owner_idx
      ON laws (owner_user_id)|}

let laws_ddl =
  String.concat
    ";\n"
    [
      laws_table_ddl;
      laws_scope_index_ddl;
      laws_replaces_index_ddl;
      laws_owner_index_ddl;
    ]

let law_normative_metadata_table_ddl =
  {|CREATE TABLE IF NOT EXISTS law_normative_metadata (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      law_id INTEGER NOT NULL
        REFERENCES laws(id) ON DELETE CASCADE,
      role_kind TEXT NOT NULL
        CHECK (role_kind IN ('primary','secondary')),
      force TEXT NOT NULL
        CHECK (force IN ('obligation','prohibition','permission',
                         'recommendation','exception')),
      modality TEXT NOT NULL
        CHECK (modality IN ('strict','lenient','conditional')),
      strength TEXT NOT NULL
        CHECK (strength IN ('hard','soft')),
      severity TEXT NOT NULL
        CHECK (severity IN ('critical','high','medium','low','informational')),
      authority TEXT NOT NULL
        CHECK (authority IN ('mandatory','advisory','internal','external')),
      is_active INTEGER NOT NULL DEFAULT 1,
      scope_kind TEXT NOT NULL
        CHECK (scope_kind IN ('global','org','project')),
      org_id INTEGER,
      project_id INTEGER,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      CHECK (
        (scope_kind = 'global'  AND org_id IS NULL    AND project_id IS NULL)
        OR (scope_kind = 'org'     AND org_id IS NOT NULL AND project_id IS NULL)
        OR (scope_kind = 'project' AND project_id IS NOT NULL)
      )
    )|}

let law_normative_metadata_law_index_ddl =
  {|CREATE INDEX IF NOT EXISTS lnm_law_idx
      ON law_normative_metadata (law_id)|}

let law_normative_metadata_role_index_ddl =
  {|CREATE INDEX IF NOT EXISTS lnm_role_idx
      ON law_normative_metadata (role_kind)|}

let law_normative_metadata_scope_index_ddl =
  {|CREATE INDEX IF NOT EXISTS lnm_scope_idx
      ON law_normative_metadata (scope_kind, org_id, project_id)|}

let law_normative_metadata_active_unique_index_ddl =
  {|CREATE UNIQUE INDEX IF NOT EXISTS lnm_active_role_uq
      ON law_normative_metadata (law_id, role_kind)
      WHERE is_active = 1|}

let law_normative_metadata_ddl =
  String.concat
    ";\n"
    [
      law_normative_metadata_table_ddl;
      law_normative_metadata_law_index_ddl;
      law_normative_metadata_role_index_ddl;
      law_normative_metadata_scope_index_ddl;
      law_normative_metadata_active_unique_index_ddl;
    ]

let law_concept_links_table_ddl =
  {|CREATE TABLE IF NOT EXISTS law_concept_links (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      law_id INTEGER NOT NULL
        REFERENCES laws(id) ON DELETE CASCADE,
      concept_id INTEGER NOT NULL
        REFERENCES concepts(id) ON DELETE CASCADE,
      role TEXT NOT NULL
        CHECK (role IN ('primary-subject','applicability-context','concern',
                        'artifact-scope','phase-scope','agent-scope',
                        'suggestion-only')),
      is_active INTEGER NOT NULL DEFAULT 1,
      scope_kind TEXT NOT NULL
        CHECK (scope_kind IN ('global','org','project')),
      org_id INTEGER,
      project_id INTEGER,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      CHECK (
        (scope_kind = 'global'  AND org_id IS NULL    AND project_id IS NULL)
        OR (scope_kind = 'org'     AND org_id IS NOT NULL AND project_id IS NULL)
        OR (scope_kind = 'project' AND project_id IS NOT NULL)
      )
    )|}

let law_concept_links_law_index_ddl =
  {|CREATE INDEX IF NOT EXISTS lcl_law_idx
      ON law_concept_links (law_id)|}

let law_concept_links_concept_index_ddl =
  {|CREATE INDEX IF NOT EXISTS lcl_concept_idx
      ON law_concept_links (concept_id)|}

let law_concept_links_role_index_ddl =
  {|CREATE INDEX IF NOT EXISTS lcl_role_idx
      ON law_concept_links (role)|}

let law_concept_links_scope_index_ddl =
  {|CREATE INDEX IF NOT EXISTS lcl_scope_idx
      ON law_concept_links (scope_kind, org_id, project_id)|}

let law_concept_links_active_index_ddl =
  {|CREATE INDEX IF NOT EXISTS lcl_active_idx
      ON law_concept_links (is_active)|}

let law_concept_links_active_unique_index_ddl =
  {|CREATE UNIQUE INDEX IF NOT EXISTS lcl_active_uq
      ON law_concept_links (
        law_id, concept_id, role,
        scope_kind,
        COALESCE(org_id, -1),
        COALESCE(project_id, -1)
      )
      WHERE is_active = 1|}

let law_concept_links_ddl =
  String.concat
    ";\n"
    [
      law_concept_links_table_ddl;
      law_concept_links_law_index_ddl;
      law_concept_links_concept_index_ddl;
      law_concept_links_role_index_ddl;
      law_concept_links_scope_index_ddl;
      law_concept_links_active_index_ddl;
      law_concept_links_active_unique_index_ddl;
    ]

let initial_schema_v1_ddl =
  [
    create_package_metadata_sql;
    seed_package_metadata_sql;
    concept_schemes_table_ddl;
    concept_schemes_unique_index_ddl;
    concept_schemes_scope_index_ddl;
    concept_schemes_provenance_index_ddl;
    concepts_table_ddl;
    concepts_scheme_index_ddl;
    concepts_scope_index_ddl;
    concepts_provenance_index_ddl;
    concept_labels_table_ddl;
    concept_labels_concept_index_ddl;
    concept_labels_text_index_ddl;
    concept_labels_kind_index_ddl;
    concept_labels_staleness_index_ddl;
    concept_relation_types_table_ddl;
    concept_relation_types_hierarchical_index_ddl;
    concept_relation_types_traversal_index_ddl;
    concept_relations_table_ddl;
    concept_relations_from_index_ddl;
    concept_relations_to_index_ddl;
    concept_relations_type_index_ddl;
    concept_relations_active_index_ddl;
    concept_relations_traverse_index_ddl;
    concept_relations_active_unique_index_ddl;
    law_normative_metadata_table_ddl;
    law_normative_metadata_law_index_ddl;
    law_normative_metadata_role_index_ddl;
    law_normative_metadata_scope_index_ddl;
    law_normative_metadata_active_unique_index_ddl;
    law_concept_links_table_ddl;
    law_concept_links_law_index_ddl;
    law_concept_links_concept_index_ddl;
    law_concept_links_role_index_ddl;
    law_concept_links_scope_index_ddl;
    law_concept_links_active_index_ddl;
    law_concept_links_active_unique_index_ddl;
    concept_search_fts_ddl;
    concept_labels_fts_insert_trigger_ddl;
    concept_labels_fts_update_trigger_ddl;
    concept_labels_fts_delete_trigger_ddl;
    concepts_fts_insert_trigger_ddl;
    concepts_fts_update_trigger_ddl;
    concepts_fts_delete_trigger_ddl;
  ]

let initial_schema_ddl =
  [
    create_package_metadata_sql;
    seed_package_metadata_sql;
    concept_schemes_table_ddl;
    concept_schemes_unique_index_ddl;
    concept_schemes_scope_index_ddl;
    concept_schemes_provenance_index_ddl;
    concepts_table_ddl;
    concepts_scheme_index_ddl;
    concepts_scope_index_ddl;
    concepts_provenance_index_ddl;
    laws_table_ddl;
    laws_scope_index_ddl;
    laws_replaces_index_ddl;
    laws_owner_index_ddl;
    concept_labels_table_ddl;
    concept_labels_concept_index_ddl;
    concept_labels_text_index_ddl;
    concept_labels_kind_index_ddl;
    concept_labels_staleness_index_ddl;
    concept_relation_types_table_ddl;
    concept_relation_types_hierarchical_index_ddl;
    concept_relation_types_traversal_index_ddl;
    concept_relations_table_ddl;
    concept_relations_from_index_ddl;
    concept_relations_to_index_ddl;
    concept_relations_type_index_ddl;
    concept_relations_active_index_ddl;
    concept_relations_traverse_index_ddl;
    concept_relations_active_unique_index_ddl;
    law_normative_metadata_table_ddl;
    law_normative_metadata_law_index_ddl;
    law_normative_metadata_role_index_ddl;
    law_normative_metadata_scope_index_ddl;
    law_normative_metadata_active_unique_index_ddl;
    law_concept_links_table_ddl;
    law_concept_links_law_index_ddl;
    law_concept_links_concept_index_ddl;
    law_concept_links_role_index_ddl;
    law_concept_links_scope_index_ddl;
    law_concept_links_active_index_ddl;
    law_concept_links_active_unique_index_ddl;
    concept_search_fts_ddl;
    concept_labels_fts_insert_trigger_ddl;
    concept_labels_fts_update_trigger_ddl;
    concept_labels_fts_delete_trigger_ddl;
    concepts_fts_insert_trigger_ddl;
    concepts_fts_update_trigger_ddl;
    concepts_fts_delete_trigger_ddl;
  ]
