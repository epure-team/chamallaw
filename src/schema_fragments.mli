(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Package-owned schema fragments applied by the package init protocol. *)

val create_package_schema_version_sql : string

val create_package_metadata_sql : string

val seed_package_metadata_sql : string

val concept_schemes_ddl : string

val concepts_ddl : string

val concept_labels_ddl : string

val concept_relation_types_ddl : string

val concept_relations_ddl : string

val laws_ddl : string

val law_normative_metadata_ddl : string

val law_concept_links_ddl : string

val concept_search_fts_ddl : string

val concept_labels_fts_triggers_ddl : string

val concepts_fts_triggers_ddl : string

val initial_schema_v1_ddl : string list

val initial_schema_ddl : string list
