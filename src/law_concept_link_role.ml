(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type link_role =
  | Primary_subject
  | Applicability_context
  | Concern
  | Artifact_scope
  | Phase_scope
  | Agent_scope
  | Suggestion_only

let slug_of = function
  | Primary_subject -> "primary-subject"
  | Applicability_context -> "applicability-context"
  | Concern -> "concern"
  | Artifact_scope -> "artifact-scope"
  | Phase_scope -> "phase-scope"
  | Agent_scope -> "agent-scope"
  | Suggestion_only -> "suggestion-only"

let of_slug = function
  | "primary-subject" -> Ok Primary_subject
  | "applicability-context" -> Ok Applicability_context
  | "concern" -> Ok Concern
  | "artifact-scope" -> Ok Artifact_scope
  | "phase-scope" -> Ok Phase_scope
  | "agent-scope" -> Ok Agent_scope
  | "suggestion-only" -> Ok Suggestion_only
  | value -> Error (Printf.sprintf "unknown law-concept link role %S" value)
