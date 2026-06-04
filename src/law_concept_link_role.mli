(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Typed law-to-concept link role. *)

type link_role =
  | Primary_subject
  | Applicability_context
  | Concern
  | Artifact_scope
  | Phase_scope
  | Agent_scope
  | Suggestion_only

val slug_of : link_role -> string

val of_slug : string -> (link_role, string) result
