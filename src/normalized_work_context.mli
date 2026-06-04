(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Host-normalized work context consumed by standalone package APIs. *)

type t = {
  phase : string option;
  artifact_kind : string option;
  action : string option;
  agent_role : string option;
  project_lifecycle : string option;
  context_files : string list;
  risk_flags : string list;
  domains : string list;
  facts : (string * Yojson.Safe.t) list;
}

val empty : t
