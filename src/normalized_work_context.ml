(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

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

let empty =
  {
    phase = None;
    artifact_kind = None;
    action = None;
    agent_role = None;
    project_lifecycle = None;
    context_files = [];
    risk_flags = [];
    domains = [];
    facts = [];
  }
