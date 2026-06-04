(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Typed host-adapter contracts for the standalone law package. *)

type error_class =
  | Authorization_denied
  | Missing_scope
  | Invalid_normalized_context
  | Host_transport_or_integration_error

type integration_error = {error_class : error_class; message : string}

type runner_attachment = {label : string; path : string}

type runner_request = {
  workflow_name : string;
  prompt_summary : string;
  attachments : runner_attachment list;
  output_schema : Yojson.Safe.t option;
}

type runner_response = {
  session_id : string option;
  structured_output : Yojson.Safe.t option;
  raw_text : string option;
}

module type CABAL_RUNNER = sig
  type t

  val run : t -> runner_request -> (runner_response, integration_error) result
end
