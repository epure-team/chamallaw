(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type relation_kind = Replaces | Refines | Overrides | Exempts

let slug_of = function
  | Replaces -> "replaces"
  | Refines -> "refines"
  | Overrides -> "overrides"
  | Exempts -> "exempts"

let of_slug = function
  | "replaces" -> Ok Replaces
  | "refines" -> Ok Refines
  | "overrides" -> Ok Overrides
  | "exempts" -> Ok Exempts
  | value -> Error (Printf.sprintf "unknown law relation kind %S" value)
