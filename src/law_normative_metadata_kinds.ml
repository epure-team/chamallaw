(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type force =
  | Obligation
  | Prohibition
  | Permission
  | Recommendation
  | Exception

type modality = Strict | Lenient | Conditional

type strength = Hard | Soft

type severity = Critical | High | Medium | Low | Informational

type authority = Mandatory | Advisory | Internal | External

type role_kind = Primary | Secondary

let force_slug_of = function
  | Obligation -> "obligation"
  | Prohibition -> "prohibition"
  | Permission -> "permission"
  | Recommendation -> "recommendation"
  | Exception -> "exception"

let force_of_slug = function
  | "obligation" -> Ok Obligation
  | "prohibition" -> Ok Prohibition
  | "permission" -> Ok Permission
  | "recommendation" -> Ok Recommendation
  | "exception" -> Ok Exception
  | value -> Error (Printf.sprintf "unknown law force %S" value)

let modality_slug_of = function
  | Strict -> "strict"
  | Lenient -> "lenient"
  | Conditional -> "conditional"

let modality_of_slug = function
  | "strict" -> Ok Strict
  | "lenient" -> Ok Lenient
  | "conditional" -> Ok Conditional
  | value -> Error (Printf.sprintf "unknown law modality %S" value)

let strength_slug_of = function Hard -> "hard" | Soft -> "soft"

let strength_of_slug = function
  | "hard" -> Ok Hard
  | "soft" -> Ok Soft
  | value -> Error (Printf.sprintf "unknown law strength %S" value)

let severity_slug_of = function
  | Critical -> "critical"
  | High -> "high"
  | Medium -> "medium"
  | Low -> "low"
  | Informational -> "informational"

let severity_of_slug = function
  | "critical" -> Ok Critical
  | "high" -> Ok High
  | "medium" -> Ok Medium
  | "low" -> Ok Low
  | "informational" -> Ok Informational
  | value -> Error (Printf.sprintf "unknown law severity %S" value)

let authority_slug_of = function
  | Mandatory -> "mandatory"
  | Advisory -> "advisory"
  | Internal -> "internal"
  | External -> "external"

let authority_of_slug = function
  | "mandatory" -> Ok Mandatory
  | "advisory" -> Ok Advisory
  | "internal" -> Ok Internal
  | "external" -> Ok External
  | value -> Error (Printf.sprintf "unknown law authority %S" value)

let role_kind_slug_of = function
  | Primary -> "primary"
  | Secondary -> "secondary"

let role_kind_of_slug = function
  | "primary" -> Ok Primary
  | "secondary" -> Ok Secondary
  | value -> Error (Printf.sprintf "unknown law metadata role %S" value)
