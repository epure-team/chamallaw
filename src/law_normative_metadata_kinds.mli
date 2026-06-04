(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Typed normative metadata dimensions for package-owned laws. *)

(** Normative force. *)
type force =
  | Obligation
  | Prohibition
  | Permission
  | Recommendation
  | Exception

(** Normative modality. *)
type modality = Strict | Lenient | Conditional

(** Normative strength. *)
type strength = Hard | Soft

(** Normative severity. *)
type severity = Critical | High | Medium | Low | Informational

(** Normative authority. *)
type authority = Mandatory | Advisory | Internal | External

(** Metadata role slot per law. *)
type role_kind = Primary | Secondary

val force_slug_of : force -> string

val force_of_slug : string -> (force, string) result

val modality_slug_of : modality -> string

val modality_of_slug : string -> (modality, string) result

val strength_slug_of : strength -> string

val strength_of_slug : string -> (strength, string) result

val severity_slug_of : severity -> string

val severity_of_slug : string -> (severity, string) result

val authority_slug_of : authority -> string

val authority_of_slug : string -> (authority, string) result

val role_kind_slug_of : role_kind -> string

val role_kind_of_slug : string -> (role_kind, string) result
