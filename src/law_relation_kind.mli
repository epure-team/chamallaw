(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Typed relation between package-owned laws. *)

type relation_kind = Replaces | Refines | Overrides | Exempts

val slug_of : relation_kind -> string

val of_slug : string -> (relation_kind, string) result
