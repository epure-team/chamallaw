(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Package-private cross-scope authoring authorization. *)

val authorize :
  actor:Authorized_scope.t -> target:Authorized_scope.t -> (unit, string) result
