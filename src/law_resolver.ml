(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

let ( let* ) = Result.bind

module LS = Law_store
module LNM = Law_normative_metadata_store
module LCL = Law_concept_links_store

type applicability_reason =
  | Scope_match of Authorized_scope.t
  | Link_match of {
      link_role : LCL.link_role;
      concept_id : int;
      concept_slug : string;
      matched_field : string;
    }

type applicable_law = {
  law : LS.law_row;
  effective_force : LNM.force;
  effective_authority : LNM.authority;
  effective_severity : LNM.severity;
  reasons : applicability_reason list;
  overridden_by : int list;
}

type resolution = {
  applicable : applicable_law list;
  unknown : (LS.law_row * string) list;
  exempted : (LS.law_row * int) list;
}

(* Fixed (link role -> work_context field) table. Not a heuristic: every
   law-concept applicability role maps to exactly one Normalized_work_context
   field. Suggestion_only is filtered out before this is ever consulted. *)
let matched_field_of_role : LCL.link_role -> string = function
  | LCL.Artifact_scope -> "artifact_kind"
  | LCL.Phase_scope -> "phase"
  | LCL.Agent_scope -> "agent_role"
  | LCL.Concern -> "domains"
  | LCL.Primary_subject -> "domains"
  | LCL.Applicability_context -> "action"
  | LCL.Suggestion_only -> "suggestion_only"

let scope_rank = function
  | Authorized_scope.Global _ -> 2
  | Authorized_scope.Organization _ -> 1
  | Authorized_scope.Project _ -> 0

(* A law's own status before relation-kind post-processing (Replaces /
   Overrides / Exempts / Refines) folds Overrides/Exempts/Refines over the
   candidate set. [Pre_ok (_, [])] is "excluded": structurally does not apply
   to this work context, which is a definite non-match, not an unknown. *)
type pre_status =
  | Pre_ok of LNM.metadata_row * applicability_reason list
  | Pre_unknown of string

let concept_identity conn ~scope ~concept_id =
  let* concept_opt = Vocabulary_store.Concept_store.get_by_id conn concept_id in
  match concept_opt with
  | None -> Ok None
  | Some (concept : Vocabulary_store.Concept_store.concept_row) ->
      let* labels =
        Vocabulary_store.Concept_label_store.list_by_concept
          conn
          ~scope
          ~concept_id
      in
      let active_label_texts =
        labels
        |> List.filter (fun (l : Vocabulary_store.Concept_label_store.label_row) ->
               l.staleness_status = Active)
        |> List.map (fun (l : Vocabulary_store.Concept_label_store.label_row) ->
               l.text)
      in
      Ok (Some (concept.slug, concept.slug :: active_label_texts))

(* Exact match only (slug or an active label's text) -- never a fuzzy/FTS
   score. FTS5 may rank candidates at ingestion time; it never decides
   applicability here. Returns the concept's slug (its canonical identity,
   independent of which label happened to match) on a hit. *)
let concept_matches_value conn ~scope ~concept_id value =
  let* identity = concept_identity conn ~scope ~concept_id in
  match identity with
  | None -> Ok None
  | Some (slug, candidates) ->
      if List.exists (String.equal value) candidates then Ok (Some slug) else Ok None

let concept_matches_any conn ~scope ~concept_id values =
  let* identity = concept_identity conn ~scope ~concept_id in
  match identity with
  | None -> Ok None
  | Some (slug, candidates) ->
      if List.exists (fun v -> List.exists (String.equal v) candidates) values then
        Ok (Some slug)
      else Ok None

let reason_for_link conn ~scope (work_context : Normalized_work_context.t)
    (link : LCL.link_row) =
  let matched_field = matched_field_of_role link.role in
  let field_match field_value_opt =
    match field_value_opt with
    | None -> Ok None
    | Some v -> concept_matches_value conn ~scope ~concept_id:link.concept_id v
  in
  let* matched_slug =
    match link.role with
    | LCL.Artifact_scope -> field_match work_context.artifact_kind
    | LCL.Phase_scope -> field_match work_context.phase
    | LCL.Agent_scope -> field_match work_context.agent_role
    | LCL.Applicability_context -> field_match work_context.action
    | LCL.Concern | LCL.Primary_subject ->
        concept_matches_any
          conn
          ~scope
          ~concept_id:link.concept_id
          work_context.domains
    | LCL.Suggestion_only -> Ok None
  in
  match matched_slug with
  | None -> Ok None
  | Some concept_slug ->
      Ok
        (Some
           (Link_match
              {link_role = link.role; concept_id = link.concept_id; concept_slug; matched_field}))

(* One law's structural pre-status: its own active [Primary] normative
   metadata plus, if any, which of its non-suggestion applicability links
   matched [work_context]. Missing/ambiguous metadata is always [Pre_unknown]
   -- fail-closed, never silently "does not apply". *)
let structural_pre_status conn ~scope (work_context : Normalized_work_context.t)
    (law : LS.law_row) =
  let* metadata_rows = LNM.list_for_law conn ~scope ~law_id:law.id in
  let primary_rows =
    List.filter (fun (m : LNM.metadata_row) -> m.role_kind = LNM.Primary) metadata_rows
  in
  match primary_rows with
  | [] -> Ok (Pre_unknown "no active primary normative metadata")
  | _ :: _ :: _ -> Ok (Pre_unknown "multiple active primary normative metadata rows")
  | [metadata] ->
      let* links = LCL.list_for_law conn ~scope ~law_id:law.id in
      let applicability_links =
        List.filter (fun (l : LCL.link_row) -> l.role <> LCL.Suggestion_only) links
      in
      let* reasons =
        List.fold_left
          (fun acc link ->
            let* acc = acc in
            let* reason_opt = reason_for_link conn ~scope work_context link in
            match reason_opt with None -> Ok acc | Some r -> Ok (r :: acc))
          (Ok [])
          applicability_links
      in
      if applicability_links = [] then
        if metadata.authority = LNM.Mandatory then
          Ok (Pre_ok (metadata, [Scope_match law.scope]))
        else Ok (Pre_ok (metadata, []))
      else Ok (Pre_ok (metadata, reasons))

let applicable_of law (metadata : LNM.metadata_row) reasons overridden_by =
  {
    law;
    effective_force = metadata.force;
    effective_authority = metadata.authority;
    effective_severity = metadata.severity;
    reasons;
    overridden_by;
  }

(* Shared Overrides/Exempts legitimacy rule: illegitimate iff the target's
   effective authority is Mandatory and the target's scope is strictly
   superior (by scope kind rank) to the acting law's scope. Fail-closed when
   the target isn't resolvable in this scope or its own metadata is itself
   unknown/ambiguous. *)
let assess_relation_legitimacy conn ~scope ~(acting : LS.law_row) ~target_id =
  let* target_opt = LS.get_law conn ~scope ~law_id:target_id in
  match target_opt with
  | None ->
      Ok
        (Error
           (Printf.sprintf
              "relation target law %d not visible in this scope"
              target_id))
  | Some target -> (
      let* target_metadata_rows = LNM.list_for_law conn ~scope ~law_id:target.id in
      let target_primary =
        List.filter
          (fun (m : LNM.metadata_row) -> m.role_kind = LNM.Primary)
          target_metadata_rows
      in
      match target_primary with
      | [metadata] ->
          let illegitimate =
            metadata.authority = LNM.Mandatory
            && scope_rank target.scope > scope_rank acting.scope
          in
          if illegitimate then
            Ok
              (Error
                 (Printf.sprintf
                    "override/exempt forbidden: cannot weaken mandatory law %d \
                     from a superior scope"
                    target.id))
          else Ok (Ok target)
      | [] ->
          Ok
            (Error
               (Printf.sprintf
                  "relation target law %d has no active primary normative \
                   metadata"
                  target.id))
      | _ :: _ :: _ ->
          Ok
            (Error
               (Printf.sprintf
                  "relation target law %d has multiple active primary \
                   normative metadata rows"
                  target.id)))

let resolve conn ~scope ~work_context =
  let* visible = LS.list_visible conn ~scope in
  let candidates = List.filter (fun (l : LS.law_row) -> not l.is_archived) visible in
  let* pre =
    List.fold_left
      (fun acc law ->
        let* acc = acc in
        let* status = structural_pre_status conn ~scope work_context law in
        Ok ((law.LS.id, status) :: acc))
      (Ok [])
      candidates
  in
  let pre_table = Hashtbl.create (List.length pre) in
  List.iter (fun (id, status) -> Hashtbl.replace pre_table id status) pre;
  (* [table] only ever holds Applicable/Unknown entries; an absent entry
     means the law was structurally excluded (definite non-match). *)
  let table = Hashtbl.create (List.length pre) in
  List.iter
    (fun (law : LS.law_row) ->
      match Hashtbl.find pre_table law.id with
      | Pre_unknown reason -> Hashtbl.replace table law.id (`Unknown reason)
      | Pre_ok (_, []) -> ()
      | Pre_ok (metadata, reasons) ->
          Hashtbl.replace table law.id (`Applicable (applicable_of law metadata reasons [])))
    candidates ;
  let exempted = ref [] in
  (* Pass 1: Replaces is a lifecycle fact, independent of whether the
     replacing law itself matches this work context (symmetric to archive). *)
  List.iter
    (fun (law : LS.law_row) ->
      match (law.relation_kind, law.replaces_law_id) with
      | Some LS.Replaces, Some target_id -> Hashtbl.remove table target_id
      | _ -> ())
    candidates ;
  (* Pass 2: Overrides / Exempts. Only a currently-Applicable law exercises
     governance power over another law; an Unknown or excluded law never
     does. *)
  let* () =
    List.fold_left
      (fun acc (law : LS.law_row) ->
        let* () = acc in
        match Hashtbl.find_opt table law.id with
        | Some (`Applicable _) -> (
            match (law.relation_kind, law.replaces_law_id) with
            | Some LS.Overrides, Some target_id -> (
                let* verdict = assess_relation_legitimacy conn ~scope ~acting:law ~target_id in
                match verdict with
                | Error reason -> Hashtbl.replace table law.id (`Unknown reason); Ok ()
                | Ok _ -> (
                    match Hashtbl.find_opt table target_id with
                    | Some (`Applicable _) ->
                        Hashtbl.remove table target_id;
                        (match Hashtbl.find_opt table law.id with
                        | Some (`Applicable a) ->
                            Hashtbl.replace
                              table
                              law.id
                              (`Applicable {a with overridden_by = target_id :: a.overridden_by})
                        | _ -> ()) ;
                        Ok ()
                    | _ -> Ok ()))
            | Some LS.Exempts, Some target_id -> (
                let* verdict = assess_relation_legitimacy conn ~scope ~acting:law ~target_id in
                match verdict with
                | Error reason -> Hashtbl.replace table law.id (`Unknown reason); Ok ()
                | Ok target -> (
                    match Hashtbl.find_opt table target_id with
                    | Some (`Applicable _) ->
                        Hashtbl.remove table target_id;
                        exempted := (target, law.id) :: !exempted;
                        Ok ()
                    | _ -> Ok ()))
            | _ -> Ok ())
        | _ -> Ok ())
      (Ok ())
      candidates
  in
  (* Pass 3: Refines. A law that did not independently qualify (excluded: has
     metadata, matched no link) but refines a law that IS applicable inherits
     that law's applicability reasons; its own metadata still determines its
     effective force/authority/severity (it may be stricter than the law it
     refines). A law with unknown metadata is never rescued this way. *)
  List.iter
    (fun (law : LS.law_row) ->
      match (law.relation_kind, law.replaces_law_id) with
      | Some LS.Refines, Some target_id -> (
          match (Hashtbl.find_opt table law.id, Hashtbl.find_opt pre_table law.id) with
          | None, Some (Pre_ok (metadata, [])) -> (
              match Hashtbl.find_opt table target_id with
              | Some (`Applicable target_applicable) ->
                  Hashtbl.replace
                    table
                    law.id
                    (`Applicable (applicable_of law metadata target_applicable.reasons []))
              | _ -> ())
          | _ -> ())
      | _ -> ())
    candidates ;
  let law_by_id = List.map (fun (l : LS.law_row) -> (l.LS.id, l)) candidates in
  let applicable, unknown =
    Hashtbl.fold
      (fun id entry (app_acc, unk_acc) ->
        match entry with
        | `Applicable a -> (a :: app_acc, unk_acc)
        | `Unknown reason -> (
            match List.assoc_opt id law_by_id with
            | Some law -> (app_acc, (law, reason) :: unk_acc)
            | None -> (app_acc, unk_acc)))
      table
      ([], [])
  in
  let sort_by_id_a l = List.sort (fun a b -> compare a.law.LS.id b.law.LS.id) l in
  let sort_by_id_u l =
    List.sort (fun (a, _) (b, _) -> compare a.LS.id b.LS.id) l
  in
  Ok
    {
      applicable = sort_by_id_a applicable;
      unknown = sort_by_id_u unknown;
      exempted = sort_by_id_u !exempted;
    }
