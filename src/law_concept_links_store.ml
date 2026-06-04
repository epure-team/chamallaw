(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

let ( let* ) = Result.bind

type link_role = Law_concept_link_role.link_role =
  | Primary_subject
  | Applicability_context
  | Concern
  | Artifact_scope
  | Phase_scope
  | Agent_scope
  | Suggestion_only

type link_row = {
  id : int;
  law_id : int;
  concept_id : int;
  role : link_role;
  is_active : bool;
  scope : Authorized_scope.t;
  created_at : Ptime.t;
  updated_at : Ptime.t;
}

let db_error = Caqti_error.show

let int_of_bool value = if value then 1 else 0

let bool_of_int value = value <> 0

let exec_sql (module Db : Caqti_eio.CONNECTION) sql =
  let req = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) sql in
  Db.exec req () |> Result.map_error db_error

let rollback conn = ignore (exec_sql conn "ROLLBACK" : (unit, string) result)

let with_begin_immediate conn f =
  let* () = exec_sql conn "BEGIN IMMEDIATE" in
  match f () with
  | Ok value -> (
      match exec_sql conn "COMMIT" with
      | Ok () -> Ok value
      | Error e ->
          rollback conn ;
          Error e)
  | Error e ->
      rollback conn ;
      Error e

let now_string () = Ptime.to_rfc3339 ~tz_offset_s:0 (Ptime_clock.now ())

let time_of_string value =
  match Ptime.of_rfc3339 value with
  | Ok (t, _, _) -> Ok t
  | Error _ -> Error (Printf.sprintf "invalid RFC3339 timestamp %S" value)

let contains_substring haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop index =
    if needle_len = 0 then true
    else if index + needle_len > haystack_len then false
    else if String.sub haystack index needle_len = needle then true
    else loop (index + 1)
  in
  loop 0

let map_insert_error error =
  let msg = db_error error in
  if
    contains_substring msg "UNIQUE constraint failed"
    || contains_substring msg "lcl_active_uq"
  then Error "duplicate active law-concept link"
  else Error msg

let scope_to_db_columns = function
  | Authorized_scope.Global _ -> ("global", None, None)
  | Authorized_scope.Organization {org_id; _} -> ("org", Some org_id, None)
  | Authorized_scope.Project {project_id; org_id; _} ->
      ("project", org_id, Some project_id)

let decode_scope ~scope_kind ~org_id ~project_id =
  match (scope_kind, org_id, project_id) with
  | "global", None, None -> Ok (Authorized_scope.Global {actor_id = None})
  | "org", Some org_id, None ->
      Ok (Authorized_scope.Organization {org_id; actor_id = None})
  | "project", org_id, Some project_id ->
      Ok (Authorized_scope.Project {project_id; org_id; actor_id = None})
  | _ ->
      Error
        (Printf.sprintf
           "invalid link scope columns kind=%S org_id=%s project_id=%s"
           scope_kind
           (Option.fold ~none:"NULL" ~some:string_of_int org_id)
           (Option.fold ~none:"NULL" ~some:string_of_int project_id))

let same_scope left right =
  match (left, right) with
  | Authorized_scope.Global _, Authorized_scope.Global _ -> true
  | Authorized_scope.Organization left, Authorized_scope.Organization right ->
      left.org_id = right.org_id
  | Authorized_scope.Project left, Authorized_scope.Project right ->
      left.project_id = right.project_id && left.org_id = right.org_id
  | _ -> false

let target_scope ~scope = function None -> scope | Some target -> target

let authorize_target ~scope target_scope_opt =
  let target = target_scope ~scope target_scope_opt in
  let* () = Authorized_scope_check.authorize ~actor:scope ~target in
  Ok target

let law_visible conn ~scope ~law_id =
  let* law_opt = Law_store.get_law conn ~scope ~law_id in
  Ok (Option.is_some law_opt)

let concept_visible ((module Db : Caqti_eio.CONNECTION) as _conn) ~scope
    ~concept_id =
  let visible_sql predicate =
    "SELECT id FROM concepts WHERE id = ? AND " ^ predicate ^ " LIMIT 1"
  in
  match scope with
  | Authorized_scope.Global _ ->
      let req =
        Caqti_request.Infix.(Caqti_type.int ->? Caqti_type.int)
          (visible_sql "scope_kind = 'global'")
      in
      Db.find_opt req concept_id |> Result.map_error db_error
      |> Result.map Option.is_some
  | Authorized_scope.Organization {org_id; _} ->
      let req =
        Caqti_request.Infix.(Caqti_type.(t2 int int) ->? Caqti_type.int)
          (visible_sql
             "(scope_kind = 'global' OR (scope_kind = 'organization' AND \
              org_id = ?))")
      in
      Db.find_opt req (concept_id, org_id)
      |> Result.map_error db_error |> Result.map Option.is_some
  | Authorized_scope.Project {project_id; org_id = Some org_id; _} ->
      let req =
        Caqti_request.Infix.(Caqti_type.(t3 int int int) ->? Caqti_type.int)
          (visible_sql
             "(scope_kind = 'global' OR (scope_kind = 'organization' AND \
              org_id = ?) OR (scope_kind = 'project' AND project_id = ?))")
      in
      Db.find_opt req (concept_id, org_id, project_id)
      |> Result.map_error db_error |> Result.map Option.is_some
  | Authorized_scope.Project {project_id; org_id = None; _} ->
      let req =
        Caqti_request.Infix.(Caqti_type.(t2 int int) ->? Caqti_type.int)
          (visible_sql
             "(scope_kind = 'global' OR (scope_kind = 'project' AND project_id \
              = ?))")
      in
      Db.find_opt req (concept_id, project_id)
      |> Result.map_error db_error |> Result.map Option.is_some

let row_type =
  Caqti_type.(
    t2
      (t5 int int int string int)
      (t5 string (option int) (option int) string string))

let decode_row
    ( (id, law_id, concept_id, role_s, is_active),
      (scope_kind, org_id, project_id, created_at_s, updated_at_s) ) =
  let* role = Law_concept_link_role.of_slug role_s in
  let* scope = decode_scope ~scope_kind ~org_id ~project_id in
  let* created_at = time_of_string created_at_s in
  let* updated_at = time_of_string updated_at_s in
  Ok
    {
      id;
      law_id;
      concept_id;
      role;
      is_active = bool_of_int is_active;
      scope;
      created_at;
      updated_at;
    }

let map_rows result =
  let* rows = result |> Result.map_error db_error in
  List.fold_right
    (fun row acc ->
      let* decoded = decode_row row in
      let* rest = acc in
      Ok (decoded :: rest))
    rows
    (Ok [])

let select_fields =
  {|SELECT id, law_id, concept_id, role, is_active, scope_kind, org_id,
           project_id, created_at, updated_at
      FROM law_concept_links|}

let get_by_id_unscoped (module Db : Caqti_eio.CONNECTION) link_id =
  let req =
    Caqti_request.Infix.(Caqti_type.int ->? row_type)
      (select_fields ^ " WHERE id = ?")
  in
  let* row_opt = Db.find_opt req link_id |> Result.map_error db_error in
  match row_opt with
  | None -> Ok None
  | Some row -> decode_row row |> Result.map Option.some

let create_link ((module Db : Caqti_eio.CONNECTION) as conn) ~scope
    ?target_scope ~law_id ~concept_id ~role () =
  let* target_scope = authorize_target ~scope target_scope in
  with_begin_immediate conn @@ fun () ->
  let* law_is_visible = law_visible conn ~scope ~law_id in
  if not law_is_visible then Error "law is not visible in the authorized scope"
  else
    let* concept_is_visible = concept_visible conn ~scope ~concept_id in
    if not concept_is_visible then
      Error "concept is not visible in the authorized scope"
    else
      let scope_kind, org_id, project_id = scope_to_db_columns target_scope in
      let timestamp = now_string () in
      let req =
        Caqti_request.Infix.(
          Caqti_type.(
            t2
              (t5 int int string int string)
              (t4 (option int) (option int) string string))
          ->. Caqti_type.unit)
          {|INSERT INTO law_concept_links
            (law_id, concept_id, role, is_active, scope_kind, org_id, project_id,
             created_at, updated_at)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)|}
      in
      let args =
        ( ( law_id,
            concept_id,
            Law_concept_link_role.slug_of role,
            int_of_bool true,
            scope_kind ),
          (org_id, project_id, timestamp, timestamp) )
      in
      let* () =
        Db.exec req args |> function
        | Ok () -> Ok ()
        | Error e -> map_insert_error e
      in
      let id_req =
        Caqti_request.Infix.(Caqti_type.unit ->! Caqti_type.int)
          "SELECT last_insert_rowid()"
      in
      let* id = Db.find id_req () |> Result.map_error db_error in
      match get_by_id_unscoped conn id with
      | Ok (Some row) -> Ok row
      | Ok None -> Error "created law-concept link could not be reloaded"
      | Error e -> Error e

let list_for_law ((module Db : Caqti_eio.CONNECTION) as conn) ~scope ~law_id =
  let* law_is_visible = law_visible conn ~scope ~law_id in
  if not law_is_visible then Ok []
  else
    match scope with
    | Authorized_scope.Global _ ->
        let req =
          Caqti_request.Infix.(Caqti_type.int ->* row_type)
            (select_fields
           ^ " WHERE law_id = ? AND is_active = 1 AND scope_kind = 'global' \
              ORDER BY id ASC")
        in
        map_rows (Db.collect_list req law_id)
    | Authorized_scope.Organization {org_id; _} ->
        let req =
          Caqti_request.Infix.(Caqti_type.(t2 int int) ->* row_type)
            (select_fields
           ^ " WHERE law_id = ? AND is_active = 1 AND (scope_kind = 'global' \
              OR (scope_kind = 'org' AND org_id = ?)) ORDER BY id ASC")
        in
        map_rows (Db.collect_list req (law_id, org_id))
    | Authorized_scope.Project {project_id; org_id = Some org_id; _} ->
        let req =
          Caqti_request.Infix.(Caqti_type.(t3 int int int) ->* row_type)
            (select_fields
           ^ " WHERE law_id = ? AND is_active = 1 AND (scope_kind = 'global' \
              OR (scope_kind = 'org' AND org_id = ?) OR (scope_kind = \
              'project' AND project_id = ?)) ORDER BY id ASC")
        in
        map_rows (Db.collect_list req (law_id, org_id, project_id))
    | Authorized_scope.Project {project_id; org_id = None; _} ->
        let req =
          Caqti_request.Infix.(Caqti_type.(t2 int int) ->* row_type)
            (select_fields
           ^ " WHERE law_id = ? AND is_active = 1 AND (scope_kind = 'global' \
              OR (scope_kind = 'project' AND project_id = ?)) ORDER BY id ASC")
        in
        map_rows (Db.collect_list req (law_id, project_id))

let list_for_concept ((module Db : Caqti_eio.CONNECTION) as conn) ~scope
    ~concept_id =
  let* concept_is_visible = concept_visible conn ~scope ~concept_id in
  if not concept_is_visible then Ok []
  else
    match scope with
    | Authorized_scope.Global _ ->
        let req =
          Caqti_request.Infix.(Caqti_type.int ->* row_type)
            (select_fields
           ^ " WHERE concept_id = ? AND is_active = 1 AND scope_kind = \
              'global' ORDER BY id ASC")
        in
        map_rows (Db.collect_list req concept_id)
    | Authorized_scope.Organization {org_id; _} ->
        let req =
          Caqti_request.Infix.(Caqti_type.(t2 int int) ->* row_type)
            (select_fields
           ^ " WHERE concept_id = ? AND is_active = 1 AND (scope_kind = \
              'global' OR (scope_kind = 'org' AND org_id = ?)) ORDER BY id ASC"
            )
        in
        map_rows (Db.collect_list req (concept_id, org_id))
    | Authorized_scope.Project {project_id; org_id = Some org_id; _} ->
        let req =
          Caqti_request.Infix.(Caqti_type.(t3 int int int) ->* row_type)
            (select_fields
           ^ " WHERE concept_id = ? AND is_active = 1 AND (scope_kind = \
              'global' OR (scope_kind = 'org' AND org_id = ?) OR (scope_kind = \
              'project' AND project_id = ?)) ORDER BY id ASC")
        in
        map_rows (Db.collect_list req (concept_id, org_id, project_id))
    | Authorized_scope.Project {project_id; org_id = None; _} ->
        let req =
          Caqti_request.Infix.(Caqti_type.(t2 int int) ->* row_type)
            (select_fields
           ^ " WHERE concept_id = ? AND is_active = 1 AND (scope_kind = \
              'global' OR (scope_kind = 'project' AND project_id = ?)) ORDER \
              BY id ASC")
        in
        map_rows (Db.collect_list req (concept_id, project_id))

let deactivate_link ((module Db : Caqti_eio.CONNECTION) as conn) ~scope
    ?target_scope ~link_id () =
  let* target_scope = authorize_target ~scope target_scope in
  with_begin_immediate conn @@ fun () ->
  let* row_opt = get_by_id_unscoped conn link_id in
  match row_opt with
  | None -> Error "law-concept link not found"
  | Some row when not (same_scope row.scope target_scope) ->
      Error "law-concept link not found in target scope"
  | Some _ ->
      let req =
        Caqti_request.Infix.(Caqti_type.(t3 int string int) ->. Caqti_type.unit)
          {|UPDATE law_concept_links
               SET is_active = ?, updated_at = ?
             WHERE id = ?|}
      in
      Db.exec req (int_of_bool false, now_string (), link_id)
      |> Result.map_error db_error
