(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

let ( let* ) = Result.bind

module K = Law_normative_metadata_kinds

type force = K.force =
  | Obligation
  | Prohibition
  | Permission
  | Recommendation
  | Exception

type modality = K.modality = Strict | Lenient | Conditional

type strength = K.strength = Hard | Soft

type severity = K.severity = Critical | High | Medium | Low | Informational

type authority = K.authority = Mandatory | Advisory | Internal | External

type role_kind = K.role_kind = Primary | Secondary

type metadata_row = {
  id : int;
  law_id : int;
  role_kind : role_kind;
  force : force;
  modality : modality;
  strength : strength;
  severity : severity;
  authority : authority;
  is_active : bool;
  scope : Authorized_scope.t;
  created_at : Ptime.t;
  updated_at : Ptime.t;
}

let db_error = Caqti_error.show

let int_of_bool value = if value then 1 else 0

let bool_of_int value = value <> 0

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
    || contains_substring msg "lnm_active_role_uq"
  then Error "duplicate active normative metadata"
  else Error msg

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
           "invalid metadata scope columns kind=%S org_id=%s project_id=%s"
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

let row_type =
  Caqti_type.(
    t2
      (t5 int int string string string)
      (t2
         (t4 string string string int)
         (t5 string (option int) (option int) string string)))

let decode_row
    ( (id, law_id, role_kind_s, force_s, modality_s),
      ( (strength_s, severity_s, authority_s, is_active),
        (scope_kind, org_id, project_id, created_at_s, updated_at_s) ) ) =
  let* role_kind = K.role_kind_of_slug role_kind_s in
  let* force = K.force_of_slug force_s in
  let* modality = K.modality_of_slug modality_s in
  let* strength = K.strength_of_slug strength_s in
  let* severity = K.severity_of_slug severity_s in
  let* authority = K.authority_of_slug authority_s in
  let* scope = decode_scope ~scope_kind ~org_id ~project_id in
  let* created_at = time_of_string created_at_s in
  let* updated_at = time_of_string updated_at_s in
  Ok
    {
      id;
      law_id;
      role_kind;
      force;
      modality;
      strength;
      severity;
      authority;
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
  {|SELECT id, law_id, role_kind, force, modality, strength, severity,
           authority, is_active, scope_kind, org_id, project_id, created_at,
           updated_at
      FROM law_normative_metadata|}

let get_by_id_unscoped (module Db : Caqti_eio.CONNECTION) metadata_id =
  let req =
    Caqti_request.Infix.(Caqti_type.int ->? row_type)
      (select_fields ^ " WHERE id = ?")
  in
  let* row_opt = Db.find_opt req metadata_id |> Result.map_error db_error in
  match row_opt with
  | None -> Ok None
  | Some row -> decode_row row |> Result.map Option.some

let insert_metadata_row ((module Db : Caqti_eio.CONNECTION) as conn)
    ~target_scope ~law_id ~role_kind ~force ~modality ~strength ~severity
    ~authority =
  let scope_kind, org_id, project_id = scope_to_db_columns target_scope in
  let timestamp = now_string () in
  let req =
    Caqti_request.Infix.(
      Caqti_type.(
        t2
          (t5 int string string string string)
          (t2
             (t4 string string int string)
             (t4 (option int) (option int) string string)))
      ->. Caqti_type.unit)
      {|INSERT INTO law_normative_metadata
        (law_id, role_kind, force, modality, strength, severity, authority,
         is_active, scope_kind, org_id, project_id, created_at, updated_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
  in
  let args =
    ( ( law_id,
        K.role_kind_slug_of role_kind,
        K.force_slug_of force,
        K.modality_slug_of modality,
        K.strength_slug_of strength ),
      ( ( K.severity_slug_of severity,
          K.authority_slug_of authority,
          int_of_bool true,
          scope_kind ),
        (org_id, project_id, timestamp, timestamp) ) )
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
  | Ok None -> Error "created normative metadata could not be reloaded"
  | Error e -> Error e

let create_metadata conn ~scope ?target_scope ~law_id ~role_kind ~force
    ~modality ~strength ~severity ~authority () =
  let* target_scope = authorize_target ~scope target_scope in
  with_begin_immediate conn @@ fun () ->
  let* visible = law_visible conn ~scope ~law_id in
  if not visible then Error "law is not visible in the authorized scope"
  else
    insert_metadata_row
      conn
      ~target_scope
      ~law_id
      ~role_kind
      ~force
      ~modality
      ~strength
      ~severity
      ~authority

let list_for_law ((module Db : Caqti_eio.CONNECTION) as conn) ~scope ~law_id =
  let* visible = law_visible conn ~scope ~law_id in
  if not visible then Ok []
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

let set_active ((module Db : Caqti_eio.CONNECTION) as conn) ~scope ?target_scope
    ~metadata_id ~is_active () =
  let* target_scope = authorize_target ~scope target_scope in
  with_begin_immediate conn @@ fun () ->
  let* row_opt = get_by_id_unscoped conn metadata_id in
  match row_opt with
  | None -> Error "normative metadata not found"
  | Some row when not (same_scope row.scope target_scope) ->
      Error "normative metadata not found in target scope"
  | Some _ -> (
      let req =
        Caqti_request.Infix.(Caqti_type.(t3 int string int) ->. Caqti_type.unit)
          {|UPDATE law_normative_metadata
               SET is_active = ?, updated_at = ?
             WHERE id = ?|}
      in
      Db.exec req (int_of_bool is_active, now_string (), metadata_id)
      |> function
      | Ok () -> Ok ()
      | Error e -> map_insert_error e)

let replace_for_law_in_tx ((module Db : Caqti_eio.CONNECTION) as conn)
    ~target_scope ~law_id ~rows =
  let scope_kind, org_id, project_id = scope_to_db_columns target_scope in
  let deactivate_req =
    Caqti_request.Infix.(
      Caqti_type.(t5 string int string (option int) (option int))
      ->. Caqti_type.unit)
      {|UPDATE law_normative_metadata
           SET is_active = 0, updated_at = ?
         WHERE law_id = ? AND is_active = 1 AND scope_kind = ?
           AND COALESCE(org_id, -1) = COALESCE(?, -1)
           AND COALESCE(project_id, -1) = COALESCE(?, -1)|}
  in
  let* () =
    Db.exec
      deactivate_req
      (now_string (), law_id, scope_kind, org_id, project_id)
    |> Result.map_error db_error
  in
  List.fold_left
    (fun acc row ->
      let* inserted = acc in
      let* next =
        insert_metadata_row
          conn
          ~target_scope
          ~law_id
          ~role_kind:row.role_kind
          ~force:row.force
          ~modality:row.modality
          ~strength:row.strength
          ~severity:row.severity
          ~authority:row.authority
      in
      Ok (next :: inserted))
    (Ok [])
    rows
  |> Result.map List.rev

let replace_for_law conn ~scope ?target_scope ~law_id ~rows () =
  let* target_scope = authorize_target ~scope target_scope in
  with_begin_immediate conn @@ fun () ->
  let* visible = law_visible conn ~scope ~law_id in
  if not visible then Error "law is not visible in the authorized scope"
  else replace_for_law_in_tx conn ~target_scope ~law_id ~rows
