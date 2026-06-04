(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

let ( let* ) = Result.bind

type relation_kind = Law_relation_kind.relation_kind =
  | Replaces
  | Refines
  | Overrides
  | Exempts

type law_row = {
  id : int;
  statement : string;
  rationale : string option;
  scope : Authorized_scope.t;
  owner_user_id : int option;
  replaces_law_id : int option;
  relation_kind : relation_kind option;
  provenance_note : string option;
  is_archived : bool;
  created_at : Ptime.t;
  updated_at : Ptime.t;
}

let db_error = Caqti_error.show

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
           "invalid law scope columns kind=%S org_id=%s project_id=%s"
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

let validate_relation ~replaces ~relation_kind =
  match (replaces, relation_kind) with
  | None, None | Some _, Some _ -> Ok ()
  | _ -> Error "replaces and relation_kind must be supplied together"

let row_type =
  Caqti_type.(
    t2
      (t5 int string (option string) string (option int))
      (t2
         (t3 (option int) (option int) (option int))
         (t5 (option string) (option string) int string string)))

let decode_row
    ( (id, statement, rationale, scope_kind, org_id),
      ( (project_id, owner_user_id, replaces_law_id),
        ( relation_kind_s,
          provenance_note,
          is_archived,
          created_at_s,
          updated_at_s ) ) ) =
  let* scope = decode_scope ~scope_kind ~org_id ~project_id in
  let* relation_kind =
    match relation_kind_s with
    | None -> Ok None
    | Some value -> Law_relation_kind.of_slug value |> Result.map Option.some
  in
  let* created_at = time_of_string created_at_s in
  let* updated_at = time_of_string updated_at_s in
  Ok
    {
      id;
      statement;
      rationale;
      scope;
      owner_user_id;
      replaces_law_id;
      relation_kind;
      provenance_note;
      is_archived = bool_of_int is_archived;
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
  {|SELECT id, statement, rationale, scope_kind, org_id, project_id,
           owner_user_id, replaces_law_id, relation_kind, provenance_note,
           is_archived, created_at, updated_at
      FROM laws|}

let get_by_id_unscoped (module Db : Caqti_eio.CONNECTION) law_id =
  let req =
    Caqti_request.Infix.(Caqti_type.int ->? row_type)
      (select_fields ^ " WHERE id = ?")
  in
  let* row_opt = Db.find_opt req law_id |> Result.map_error db_error in
  match row_opt with
  | None -> Ok None
  | Some row -> decode_row row |> Result.map Option.some

let get_in_exact_scope conn ~target_scope ~law_id =
  let* row_opt = get_by_id_unscoped conn law_id in
  match row_opt with
  | Some row when same_scope row.scope target_scope -> Ok (Some row)
  | Some _ | None -> Ok None

let create_law_in_tx ((module Db : Caqti_eio.CONNECTION) as conn) ~target_scope
    ~statement ?rationale ?owner_user_id ?replaces ?relation_kind
    ?provenance_note () =
  let* () = validate_relation ~replaces ~relation_kind in
  let scope_kind, org_id, project_id = scope_to_db_columns target_scope in
  let relation_kind_s = Option.map Law_relation_kind.slug_of relation_kind in
  let timestamp = now_string () in
  let req =
    Caqti_request.Infix.(
      Caqti_type.(
        t2
          (t4 string (option string) string (option int))
          (t2
             (t3 (option int) (option int) (option int))
             (t4 (option string) (option string) string string)))
      ->. Caqti_type.unit)
      {|INSERT INTO laws
        (statement, rationale, scope_kind, org_id, project_id, owner_user_id,
         replaces_law_id, relation_kind, provenance_note, created_at, updated_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
  in
  let args =
    ( (statement, rationale, scope_kind, org_id),
      ( (project_id, owner_user_id, replaces),
        (relation_kind_s, provenance_note, timestamp, timestamp) ) )
  in
  let* () = Db.exec req args |> Result.map_error db_error in
  let id_req =
    Caqti_request.Infix.(Caqti_type.unit ->! Caqti_type.int)
      "SELECT last_insert_rowid()"
  in
  let* id = Db.find id_req () |> Result.map_error db_error in
  match get_by_id_unscoped conn id with
  | Ok (Some row) -> Ok row
  | Ok None -> Error "created law could not be reloaded"
  | Error e -> Error e

let create_law conn ~scope ?target_scope ~statement ?rationale ?owner_user_id
    ?replaces ?relation_kind ?provenance_note () =
  let* target_scope = authorize_target ~scope target_scope in
  with_begin_immediate conn @@ fun () ->
  create_law_in_tx
    conn
    ~target_scope
    ~statement
    ?rationale
    ?owner_user_id
    ?replaces
    ?relation_kind
    ?provenance_note
    ()

let get_law (module Db : Caqti_eio.CONNECTION) ~scope ~law_id =
  match scope with
  | Authorized_scope.Global _ ->
      let req =
        Caqti_request.Infix.(Caqti_type.int ->? row_type)
          (select_fields ^ " WHERE id = ? AND scope_kind = 'global'")
      in
      let* row_opt = Db.find_opt req law_id |> Result.map_error db_error in
      Option.fold
        ~none:(Ok None)
        ~some:(fun row -> decode_row row |> Result.map Option.some)
        row_opt
  | Authorized_scope.Organization {org_id; _} ->
      let req =
        Caqti_request.Infix.(Caqti_type.(t2 int int) ->? row_type)
          (select_fields
         ^ " WHERE id = ? AND (scope_kind = 'global' OR (scope_kind = 'org' \
            AND org_id = ?))")
      in
      let* row_opt =
        Db.find_opt req (law_id, org_id) |> Result.map_error db_error
      in
      Option.fold
        ~none:(Ok None)
        ~some:(fun row -> decode_row row |> Result.map Option.some)
        row_opt
  | Authorized_scope.Project {project_id; org_id = Some org_id; _} ->
      let req =
        Caqti_request.Infix.(Caqti_type.(t3 int int int) ->? row_type)
          (select_fields
         ^ " WHERE id = ? AND (scope_kind = 'global' OR (scope_kind = 'org' \
            AND org_id = ?) OR (scope_kind = 'project' AND project_id = ?))")
      in
      let* row_opt =
        Db.find_opt req (law_id, org_id, project_id)
        |> Result.map_error db_error
      in
      Option.fold
        ~none:(Ok None)
        ~some:(fun row -> decode_row row |> Result.map Option.some)
        row_opt
  | Authorized_scope.Project {project_id; org_id = None; _} ->
      let req =
        Caqti_request.Infix.(Caqti_type.(t2 int int) ->? row_type)
          (select_fields
         ^ " WHERE id = ? AND (scope_kind = 'global' OR (scope_kind = \
            'project' AND project_id = ?))")
      in
      let* row_opt =
        Db.find_opt req (law_id, project_id) |> Result.map_error db_error
      in
      Option.fold
        ~none:(Ok None)
        ~some:(fun row -> decode_row row |> Result.map Option.some)
        row_opt

let list_visible (module Db : Caqti_eio.CONNECTION) ~scope =
  match scope with
  | Authorized_scope.Global _ ->
      let req =
        Caqti_request.Infix.(Caqti_type.unit ->* row_type)
          (select_fields ^ " WHERE scope_kind = 'global' ORDER BY id ASC")
      in
      map_rows (Db.collect_list req ())
  | Authorized_scope.Organization {org_id; _} ->
      let req =
        Caqti_request.Infix.(Caqti_type.int ->* row_type)
          (select_fields
         ^ " WHERE scope_kind = 'global' OR (scope_kind = 'org' AND org_id = \
            ?) ORDER BY id ASC")
      in
      map_rows (Db.collect_list req org_id)
  | Authorized_scope.Project {project_id; org_id = Some org_id; _} ->
      let req =
        Caqti_request.Infix.(Caqti_type.(t2 int int) ->* row_type)
          (select_fields
         ^ " WHERE scope_kind = 'global' OR (scope_kind = 'org' AND org_id = \
            ?) OR (scope_kind = 'project' AND project_id = ?) ORDER BY id ASC")
      in
      map_rows (Db.collect_list req (org_id, project_id))
  | Authorized_scope.Project {project_id; org_id = None; _} ->
      let req =
        Caqti_request.Infix.(Caqti_type.int ->* row_type)
          (select_fields
         ^ " WHERE scope_kind = 'global' OR (scope_kind = 'project' AND \
            project_id = ?) ORDER BY id ASC")
      in
      map_rows (Db.collect_list req project_id)

let update_relation ((module Db : Caqti_eio.CONNECTION) as conn) ~scope
    ?target_scope ~law_id ~replaces ~relation_kind () =
  let* target_scope = authorize_target ~scope target_scope in
  let* () = validate_relation ~replaces ~relation_kind in
  with_begin_immediate conn @@ fun () ->
  let* existing = get_in_exact_scope conn ~target_scope ~law_id in
  match existing with
  | None -> Error "law not found in target scope"
  | Some _ -> (
      let relation_kind_s =
        Option.map Law_relation_kind.slug_of relation_kind
      in
      let req =
        Caqti_request.Infix.(
          Caqti_type.(t4 (option int) (option string) string int)
          ->. Caqti_type.unit)
          {|UPDATE laws
               SET replaces_law_id = ?, relation_kind = ?, updated_at = ?
             WHERE id = ?|}
      in
      let* () =
        Db.exec req (replaces, relation_kind_s, now_string (), law_id)
        |> Result.map_error db_error
      in
      match get_by_id_unscoped conn law_id with
      | Ok (Some row) -> Ok row
      | Ok None -> Error "updated law could not be reloaded"
      | Error e -> Error e)

let archive_law ((module Db : Caqti_eio.CONNECTION) as conn) ~scope
    ?target_scope ~law_id () =
  let* target_scope = authorize_target ~scope target_scope in
  with_begin_immediate conn @@ fun () ->
  let* existing = get_in_exact_scope conn ~target_scope ~law_id in
  match existing with
  | None -> Error "law not found in target scope"
  | Some _ ->
      let req =
        Caqti_request.Infix.(Caqti_type.(t2 string int) ->. Caqti_type.unit)
          {|UPDATE laws
               SET is_archived = 1, updated_at = ?
             WHERE id = ?|}
      in
      Db.exec req (now_string (), law_id) |> Result.map_error db_error
