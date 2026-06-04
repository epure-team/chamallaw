(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

module Provenance_codec = struct
  type t = Epure_builtin | User

  let to_string = function Epure_builtin -> "epure_builtin" | User -> "user"

  let of_string = function
    | "epure_builtin" -> Ok Epure_builtin
    | "user" -> Ok User
    | value -> Error (Printf.sprintf "unknown provenance %S" value)
end

module Time_codec = struct
  let to_string t = Ptime.to_rfc3339 ~tz_offset_s:0 t

  let of_string value =
    match Ptime.of_rfc3339 value with
    | Ok (t, _, _) -> Ok t
    | Error _ -> Error (Printf.sprintf "invalid RFC3339 timestamp %S" value)
end

module Concept_scheme_store = struct
  let ( let* ) = Result.bind

  type scheme_row = {
    id : int;
    slug : string;
    display_name : string;
    description : string option;
    provenance : Provenance_codec.t;
    scope : Authorized_scope.t;
    created_at : Ptime.t;
  }

  let db_error = Caqti_error.show

  let scope_to_db_columns = function
    | Authorized_scope.Global _ -> ("global", None, None)
    | Organization {org_id; _} -> ("organization", Some org_id, None)
    | Project {project_id; org_id; _} -> ("project", org_id, Some project_id)

  let decode_scope ~scope_kind ~org_id ~project_id =
    match (scope_kind, org_id, project_id) with
    | "global", None, None -> Ok (Authorized_scope.Global {actor_id = None})
    | "organization", Some org_id, None ->
        Ok (Authorized_scope.Organization {org_id; actor_id = None})
    | "project", org_id, Some project_id ->
        Ok (Authorized_scope.Project {project_id; org_id; actor_id = None})
    | _ ->
        Error
          (Printf.sprintf
             "invalid scope columns kind=%S org_id=%s project_id=%s"
             scope_kind
             (Option.fold ~none:"NULL" ~some:string_of_int org_id)
             (Option.fold ~none:"NULL" ~some:string_of_int project_id))

  let row_type =
    Caqti_type.(
      t2
        (t4 int string string (option string))
        (t5 string string (option int) (option int) string))

  let decode_row
      ( (id, slug, display_name, description),
        (provenance_s, scope_kind, org_id, project_id, created_at_s) ) =
    let* provenance = Provenance_codec.of_string provenance_s in
    let* scope = decode_scope ~scope_kind ~org_id ~project_id in
    let* created_at = Time_codec.of_string created_at_s in
    Ok {id; slug; display_name; description; provenance; scope; created_at}

  let map_row result =
    let* row = result |> Result.map_error db_error in
    decode_row row

  let map_rows result =
    let* rows = result |> Result.map_error db_error in
    List.fold_right
      (fun row acc ->
        let* decoded = decode_row row in
        let* rest = acc in
        Ok (decoded :: rest))
      rows
      (Ok [])

  let get_by_id (module Db : Caqti_eio.CONNECTION) id =
    let req =
      Caqti_request.Infix.(Caqti_type.int ->? row_type)
        {|SELECT id, slug, display_name, description,
               provenance, scope_kind, org_id, project_id, created_at
          FROM concept_schemes
         WHERE id = ?|}
    in
    let* row_opt = Db.find_opt req id |> Result.map_error db_error in
    match row_opt with
    | None -> Ok None
    | Some row -> decode_row row |> Result.map Option.some

  let create (module Db : Caqti_eio.CONNECTION) ~scope ~slug ~display_name
      ?description ~provenance () =
    let scope_kind, org_id, project_id = scope_to_db_columns scope in
    let req =
      Caqti_request.Infix.(
        Caqti_type.(
          t2
            (t4 string string (option string) string)
            (t3 string (option int) (option int)))
        ->. Caqti_type.unit)
        {|INSERT INTO concept_schemes
          (slug, display_name, description, provenance, scope_kind, org_id, project_id)
        VALUES (?, ?, ?, ?, ?, ?, ?)|}
    in
    let args =
      ( (slug, display_name, description, Provenance_codec.to_string provenance),
        (scope_kind, org_id, project_id) )
    in
    let* () = Db.exec req args |> Result.map_error db_error in
    let id_req =
      Caqti_request.Infix.(Caqti_type.unit ->! Caqti_type.int)
        "SELECT last_insert_rowid()"
    in
    let* id = Db.find id_req () |> Result.map_error db_error in
    match get_by_id (module Db) id with
    | Ok (Some row) -> Ok row
    | Ok None -> Error "created concept scheme could not be reloaded"
    | Error e -> Error e

  let select_visible_project_with_org =
    {|SELECT id, slug, display_name, description,
           provenance, scope_kind, org_id, project_id, created_at
      FROM concept_schemes
     WHERE scope_kind = 'global'
        OR (scope_kind = 'organization' AND org_id = ?)
        OR (scope_kind = 'project' AND project_id = ?)
     ORDER BY id ASC|}

  let list_visible (module Db : Caqti_eio.CONNECTION) ~scope =
    match scope with
    | Authorized_scope.Global _ ->
        let req =
          Caqti_request.Infix.(Caqti_type.unit ->* row_type)
            {|SELECT id, slug, display_name, description,
                   provenance, scope_kind, org_id, project_id, created_at
              FROM concept_schemes
             WHERE scope_kind = 'global'
             ORDER BY id ASC|}
        in
        map_rows (Db.collect_list req ())
    | Organization {org_id; _} ->
        let req =
          Caqti_request.Infix.(Caqti_type.int ->* row_type)
            {|SELECT id, slug, display_name, description,
                   provenance, scope_kind, org_id, project_id, created_at
              FROM concept_schemes
             WHERE scope_kind = 'global'
                OR (scope_kind = 'organization' AND org_id = ?)
             ORDER BY id ASC|}
        in
        map_rows (Db.collect_list req org_id)
    | Project {project_id; org_id = Some org_id; _} ->
        let req =
          Caqti_request.Infix.(Caqti_type.(t2 int int) ->* row_type)
            select_visible_project_with_org
        in
        map_rows (Db.collect_list req (org_id, project_id))
    | Project {project_id; org_id = None; _} ->
        let req =
          Caqti_request.Infix.(Caqti_type.int ->* row_type)
            {|SELECT id, slug, display_name, description,
                   provenance, scope_kind, org_id, project_id, created_at
              FROM concept_schemes
             WHERE scope_kind = 'global'
                OR (scope_kind = 'project' AND project_id = ?)
             ORDER BY id ASC|}
        in
        map_rows (Db.collect_list req project_id)

  let get_by_slug (module Db : Caqti_eio.CONNECTION) ~scope ~slug =
    let order =
      " ORDER BY CASE scope_kind WHEN 'project' THEN 3 WHEN 'organization' \
       THEN 2 ELSE 1 END DESC, id ASC LIMIT 1"
    in
    match scope with
    | Authorized_scope.Global _ -> (
        let req =
          Caqti_request.Infix.(Caqti_type.string ->? row_type)
            ("SELECT id, slug, display_name, description, provenance, \
              scope_kind, org_id, project_id, created_at FROM concept_schemes \
              WHERE slug = ? AND scope_kind = 'global'" ^ order)
        in
        let* row_opt = Db.find_opt req slug |> Result.map_error db_error in
        match row_opt with
        | None -> Ok None
        | Some row -> decode_row row |> Result.map Option.some)
    | Organization {org_id; _} -> (
        let req =
          Caqti_request.Infix.(Caqti_type.(t2 string int) ->? row_type)
            ("SELECT id, slug, display_name, description, provenance, \
              scope_kind, org_id, project_id, created_at FROM concept_schemes \
              WHERE slug = ? AND (scope_kind = 'global' OR (scope_kind = \
              'organization' AND org_id = ?))" ^ order)
        in
        let* row_opt =
          Db.find_opt req (slug, org_id) |> Result.map_error db_error
        in
        match row_opt with
        | None -> Ok None
        | Some row -> decode_row row |> Result.map Option.some)
    | Project {project_id; org_id = Some org_id; _} -> (
        let req =
          Caqti_request.Infix.(Caqti_type.(t3 string int int) ->? row_type)
            ("SELECT id, slug, display_name, description, provenance, \
              scope_kind, org_id, project_id, created_at FROM concept_schemes \
              WHERE slug = ? AND (scope_kind = 'global' OR (scope_kind = \
              'organization' AND org_id = ?) OR (scope_kind = 'project' AND \
              project_id = ?))" ^ order)
        in
        let* row_opt =
          Db.find_opt req (slug, org_id, project_id)
          |> Result.map_error db_error
        in
        match row_opt with
        | None -> Ok None
        | Some row -> decode_row row |> Result.map Option.some)
    | Project {project_id; org_id = None; _} -> (
        let req =
          Caqti_request.Infix.(Caqti_type.(t2 string int) ->? row_type)
            ("SELECT id, slug, display_name, description, provenance, \
              scope_kind, org_id, project_id, created_at FROM concept_schemes \
              WHERE slug = ? AND (scope_kind = 'global' OR (scope_kind = \
              'project' AND project_id = ?))" ^ order)
        in
        let* row_opt =
          Db.find_opt req (slug, project_id) |> Result.map_error db_error
        in
        match row_opt with
        | None -> Ok None
        | Some row -> decode_row row |> Result.map Option.some)
end

module Concept_store = struct
  let ( let* ) = Result.bind

  type concept_row = {
    id : int;
    scheme_id : int;
    slug : string;
    definition : string option;
    scope_note : string option;
    provenance : Provenance_codec.t;
    scope : Authorized_scope.t;
    created_at : Ptime.t;
  }

  let db_error = Caqti_error.show

  let row_type =
    Caqti_type.(
      t2
        (t5 int int string (option string) (option string))
        (t5 string string (option int) (option int) string))

  let decode_row
      ( (id, scheme_id, slug, definition, scope_note),
        (provenance_s, scope_kind, org_id, project_id, created_at_s) ) =
    let* provenance = Provenance_codec.of_string provenance_s in
    let* scope =
      Concept_scheme_store.decode_scope ~scope_kind ~org_id ~project_id
    in
    let* created_at = Time_codec.of_string created_at_s in
    Ok
      {
        id;
        scheme_id;
        slug;
        definition;
        scope_note;
        provenance;
        scope;
        created_at;
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

  let get_by_id (module Db : Caqti_eio.CONNECTION) id =
    let req =
      Caqti_request.Infix.(Caqti_type.int ->? row_type)
        {|SELECT id, scheme_id, slug, definition, scope_note,
               provenance, scope_kind, org_id, project_id, created_at
          FROM concepts
         WHERE id = ?|}
    in
    let* row_opt = Db.find_opt req id |> Result.map_error db_error in
    match row_opt with
    | None -> Ok None
    | Some row -> decode_row row |> Result.map Option.some

  let create (module Db : Caqti_eio.CONNECTION) ~scope ~scheme_id ~slug
      ?definition ?scope_note ~provenance () =
    let scope_kind, org_id, project_id =
      Concept_scheme_store.scope_to_db_columns scope
    in
    let req =
      Caqti_request.Infix.(
        Caqti_type.(
          t2
            (t4 int string (option string) (option string))
            (t4 string string (option int) (option int)))
        ->. Caqti_type.unit)
        {|INSERT INTO concepts
          (scheme_id, slug, definition, scope_note, provenance, scope_kind, org_id, project_id)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?)|}
    in
    let args =
      ( (scheme_id, slug, definition, scope_note),
        (Provenance_codec.to_string provenance, scope_kind, org_id, project_id)
      )
    in
    let* () = Db.exec req args |> Result.map_error db_error in
    let id_req =
      Caqti_request.Infix.(Caqti_type.unit ->! Caqti_type.int)
        "SELECT last_insert_rowid()"
    in
    let* id = Db.find id_req () |> Result.map_error db_error in
    match get_by_id (module Db) id with
    | Ok (Some row) -> Ok row
    | Ok None -> Error "created concept could not be reloaded"
    | Error e -> Error e

  let visible_select =
    "SELECT id, scheme_id, slug, definition, scope_note, provenance, \
     scope_kind, org_id, project_id, created_at FROM concepts"

  let list_visible_by_scheme (module Db : Caqti_eio.CONNECTION) ~scope
      ~scheme_id =
    match scope with
    | Authorized_scope.Global _ ->
        let req =
          Caqti_request.Infix.(Caqti_type.int ->* row_type)
            (visible_select
           ^ " WHERE scheme_id = ? AND scope_kind = 'global' ORDER BY id ASC")
        in
        map_rows (Db.collect_list req scheme_id)
    | Organization {org_id; _} ->
        let req =
          Caqti_request.Infix.(Caqti_type.(t2 int int) ->* row_type)
            (visible_select
           ^ " WHERE scheme_id = ? AND (scope_kind = 'global' OR (scope_kind = \
              'organization' AND org_id = ?)) ORDER BY id ASC")
        in
        map_rows (Db.collect_list req (scheme_id, org_id))
    | Project {project_id; org_id = Some org_id; _} ->
        let req =
          Caqti_request.Infix.(Caqti_type.(t3 int int int) ->* row_type)
            (visible_select
           ^ " WHERE scheme_id = ? AND (scope_kind = 'global' OR (scope_kind = \
              'organization' AND org_id = ?) OR (scope_kind = 'project' AND \
              project_id = ?)) ORDER BY id ASC")
        in
        map_rows (Db.collect_list req (scheme_id, org_id, project_id))
    | Project {project_id; org_id = None; _} ->
        let req =
          Caqti_request.Infix.(Caqti_type.(t2 int int) ->* row_type)
            (visible_select
           ^ " WHERE scheme_id = ? AND (scope_kind = 'global' OR (scope_kind = \
              'project' AND project_id = ?)) ORDER BY id ASC")
        in
        map_rows (Db.collect_list req (scheme_id, project_id))

  let get_by_scheme_and_slug (module Db : Caqti_eio.CONNECTION) ~scope
      ~scheme_id ~slug =
    let order =
      " ORDER BY CASE scope_kind WHEN 'project' THEN 3 WHEN 'organization' \
       THEN 2 ELSE 1 END DESC, id ASC LIMIT 1"
    in
    match scope with
    | Authorized_scope.Global _ -> (
        let req =
          Caqti_request.Infix.(Caqti_type.(t2 int string) ->? row_type)
            (visible_select
           ^ " WHERE scheme_id = ? AND slug = ? AND scope_kind = 'global'"
           ^ order)
        in
        let* row_opt =
          Db.find_opt req (scheme_id, slug) |> Result.map_error db_error
        in
        match row_opt with
        | None -> Ok None
        | Some row -> decode_row row |> Result.map Option.some)
    | Organization {org_id; _} -> (
        let req =
          Caqti_request.Infix.(Caqti_type.(t3 int string int) ->? row_type)
            (visible_select
           ^ " WHERE scheme_id = ? AND slug = ? AND (scope_kind = 'global' OR \
              (scope_kind = 'organization' AND org_id = ?))" ^ order)
        in
        let* row_opt =
          Db.find_opt req (scheme_id, slug, org_id) |> Result.map_error db_error
        in
        match row_opt with
        | None -> Ok None
        | Some row -> decode_row row |> Result.map Option.some)
    | Project {project_id; org_id = Some org_id; _} -> (
        let req =
          Caqti_request.Infix.(
            Caqti_type.(t2 (t3 int string int) int) ->? row_type)
            (visible_select
           ^ " WHERE scheme_id = ? AND slug = ? AND (scope_kind = 'global' OR \
              (scope_kind = 'organization' AND org_id = ?) OR (scope_kind = \
              'project' AND project_id = ?))" ^ order)
        in
        let* row_opt =
          Db.find_opt req ((scheme_id, slug, org_id), project_id)
          |> Result.map_error db_error
        in
        match row_opt with
        | None -> Ok None
        | Some row -> decode_row row |> Result.map Option.some)
    | Project {project_id; org_id = None; _} -> (
        let req =
          Caqti_request.Infix.(Caqti_type.(t3 int string int) ->? row_type)
            (visible_select
           ^ " WHERE scheme_id = ? AND slug = ? AND (scope_kind = 'global' OR \
              (scope_kind = 'project' AND project_id = ?))" ^ order)
        in
        let* row_opt =
          Db.find_opt req (scheme_id, slug, project_id)
          |> Result.map_error db_error
        in
        match row_opt with
        | None -> Ok None
        | Some row -> decode_row row |> Result.map Option.some)
end

module Concept_label_store = struct
  let ( let* ) = Result.bind

  type label_kind =
    | Label_preferred
    | Label_alternate
    | Label_hidden
    | Label_deprecated

  type staleness_status = Active | Deprecated | Stale

  type label_row = {
    id : int;
    concept_id : int;
    text : string;
    kind : label_kind;
    staleness_status : staleness_status;
    last_marked_at : Ptime.t option;
    created_at : Ptime.t;
  }

  let db_error = Caqti_error.show

  let kind_to_string = function
    | Label_preferred -> "preferred"
    | Label_alternate -> "alternate"
    | Label_hidden -> "hidden"
    | Label_deprecated -> "deprecated"

  let kind_of_string = function
    | "preferred" -> Ok Label_preferred
    | "alternate" -> Ok Label_alternate
    | "hidden" -> Ok Label_hidden
    | "deprecated" -> Ok Label_deprecated
    | value -> Error (Printf.sprintf "unknown label kind %S" value)

  let status_to_string = function
    | Active -> "active"
    | Deprecated -> "deprecated"
    | Stale -> "stale"

  let status_of_string = function
    | "active" -> Ok Active
    | "deprecated" -> Ok Deprecated
    | "stale" -> Ok Stale
    | value -> Error (Printf.sprintf "unknown staleness status %S" value)

  let row_type =
    Caqti_type.(
      t2 (t4 int int string string) (t3 string (option string) string))

  let decode_row ((id, concept_id, text, kind_s), (status_s, last_s, created_s))
      =
    let* kind = kind_of_string kind_s in
    let* staleness_status = status_of_string status_s in
    let* last_marked_at =
      match last_s with
      | None -> Ok None
      | Some value -> Time_codec.of_string value |> Result.map Option.some
    in
    let* created_at = Time_codec.of_string created_s in
    Ok
      {id; concept_id; text; kind; staleness_status; last_marked_at; created_at}

  let map_rows result =
    let* rows = result |> Result.map_error db_error in
    List.fold_right
      (fun row acc ->
        let* decoded = decode_row row in
        let* rest = acc in
        Ok (decoded :: rest))
      rows
      (Ok [])

  let visible_concept_sql predicate =
    "SELECT id FROM concepts WHERE id = ? AND " ^ predicate ^ " LIMIT 1"

  let concept_visible (module Db : Caqti_eio.CONNECTION) ~scope ~concept_id =
    match scope with
    | Authorized_scope.Global _ ->
        let req =
          Caqti_request.Infix.(Caqti_type.int ->? Caqti_type.int)
            (visible_concept_sql "scope_kind = 'global'")
        in
        Db.find_opt req concept_id |> Result.map_error db_error
        |> Result.map Option.is_some
    | Organization {org_id; _} ->
        let req =
          Caqti_request.Infix.(Caqti_type.(t2 int int) ->? Caqti_type.int)
            (visible_concept_sql
               "(scope_kind = 'global' OR (scope_kind = 'organization' AND \
                org_id = ?))")
        in
        Db.find_opt req (concept_id, org_id)
        |> Result.map_error db_error |> Result.map Option.is_some
    | Project {project_id; org_id = Some org_id; _} ->
        let req =
          Caqti_request.Infix.(Caqti_type.(t3 int int int) ->? Caqti_type.int)
            (visible_concept_sql
               "(scope_kind = 'global' OR (scope_kind = 'organization' AND \
                org_id = ?) OR (scope_kind = 'project' AND project_id = ?))")
        in
        Db.find_opt req (concept_id, org_id, project_id)
        |> Result.map_error db_error |> Result.map Option.is_some
    | Project {project_id; org_id = None; _} ->
        let req =
          Caqti_request.Infix.(Caqti_type.(t2 int int) ->? Caqti_type.int)
            (visible_concept_sql
               "(scope_kind = 'global' OR (scope_kind = 'project' AND \
                project_id = ?))")
        in
        Db.find_opt req (concept_id, project_id)
        |> Result.map_error db_error |> Result.map Option.is_some

  let get_by_id (module Db : Caqti_eio.CONNECTION) id =
    let req =
      Caqti_request.Infix.(Caqti_type.int ->? row_type)
        {|SELECT id, concept_id, text, kind,
               staleness_status, last_marked_at, created_at
          FROM concept_labels
         WHERE id = ?|}
    in
    let* row_opt = Db.find_opt req id |> Result.map_error db_error in
    match row_opt with
    | None -> Ok None
    | Some row -> decode_row row |> Result.map Option.some

  let create (module Db : Caqti_eio.CONNECTION) ~scope ~concept_id ~text ~kind =
    let* is_visible = concept_visible (module Db) ~scope ~concept_id in
    if not is_visible then
      Error "concept is not visible in the authorized scope"
    else
      let req =
        Caqti_request.Infix.(
          Caqti_type.(t3 int string string) ->. Caqti_type.unit)
          {|INSERT INTO concept_labels (concept_id, text, kind)
          VALUES (?, ?, ?)|}
      in
      let* () =
        Db.exec req (concept_id, text, kind_to_string kind)
        |> Result.map_error db_error
      in
      let id_req =
        Caqti_request.Infix.(Caqti_type.unit ->! Caqti_type.int)
          "SELECT last_insert_rowid()"
      in
      let* id = Db.find id_req () |> Result.map_error db_error in
      match get_by_id (module Db) id with
      | Ok (Some row) -> Ok row
      | Ok None -> Error "created concept label could not be reloaded"
      | Error e -> Error e

  let list_by_concept (module Db : Caqti_eio.CONNECTION) ~scope ~concept_id =
    let* is_visible = concept_visible (module Db) ~scope ~concept_id in
    if not is_visible then Ok []
    else
      let req =
        Caqti_request.Infix.(Caqti_type.int ->* row_type)
          {|SELECT id, concept_id, text, kind,
                 staleness_status, last_marked_at, created_at
            FROM concept_labels
           WHERE concept_id = ?
           ORDER BY id ASC|}
      in
      map_rows (Db.collect_list req concept_id)

  let mark_stale (module Db : Caqti_eio.CONNECTION) ~scope ~label_id ~status =
    let lookup_req =
      Caqti_request.Infix.(Caqti_type.int ->? Caqti_type.int)
        "SELECT concept_id FROM concept_labels WHERE id = ?"
    in
    let* concept_id_opt =
      Db.find_opt lookup_req label_id |> Result.map_error db_error
    in
    match concept_id_opt with
    | None -> Error "concept label not found"
    | Some concept_id ->
        let* is_visible = concept_visible (module Db) ~scope ~concept_id in
        if not is_visible then
          Error "concept label is not visible in the authorized scope"
        else
          let marked_at = Time_codec.to_string (Ptime_clock.now ()) in
          let req =
            Caqti_request.Infix.(
              Caqti_type.(t3 string string int) ->. Caqti_type.unit)
              {|UPDATE concept_labels
                 SET staleness_status = ?, last_marked_at = ?
               WHERE id = ?|}
          in
          Db.exec req (status_to_string status, marked_at, label_id)
          |> Result.map_error db_error
end

module Concept_relation_store = struct
  let ( let* ) = Result.bind

  type relation_type_row = {
    id : int;
    slug : string;
    is_hierarchical : bool;
    is_traversal_enabled : bool;
  }

  type relation_row = {
    id : int;
    from_concept_id : int;
    to_concept_id : int;
    relation_type_id : int;
    is_active : bool;
    created_at : Ptime.t;
  }

  let db_error = Caqti_error.show

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

  let bool_of_int n = n <> 0

  let int_of_bool b = if b then 1 else 0

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

  let map_insert_relation_error error =
    let msg = db_error error in
    if contains_substring msg "UNIQUE constraint failed" then
      Error "duplicate active relation"
    else Error msg

  let relation_type_type = Caqti_type.(t4 int string int int)

  let decode_relation_type (id, slug, is_hierarchical, is_traversal_enabled) =
    {
      id;
      slug;
      is_hierarchical = bool_of_int is_hierarchical;
      is_traversal_enabled = bool_of_int is_traversal_enabled;
    }

  let relation_type_by_id (module Db : Caqti_eio.CONNECTION) relation_type_id =
    let req =
      Caqti_request.Infix.(Caqti_type.int ->? relation_type_type)
        {|SELECT id, slug, is_hierarchical, is_traversal_enabled
          FROM concept_relation_types
         WHERE id = ?|}
    in
    Db.find_opt req relation_type_id
    |> Result.map_error db_error
    |> Result.map (Option.map decode_relation_type)

  let list_relation_types (module Db : Caqti_eio.CONNECTION) =
    let req =
      Caqti_request.Infix.(Caqti_type.unit ->* relation_type_type)
        {|SELECT id, slug, is_hierarchical, is_traversal_enabled
          FROM concept_relation_types
         ORDER BY id ASC|}
    in
    Db.collect_list req () |> Result.map_error db_error
    |> Result.map (List.map decode_relation_type)

  let row_type = Caqti_type.(t2 (t5 int int int int int) string)

  let decode_row
      ((id, from_concept_id, to_concept_id, relation_type_id, active), created_s)
      =
    let* created_at = Time_codec.of_string created_s in
    Ok
      {
        id;
        from_concept_id;
        to_concept_id;
        relation_type_id;
        is_active = bool_of_int active;
        created_at;
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

  let get_by_id (module Db : Caqti_eio.CONNECTION) id =
    let req =
      Caqti_request.Infix.(Caqti_type.int ->? row_type)
        {|SELECT id, from_concept_id, to_concept_id, relation_type_id,
               is_active, created_at
          FROM concept_relations
         WHERE id = ?|}
    in
    let* row_opt = Db.find_opt req id |> Result.map_error db_error in
    match row_opt with
    | None -> Ok None
    | Some row -> decode_row row |> Result.map Option.some

  let concept_visible conn ~scope ~concept_id =
    Concept_label_store.concept_visible conn ~scope ~concept_id

  let cycle_exists (module Db : Caqti_eio.CONNECTION) ~from_concept_id
      ~to_concept_id ~relation_type_id =
    let req =
      Caqti_request.Infix.(Caqti_type.(t3 int int int) ->? Caqti_type.int)
        {|WITH RECURSIVE reachable(cid) AS (
          SELECT ? AS cid
          UNION
          SELECT cr.to_concept_id
            FROM concept_relations cr
            JOIN reachable r ON cr.from_concept_id = r.cid
           WHERE cr.is_active = 1
             AND cr.relation_type_id = ?
        )
        SELECT 1 FROM reachable WHERE cid = ? LIMIT 1|}
    in
    Db.find_opt req (to_concept_id, relation_type_id, from_concept_id)
    |> Result.map_error db_error |> Result.map Option.is_some

  (** [create_relation_in_tx] assumes the caller already holds the surrounding
      [BEGIN IMMEDIATE] transaction. It performs the same visibility, cycle, and
      duplicate-active checks as the public wrapper without opening a nested
      transaction. *)
  let create_relation_in_tx ((module Db : Caqti_eio.CONNECTION) as conn) ~scope
      ~from_concept_id ~to_concept_id ~relation_type_id =
    let* from_visible =
      concept_visible conn ~scope ~concept_id:from_concept_id
    in
    let* to_visible = concept_visible conn ~scope ~concept_id:to_concept_id in
    if (not from_visible) || not to_visible then
      Error "relation concepts are not visible in the authorized scope"
    else
      let* relation_type_opt = relation_type_by_id conn relation_type_id in
      match relation_type_opt with
      | None -> Error "concept relation type not found"
      | Some relation_type -> (
          let* has_cycle =
            if relation_type.is_hierarchical then
              cycle_exists
                conn
                ~from_concept_id
                ~to_concept_id
                ~relation_type_id
            else Ok false
          in
          if has_cycle then
            Error
              (Printf.sprintf
                 "Cycle detected: adding relation from %d to %d would create a \
                  cycle"
                 from_concept_id
                 to_concept_id)
          else
            let req =
              Caqti_request.Infix.(
                Caqti_type.(t4 int int int int) ->. Caqti_type.unit)
                {|INSERT INTO concept_relations
                  (from_concept_id, to_concept_id, relation_type_id, is_active)
                VALUES (?, ?, ?, ?)|}
            in
            let* () =
              Db.exec
                req
                ( from_concept_id,
                  to_concept_id,
                  relation_type_id,
                  int_of_bool true )
              |> function
              | Ok () -> Ok ()
              | Error e -> map_insert_relation_error e
            in
            let id_req =
              Caqti_request.Infix.(Caqti_type.unit ->! Caqti_type.int)
                "SELECT last_insert_rowid()"
            in
            let* id = Db.find id_req () |> Result.map_error db_error in
            match get_by_id conn id with
            | Ok (Some row) -> Ok row
            | Ok None -> Error "created concept relation could not be reloaded"
            | Error e -> Error e)

  (** [create_relation] inserts one active concept relation after validating
    hierarchical cycle safety. Hierarchical-relation inserts use a single
    [BEGIN IMMEDIATE] transaction: the cycle check and insert share one atomic
    critical section, and SQLite serialises this writer with all other writers
    project-wide until [COMMIT] or [ROLLBACK] (readers remain unaffected under
    WAL). The recursive CTE intentionally has no early short-circuit: SQLite
    materialises the full forward closure from [to_concept_id] for the selected
    relation type before applying the final filter. The traversal join is backed
    by the composite [idx_concept_relations_traverse] index on
    [(relation_type_id, from_concept_id, is_active)]. Duplicate active triples
    are rejected as [Error "duplicate active relation"]. *)
  let create_relation conn ~scope ~from_concept_id ~to_concept_id
      ~relation_type_id =
    with_begin_immediate conn @@ fun () ->
    create_relation_in_tx
      conn
      ~scope
      ~from_concept_id
      ~to_concept_id
      ~relation_type_id

  let list_relations_by_concept ((module Db : Caqti_eio.CONNECTION) as conn)
      ~scope ~concept_id =
    let* is_visible = concept_visible conn ~scope ~concept_id in
    if not is_visible then Ok []
    else
      let req =
        Caqti_request.Infix.(Caqti_type.int ->* row_type)
          {|SELECT id, from_concept_id, to_concept_id, relation_type_id,
                 is_active, created_at
            FROM concept_relations
           WHERE is_active = 1 AND from_concept_id = ?
           ORDER BY id ASC|}
      in
      map_rows (Db.collect_list req concept_id)

  let list_incoming_relations ((module Db : Caqti_eio.CONNECTION) as conn)
      ~scope ~concept_id =
    let* is_visible = concept_visible conn ~scope ~concept_id in
    if not is_visible then Ok []
    else
      let req =
        Caqti_request.Infix.(Caqti_type.int ->* row_type)
          {|SELECT id, from_concept_id, to_concept_id, relation_type_id,
                 is_active, created_at
            FROM concept_relations
           WHERE is_active = 1 AND to_concept_id = ?
           ORDER BY id ASC|}
      in
      map_rows (Db.collect_list req concept_id)
end

module Concept_search_store = struct
  let ( let* ) = Result.bind

  type search_result = {concept_id : int; rank : float}

  let db_error = Caqti_error.show

  let escape_fts_phrase query =
    let escaped = String.concat "\"\"" (String.split_on_char '"' query) in
    "\"" ^ escaped ^ "\""

  let row_type = Caqti_type.(t2 int float)

  let sql filter =
    "SELECT f.concept_id, bm25(concept_search_fts) AS rank FROM \
     concept_search_fts f JOIN concepts c ON c.id = f.concept_id WHERE \
     concept_search_fts MATCH ? AND " ^ filter ^ " ORDER BY rank ASC LIMIT ?"

  let map_results result =
    result |> Result.map_error db_error
    |> Result.map (List.map (fun (concept_id, rank) -> {concept_id; rank}))

  let search (module Db : Caqti_eio.CONNECTION) ~scope ~query ~limit =
    if limit < 0 then Error "limit must be >= 0"
    else if String.trim query = "" then Ok []
    else
      let query = escape_fts_phrase query in
      match scope with
      | Authorized_scope.Global _ ->
          let req =
            Caqti_request.Infix.(Caqti_type.(t2 string int) ->* row_type)
              (sql "c.scope_kind = 'global'")
          in
          map_results (Db.collect_list req (query, limit))
      | Organization {org_id; _} ->
          let req =
            Caqti_request.Infix.(Caqti_type.(t3 string int int) ->* row_type)
              (sql
                 "(c.scope_kind = 'global' OR (c.scope_kind = 'organization' \
                  AND c.org_id = ?))")
          in
          map_results (Db.collect_list req (query, org_id, limit))
      | Project {project_id; org_id = Some org_id; _} ->
          let req =
            Caqti_request.Infix.(
              Caqti_type.(t4 string int int int) ->* row_type)
              (sql
                 "(c.scope_kind = 'global' OR (c.scope_kind = 'organization' \
                  AND c.org_id = ?) OR (c.scope_kind = 'project' AND \
                  c.project_id = ?))")
          in
          map_results (Db.collect_list req (query, org_id, project_id, limit))
      | Project {project_id; org_id = None; _} ->
          let req =
            Caqti_request.Infix.(Caqti_type.(t3 string int int) ->* row_type)
              (sql
                 "(c.scope_kind = 'global' OR (c.scope_kind = 'project' AND \
                  c.project_id = ?))")
          in
          map_results (Db.collect_list req (query, project_id, limit))
end

module Builtin_vocabulary_seed = struct
  let ( let* ) = Result.bind

  let seed_json = [%blob "seed/builtin_vocabulary.json"]

  type relation_type_seed = {
    rt_slug : string;
    is_hierarchical : bool;
    is_traversal_enabled : bool;
  }

  type scheme_seed = {
    scheme_slug : string;
    display_name : string;
    description : string option;
    scope_kind : string;
  }

  type concept_seed = {
    concept_scheme_slug : string;
    concept_slug : string;
    definition : string option;
    scope_note : string option;
  }

  type label_seed = {
    label_scheme_slug : string;
    label_concept_slug : string;
    text : string;
    kind : Concept_label_store.label_kind;
  }

  type relation_seed = {
    from_scheme_slug : string;
    from_concept_slug : string;
    to_scheme_slug : string;
    to_concept_slug : string;
    relation_type_slug : string;
  }

  type seed = {
    relation_types : relation_type_seed list;
    schemes : scheme_seed list;
    concepts : concept_seed list;
    labels : label_seed list;
    relations : relation_seed list;
  }

  let db_error = Caqti_error.show

  let int_of_bool b = if b then 1 else 0

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

  let field name fields = List.assoc_opt name fields

  let require_string name fields =
    match field name fields with
    | Some (`String value) -> Ok value
    | _ -> Error (Printf.sprintf "seed field %S must be a string" name)

  let optional_string name fields =
    match field name fields with
    | None | Some `Null -> Ok None
    | Some (`String value) -> Ok (Some value)
    | _ -> Error (Printf.sprintf "seed field %S must be a string or null" name)

  let require_bool name fields =
    match field name fields with
    | Some (`Bool value) -> Ok value
    | _ -> Error (Printf.sprintf "seed field %S must be a boolean" name)

  let require_object = function
    | `Assoc fields -> Ok fields
    | _ -> Error "seed array entry must be an object"

  let require_array name fields =
    match field name fields with
    | Some (`List values) -> Ok values
    | _ -> Error (Printf.sprintf "seed field %S must be an array" name)

  let decode_label_kind = function
    | "preferred" -> Ok Concept_label_store.Label_preferred
    | "alternate" -> Ok Label_alternate
    | "hidden" -> Ok Label_hidden
    | "deprecated" -> Ok Label_deprecated
    | value -> Error (Printf.sprintf "unknown seed label kind %S" value)

  let decode_relation_type json =
    let* fields = require_object json in
    let* rt_slug = require_string "slug" fields in
    let* is_hierarchical = require_bool "is_hierarchical" fields in
    let* is_traversal_enabled = require_bool "is_traversal_enabled" fields in
    Ok {rt_slug; is_hierarchical; is_traversal_enabled}

  let decode_scheme json =
    let* fields = require_object json in
    let* scheme_slug = require_string "slug" fields in
    let* display_name = require_string "display_name" fields in
    let* description = optional_string "description" fields in
    let* scope_kind = require_string "scope_kind" fields in
    Ok {scheme_slug; display_name; description; scope_kind}

  let decode_concept json =
    let* fields = require_object json in
    let* concept_scheme_slug = require_string "scheme_slug" fields in
    let* concept_slug = require_string "slug" fields in
    let* definition = optional_string "definition" fields in
    let* scope_note = optional_string "scope_note" fields in
    Ok {concept_scheme_slug; concept_slug; definition; scope_note}

  let decode_label json =
    let* fields = require_object json in
    let* label_scheme_slug = require_string "concept_scheme_slug" fields in
    let* label_concept_slug = require_string "concept_slug" fields in
    let* text = require_string "text" fields in
    let* kind_s = require_string "kind" fields in
    let* kind = decode_label_kind kind_s in
    Ok {label_scheme_slug; label_concept_slug; text; kind}

  let decode_relation json =
    let* fields = require_object json in
    let* from_scheme_slug = require_string "from_scheme_slug" fields in
    let* from_concept_slug = require_string "from_concept_slug" fields in
    let* to_scheme_slug = require_string "to_scheme_slug" fields in
    let* to_concept_slug = require_string "to_concept_slug" fields in
    let* relation_type_slug = require_string "relation_type_slug" fields in
    Ok
      {
        from_scheme_slug;
        from_concept_slug;
        to_scheme_slug;
        to_concept_slug;
        relation_type_slug;
      }

  let traverse decoder values =
    List.fold_right
      (fun json acc ->
        let* decoded = decoder json in
        let* rest = acc in
        Ok (decoded :: rest))
      values
      (Ok [])

  let decode_seed json =
    let* fields = require_object json in
    let* relation_type_json = require_array "relation_types" fields in
    let* relation_types = traverse decode_relation_type relation_type_json in
    let* scheme_json = require_array "schemes" fields in
    let* schemes = traverse decode_scheme scheme_json in
    let* concept_json = require_array "concepts" fields in
    let* concepts = traverse decode_concept concept_json in
    let* label_json = require_array "labels" fields in
    let* labels = traverse decode_label label_json in
    let* relation_json = require_array "relations" fields in
    let* relations = traverse decode_relation relation_json in
    Ok {relation_types; schemes; concepts; labels; relations}

  let parse_seed () =
    try Yojson.Safe.from_string seed_json |> decode_seed
    with Yojson.Json_error msg ->
      Error ("invalid built-in vocabulary JSON: " ^ msg)

  let parse_seed_json seed_json =
    try Yojson.Safe.from_string seed_json |> decode_seed
    with Yojson.Json_error msg -> Error ("invalid vocabulary JSON: " ^ msg)

  let find_builtin_scheme_id (module Db : Caqti_eio.CONNECTION) slug =
    let req =
      Caqti_request.Infix.(Caqti_type.string ->? Caqti_type.int)
        {|SELECT id FROM concept_schemes
         WHERE provenance = 'epure_builtin' AND slug = ?
         ORDER BY id ASC LIMIT 1|}
    in
    Db.find_opt req slug |> Result.map_error db_error

  let require_builtin_scheme_id conn slug =
    let* id_opt = find_builtin_scheme_id conn slug in
    match id_opt with
    | Some id -> Ok id
    | None ->
        Error (Printf.sprintf "seed scheme %S was not found after upsert" slug)

  let find_builtin_concept_id (module Db : Caqti_eio.CONNECTION) ~scheme_id
      ~slug =
    let req =
      Caqti_request.Infix.(Caqti_type.(t2 int string) ->? Caqti_type.int)
        {|SELECT id FROM concepts
         WHERE provenance = 'epure_builtin' AND scheme_id = ? AND slug = ?
         ORDER BY id ASC LIMIT 1|}
    in
    Db.find_opt req (scheme_id, slug) |> Result.map_error db_error

  let require_builtin_concept_id conn ~scheme_slug ~concept_slug =
    let* scheme_id = require_builtin_scheme_id conn scheme_slug in
    let* id_opt = find_builtin_concept_id conn ~scheme_id ~slug:concept_slug in
    match id_opt with
    | Some id -> Ok id
    | None ->
        Error
          (Printf.sprintf
             "seed concept %S/%S was not found after upsert"
             scheme_slug
             concept_slug)

  let upsert_relation_type (module Db : Caqti_eio.CONNECTION) row =
    let req =
      Caqti_request.Infix.(Caqti_type.(t3 string int int) ->. Caqti_type.unit)
        {|INSERT INTO concept_relation_types
          (slug, is_hierarchical, is_traversal_enabled)
        VALUES (?, ?, ?)
        ON CONFLICT(slug) DO UPDATE SET
          is_hierarchical = excluded.is_hierarchical,
          is_traversal_enabled = excluded.is_traversal_enabled|}
    in
    Db.exec
      req
      ( row.rt_slug,
        int_of_bool row.is_hierarchical,
        int_of_bool row.is_traversal_enabled )
    |> Result.map_error db_error

  let upsert_scheme ((module Db : Caqti_eio.CONNECTION) as conn) row =
    let scope =
      match row.scope_kind with
      | "global" -> Ok (Authorized_scope.Global {actor_id = None})
      | "organization" ->
          Error "built-in organization-scoped seeds require org_id"
      | "project" -> Error "built-in project-scoped seeds require project_id"
      | value -> Error (Printf.sprintf "unknown seed scope_kind %S" value)
    in
    let* scope = scope in
    let scope_kind, org_id, project_id =
      Concept_scheme_store.scope_to_db_columns scope
    in
    let* existing_id = find_builtin_scheme_id conn row.scheme_slug in
    match existing_id with
    | Some id ->
        let req =
          Caqti_request.Infix.(
            Caqti_type.(
              t2
                (t4 string (option string) string (option int))
                (t2 (option int) int))
            ->. Caqti_type.unit)
            {|UPDATE concept_schemes
               SET display_name = ?, description = ?, scope_kind = ?, org_id = ?, project_id = ?
             WHERE id = ? AND provenance = 'epure_builtin'|}
        in
        Db.exec
          req
          ( (row.display_name, row.description, scope_kind, org_id),
            (project_id, id) )
        |> Result.map_error db_error
    | None ->
        let* (_ : Concept_scheme_store.scheme_row) =
          Concept_scheme_store.create
            conn
            ~scope
            ~slug:row.scheme_slug
            ~display_name:row.display_name
            ?description:row.description
            ~provenance:Provenance_codec.Epure_builtin
            ()
        in
        Ok ()

  let upsert_concept ((module Db : Caqti_eio.CONNECTION) as conn) row =
    let* scheme_id = require_builtin_scheme_id conn row.concept_scheme_slug in
    let* scheme =
      Result.bind (Concept_scheme_store.get_by_id conn scheme_id) (function
        | Some scheme -> Ok scheme
        | None -> Error "built-in seed scheme disappeared during concept upsert")
    in
    let* existing_id =
      find_builtin_concept_id conn ~scheme_id ~slug:row.concept_slug
    in
    match existing_id with
    | Some id ->
        let req =
          Caqti_request.Infix.(
            Caqti_type.(t4 (option string) (option string) int int)
            ->. Caqti_type.unit)
            {|UPDATE concepts
               SET definition = ?, scope_note = ?
             WHERE scheme_id = ? AND id = ? AND provenance = 'epure_builtin'|}
        in
        Db.exec req (row.definition, row.scope_note, scheme_id, id)
        |> Result.map_error db_error
    | None ->
        let* (_ : Concept_store.concept_row) =
          Concept_store.create
            conn
            ~scope:scheme.scope
            ~scheme_id
            ~slug:row.concept_slug
            ?definition:row.definition
            ?scope_note:row.scope_note
            ~provenance:Provenance_codec.Epure_builtin
            ()
        in
        Ok ()

  let upsert_label ((module Db : Caqti_eio.CONNECTION) as conn) row =
    let* concept_id =
      require_builtin_concept_id
        conn
        ~scheme_slug:row.label_scheme_slug
        ~concept_slug:row.label_concept_slug
    in
    let req_find =
      Caqti_request.Infix.(Caqti_type.(t3 int string string) ->? Caqti_type.int)
        {|SELECT id FROM concept_labels
         WHERE concept_id = ? AND text = ? AND kind = ?
         ORDER BY id ASC LIMIT 1|}
    in
    let* existing_id =
      Db.find_opt
        req_find
        (concept_id, row.text, Concept_label_store.kind_to_string row.kind)
      |> Result.map_error db_error
    in
    match existing_id with
    | Some _ -> Ok ()
    | None ->
        let* concept =
          Result.bind (Concept_store.get_by_id conn concept_id) (function
            | Some concept -> Ok concept
            | None ->
                Error "built-in seed concept disappeared during label upsert")
        in
        let* (_ : Concept_label_store.label_row) =
          Concept_label_store.create
            conn
            ~scope:concept.scope
            ~concept_id
            ~text:row.text
            ~kind:row.kind
        in
        Ok ()

  let relation_type_id_by_slug (module Db : Caqti_eio.CONNECTION) slug =
    let req =
      Caqti_request.Infix.(Caqti_type.string ->? Caqti_type.int)
        "SELECT id FROM concept_relation_types WHERE slug = ?"
    in
    Db.find_opt req slug |> Result.map_error db_error

  let upsert_relation ((module Db : Caqti_eio.CONNECTION) as conn) row =
    let* from_concept_id =
      require_builtin_concept_id
        conn
        ~scheme_slug:row.from_scheme_slug
        ~concept_slug:row.from_concept_slug
    in
    let* to_concept_id =
      require_builtin_concept_id
        conn
        ~scheme_slug:row.to_scheme_slug
        ~concept_slug:row.to_concept_slug
    in
    let* relation_type_id =
      Result.bind
        (relation_type_id_by_slug conn row.relation_type_slug)
        (function
        | Some id -> Ok id
        | None ->
            Error
              (Printf.sprintf
                 "seed relation type %S not found"
                 row.relation_type_slug))
    in
    let find_req =
      Caqti_request.Infix.(Caqti_type.(t3 int int int) ->? Caqti_type.int)
        {|SELECT id FROM concept_relations
          WHERE from_concept_id = ? AND to_concept_id = ? AND relation_type_id = ?
            AND is_active = 1
          ORDER BY id ASC LIMIT 1|}
    in
    let* existing =
      Db.find_opt find_req (from_concept_id, to_concept_id, relation_type_id)
      |> Result.map_error db_error
    in
    match existing with
    | Some _ -> Ok ()
    | None ->
        let* concept =
          Result.bind (Concept_store.get_by_id conn from_concept_id) (function
            | Some concept -> Ok concept
            | None -> Error "built-in seed relation source disappeared")
        in
        let* (_ : Concept_relation_store.relation_row) =
          Concept_relation_store.create_relation_in_tx
            conn
            ~scope:concept.scope
            ~from_concept_id
            ~to_concept_id
            ~relation_type_id
        in
        Ok ()

  let run_with_seed conn seed =
    with_begin_immediate conn @@ fun () ->
    let* () =
      List.fold_left
        (fun acc row ->
          let* () = acc in
          upsert_relation_type conn row)
        (Ok ())
        seed.relation_types
    in
    let* () =
      List.fold_left
        (fun acc row ->
          let* () = acc in
          upsert_scheme conn row)
        (Ok ())
        seed.schemes
    in
    let* () =
      List.fold_left
        (fun acc row ->
          let* () = acc in
          upsert_concept conn row)
        (Ok ())
        seed.concepts
    in
    let* () =
      List.fold_left
        (fun acc row ->
          let* () = acc in
          upsert_label conn row)
        (Ok ())
        seed.labels
    in
    List.fold_left
      (fun acc row ->
        let* () = acc in
        upsert_relation conn row)
      (Ok ())
      seed.relations

  let run conn =
    let* seed = parse_seed () in
    run_with_seed conn seed
end
