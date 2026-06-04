(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

let ( let* ) = Result.bind

type row = {
  package_name : string;
  api_version : int;
  created_at : string;
  updated_at : string;
}

let exec_sql (module Db : Caqti_eio.CONNECTION) sql =
  let req = Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit) sql in
  Db.exec req () |> Result.map_error Caqti_error.show

let row_type = Caqti_type.(t4 string int string string)

let decode_row (package_name, api_version, created_at, updated_at) =
  {package_name; api_version; created_at; updated_at}

let provision conn =
  List.fold_left
    (fun acc sql ->
      let* () = acc in
      exec_sql conn sql)
    (Ok ())
    Schema_fragments.initial_schema_v1_ddl

let get (module Db : Caqti_eio.CONNECTION) =
  let req =
    Caqti_request.Infix.(Caqti_type.unit ->? row_type)
      {|SELECT package_name, api_version, created_at, updated_at
          FROM law_package_metadata
         WHERE singleton_key = 'epure-law'|}
  in
  Db.find_opt req ()
  |> Result.map_error Caqti_error.show
  |> Result.map (Option.map decode_row)
