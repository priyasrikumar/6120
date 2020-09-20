open Types.Bril_types

exception Not_yet_implemented of string

val parse_prog : string -> prog

val to_json : prog -> Yojson.Basic.t