open Types

exception Not_yet_implemented of string

val parse_prog : string -> prog

val parse_in : prog

val to_json : prog -> Yojson.Basic.t