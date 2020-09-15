open Core
open Yojson.Basic

let mems = ["alloc"; "free"; "store"; "load"; "ptradd"]

let in_mems op = List.mem mems op ~equal:String.equal

let count (file : string) = 
  let functions = from_file file |> Util.member "functions" |> Util.to_list in
  let ct = List.fold functions ~init:0 ~f:(fun ct f -> 
      let ins = Util.member "instrs" f |> Util.to_list in 
      List.fold ins ~init:ct ~f:(fun ct i-> match  
                           Util.member "op" i with 
                       | `String s when in_mems s -> ct + 1
                       | _ -> ct)) in Printf.fprintf stdout "Saw %d memory instructions!" ct