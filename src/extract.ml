open Core
open Yojson.Basic

(* let block_tbl = Hashtbl.create(module String)*)

let terminators = ["br"; "jmp"; "ret"]

let in_terminator op = List.mem terminators op ~equal:String.equal

let blocker body =
  let res = ref [] in 
  let cur = ref [] in
  match body with
  | ("op", op) as inst when in_terminator (to_string op)
    -> (cur := inst::!cur; res := !cur; cur := []; !res)
  | ("op", _) as inst 
    -> (cur := inst::!cur; res := !cur; cur := inst::[]; !res)
  | _ as inst -> (cur := inst::!cur; res := !cur; !res)

let mycfg (file : string) = 
  let program = from_file file in 
  (* let () = to_channel stdout program in *)
  match program with 
  | `Assoc fs -> List.iter fs ~f:(fun fn ->
      (match fn with
       | ("functions", fns) -> 
         (match fns with 
          | `List funcs -> List.iter funcs ~f:(fun instr -> 
              (match instr with 
               | `Assoc ins -> List.iter ins ~f:(fun inz -> 
                   (match inz with 
                    | ("instrs", _) as instrs -> let blocks = blocker instrs in 
                      List.iter blocks ~f:(fun b -> to_channel stdout (`Assoc [b]))
                    | _ -> ()))
               | _ -> failwith "invalid program: instructions not found")) 
          | _ -> failwith "invalid program: not a list of instructions") 
       | _ -> failwith "invalid program structure: functions not found"))
  | _-> failwith "invalid program structure: not a list of fns"