open Bril 
open Core

let block_map blocks = Sequence.of_list blocks |> Sequence.to_seq |> Stdlib.Hashtbl.of_seq 

let tdce blocks _block_map _cfg funcs = 
  List.map blocks 
    ~f:(fun (name, block) -> 
        if Hashtbl.mem funcs name then 
          let used_vars = Hashtbl.create (module String) in 
          List.fold block ~init:None ~f:(fun _acc instr -> match instr with 
              | Label _ -> None 
              | Const (dest, _) -> begin
                  ignore(Hashtbl.add used_vars ~key:(fst dest) ~data:false); 
                  None end
              | Binary (dest, _, arg1, arg2) -> begin
                  ignore(Hashtbl.add used_vars ~key:arg1 ~data:true); 
                  ignore(Hashtbl.add used_vars ~key:arg2 ~data:true); 
                  ignore(Hashtbl.add used_vars ~key:(fst dest) ~data:false); 
                  None end
              | Unary (dest, _, arg) -> begin
                  ignore(Hashtbl.add used_vars ~key:(fst dest) ~data:false);
                  ignore(Hashtbl.add used_vars ~key:arg ~data:true); 
                  None end
              | Jmp label -> begin
(*let jumped = Hashtbl.find cfg label in *)
                  Some [label]
                end
              | Br (arg, label1, label2) -> begin
                  ignore(Hashtbl.add used_vars ~key:arg ~data:true);
                  Some [label1; label2]
                end
              | Call (dest, _func_name, args) -> begin 
                  List.iter args 
                    ~f:(fun arg -> ignore(Hashtbl.add used_vars ~key:arg ~data:true));
                  ignore(Hashtbl.add used_vars ~key:(fst(Option.value ~default:("", IntType) dest)) ~data:false); 
                  None
                end
              | Ret arg -> begin
                  ignore(Hashtbl.add used_vars ~key:(Option.value ~default:"" arg) ~data:true); 
                  None
                end
              | Print args -> begin
                  List.iter args ~f:(fun arg -> ignore(Hashtbl.add used_vars ~key:arg ~data:true)); 
                  Some args
                end
              | Nop -> None) else None)
(* traverse CFG and end when code ends [ret],) else ()*)

(* if Ret, return none, else we keep processing labels until we reach none *)