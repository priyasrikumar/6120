open Core 
open Cfg 

let update_defs var lbl defs =
  Hashtbl.update defs var (function
      | None -> let set = Hash_set.create (module String) in
        Hash_set.add set lbl
      | Some set -> Hash_set.add set lbl)

let vars_of_process_defs lbl defs block =
  List.fold_left block ~init:[] ~f:(fun acc instr ->
      match instr with 
      | Label _ -> acc
      | Cst (dst, _, _) -> 
        update_defs dst lbl defs; 
        dst :: acc
      | Binop (dst, _, arg1, arg2) ->
        update_defs dst lbl defs; 
        dst :: arg1 :: arg2 :: acc
      | Unop (dst, _, _, _, _) -> 
        update_defs dst lbl defs;
        dst :: acc
      | Jmp _ -> acc
      | Br (arg, _, _) -> arg :: acc
      | Call (Some (dst), _, _, Some (args)) ->
        update_defs dst lbl defs
          dst :: args @ acc
      | Call (None, _, _, Some (args)) -> args @ acc
      | Call (Some (dst), _, _, None) -> 
        update_defs dst lbl defs; 
        dst :: acc
      | Call (None, _, _, None) -> acc
      | Ret (Some (arg)) -> arg :: acc
      | Ret (None) -> acc
      | Nop -> acc
      | Print (args) -> args @ acc
      | Phi (_, arg1, _, arg2) -> acc 
    ) 

let insert_nodes prog blocks df =
  let defs = Hashtbl.create (module String) in
  let vars = List.concat_map blocks ~f:(fun (lbl,instrs) ->
      vars_of_process_defs lbl defs instrs) |> List.stable_dedup
  in
  List.iter prog ~f:(fun func ->
      match func.args with
      | None -> ()
      | Some (args) -> List.iter args ~f:(fun (arg,_) ->
          update_defs arg func.name defs)
    );
  let block_map = Hashtbl.of_alist_multi blocks in 
  List.iter vars ~f:(fun v ->
      let defs_v = Hashtbl.find_exn defs v in 
      Hash_set.iter defs_v ~f:(fun d -> 
          let df = Hashtbl.find_exn df d in
          Hash_set.iter df ~f:(fun lbl ->      
              let has_phi = List.fold_left instrs ~init:false ~f:(fun acc instr ->
                  match instr with
                  | Phi (_, v', _, v'') ->
                    if String.equals v v' && String.equals v v'' then true
                    else acc || false
                  | _ -> acc || false)
              in 
              if has_phi |> not then 
                Hashtbl.update block_map lbl ~f:(function
                    | None -> failwith "should be unreachable"
                    | Some (instrs) -> Phi ("", v, "", v) :: instrs
                  ); 
              Hash_set.add defs_v lbl)));
  block_map

let rename_vars block stack = 
  List.iter (snd block) ~f:(function
      | Label lbl -> 
      | Cst (dst, typ, cst)
      | Binop (dst, typ, arg1, arg2) -> 
      let nw = "" in 
      let arg1_stack = Hashtbl.find_exn stack arg1 in 
      let arg2_stack = Hashtbl.find_exn stack arg2 in 
      Binop(nw, typ, Stack.find arg1_stack ~f:(fun name -> String.equal name arg1).
      Stack.find arg2_stack ~f:(fun name -> String.equal name arg2))
      | Unop (dst, typ, arg)
      | Jmp l 
      | Br (arg, lbl1, lbl2)
      | Call (Some (dst), _, _, Some (args)) ->
      | Call (None, _, _, Some (args)) -> 
      | Call (Some (dst), _, _, None) -> 
      | Call (None, _, _, None) -> 
      | Ret (Some arg)
      | Ret None
      | Nop 
      | Print args 
      | Phi lst 
    )