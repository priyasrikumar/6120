open Core 
open Cfg 
open Types

let update_defs var lbl defs =
  Hashtbl.update defs var ~f:(function
      | None -> let set = Hash_set.create (module String) in
        Hash_set.add set lbl; set
      | Some set -> Hash_set.add set lbl; set)

let vars_of_process_defs lbl defs block =
  List.fold_left block ~init:[] ~f:(fun acc instr ->
      match instr with 
      | Label _ -> acc
      | Cst (dst, typ, _) -> 
        update_defs dst lbl defs; 
        (dst, typ) :: acc
      | Binop (dst, typ, _, _, _) ->
        update_defs dst lbl defs; 
        (dst, typ) :: acc
      | Unop (dst, typ, _, _) -> 
        update_defs dst lbl defs;
        (dst, typ) :: acc
      | Jmp _ -> acc
      | Br (_, _, _) -> acc
      | Call (Some (dst), Some (typ), _, _) ->
        update_defs dst lbl defs;
        (dst, typ) :: acc
      | Call (_, _, _, _) -> acc
      | Ret (_) -> acc
      | Nop -> acc
      | Print (_) -> acc
      | Phi (dst, typ, _phis) ->
        update_defs dst lbl defs;
        (dst,typ) :: acc 
    ) 

let insert_nodes prog blocks df =
  let defs = Hashtbl.create (module String) in
  let vars = List.concat_map blocks ~f:(fun (lbl,instrs) ->
      vars_of_process_defs lbl defs instrs)
  in
  let vars = List.fold_left prog ~init:vars ~f:(fun acc func ->
      match func.args with
      | None -> acc
      | Some (args) -> List.fold_left args ~init:acc ~f:(fun acc (arg,typ) ->
          update_defs arg func.name defs; (arg,typ) :: acc))
    |> List.stable_dedup
  in
  let block_map = Hashtbl.of_alist_exn (module String) blocks in 
  List.iter vars ~f:(fun (v,t) ->
      let defs_v = Hashtbl.find_exn defs v in 
      Hash_set.iter defs_v ~f:(fun d -> 
          let df = Hashtbl.find_exn df d in
          Hash_set.iter df ~f:(fun lbl ->
              let instrs = Hashtbl.find_exn block_map lbl in      
              let has_phi = List.fold_left instrs ~init:false ~f:(fun acc instr ->
                  match instr with
                  | Phi (v', _, _) when String.equal v v' -> true
                  | _ -> acc || false)
              in 
              if has_phi |> not then 
                Hashtbl.update block_map lbl ~f:(function
                    | None -> failwith "should be unreachable"
                    | Some (instrs) -> Phi (v, t, []) :: instrs
                  ); 
              Hash_set.add defs_v lbl)));
  vars, block_map

let mk_name (v,i) = Printf.sprintf "%s_%d" v i

let process_arg stack arg =
  Hashtbl.find_exn stack arg |>
  Stack.top_exn |>
  mk_name

let process_dst stack dst =
  let stack_dst = Hashtbl.find_exn stack dst in
  let _, i_old = Stack.top_exn stack_dst in
  Stack.push stack_dst (dst,i_old+1);
  mk_name (dst,i_old+1)

let update_intrs stack block =
  let process_arg arg = process_arg stack arg in
  let process_dst dst = process_dst stack dst in
  let stack_hist = ref [] in 
  let block' = List.map block ~f:(fun instr ->
    match instr with
    | Label _ -> instr
    | Cst (dst, typ, cst) ->
        stack_hist := dst :: !stack_hist;
        Cst (process_dst dst, typ, cst)
    | Binop (dst, typ, binop, arg1, arg2) ->
        let arg1', arg2' = process_arg arg1, process_arg arg2 in
        stack_hist := dst :: !stack_hist;
        Binop (process_dst dst, typ, binop, arg1', arg2')
    | Unop (dst, typ, unop, arg) ->
        let arg' = process_arg arg in
        stack_hist := dst :: !stack_hist;
        Unop (process_dst dst, typ, unop, arg')
    | Jmp _ -> instr
    | Br (arg, lbl1, lbl2) ->
        Br (process_arg arg, lbl1, lbl2)
    | Call (Some (dst), typ, name, Some (args)) ->
        let args' = List.map args ~f:process_arg in
        stack_hist := dst :: !stack_hist;
        Call (Some (process_dst dst), typ, name, Some (args'))
    | Call (None, typ, name, Some (args)) ->
        let args' = List.map args ~f:process_arg in
        Call (None, typ, name, Some (args'))
    | Call (Some (dst), typ, name, None) ->
        stack_hist := dst :: !stack_hist;
        Call (Some (process_dst dst), typ, name, None)
    | Call (None, _, _, None) -> instr
    | Ret (Some (arg)) ->
        Ret (Some (process_arg arg))
    | Ret (None) -> instr
    | Print (args) ->
        Print (List.map args ~f:process_arg)
    | Nop -> instr
    | Phi (dst, typ, phis) ->
        stack_hist := dst :: !stack_hist;
        let phis' = phis (*List.map phis ~f:(fun (lbl,arg) -> (lbl,process_arg arg)) *)in
        Phi (process_dst dst, typ, phis'))
in
block', !stack_hist

let update_phi_reads stack l_prev block =
  List.map block ~f:(fun instr ->
    match instr with
    | Phi (dst, typ, phis) ->
      let v' = Hashtbl.find_exn stack dst |> Stack.top_exn |> mk_name in
      let phis' = (l_prev,v') :: phis in 
      Phi (dst, typ, phis')
    | _ -> instr)

let update_func_args stack args =
  Option.(args >>| (fun args' ->
    List.map args' ~f:(fun (arg,typ) ->
      (Hashtbl.find_exn stack arg |> Stack.top_exn |> mk_name,typ))))
  (*match args with
  | None -> None
  | Some (args) ->
    let args' = List.map args ~f:(fun (lbl,arg) ->

    )
    in
    Some (args)*)

let _stack_copy stack =
  let stack' = Hashtbl.create (module String) in
  Hashtbl.iteri stack ~f:(fun ~key:k ~data:d ->
    Hashtbl.update stack' k ~f:(fun _ -> Stack.copy d));
  stack'

let rename_vars prog vars block_map cfg_succ dt =
  let stack = Hashtbl.create (module String) in
  List.iter vars ~f:(fun (v,_) ->
    Hashtbl.add_exn stack ~key:v ~data:(Stack.singleton (v,0)));
  let rec rename block_name =
    let block = Hashtbl.find_exn block_map block_name in
    let block', stack_hist = update_intrs stack block in
    Hashtbl.update block_map block_name ~f:(fun _ -> block');
    (* update phi-nodes *)
    let succs = Hashtbl.find_exn cfg_succ block_name in
    List.iter succs ~f:(fun succ_name ->
      Hashtbl.update block_map succ_name ~f:(function 
        | None -> failwith "Should not be reachable"
        | Some block -> update_phi_reads stack block_name block));
    (* for b in children in dt *)
    let d_children = Hashtbl.find_exn dt block_name in
    Hash_set.iter d_children ~f:rename;
    List.iter stack_hist ~f:(fun v -> ignore (Stack.pop_exn @@ Hashtbl.find_exn stack v))
  in
  let prog' = List.map prog ~f:(fun func ->
    let func' = { func with args = update_func_args stack func.args } in
    rename func.name;
    func')
  in
  prog', Hashtbl.to_alist block_map 

let to_ssa prog blocks cfg_succ df dt =
  let vars, block_map = insert_nodes prog blocks df in
  let prog', blocks' = rename_vars prog vars block_map cfg_succ dt in
  prog_from_block_list prog' blocks' cfg_succ, blocks'
