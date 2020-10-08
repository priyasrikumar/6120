open Core 
open Cfg 
open Types

type phi_tmp =
  Phi' of dst * typ * (lbl * arg) list

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

let insert_nodes func_name func_args blocks df =
  let defs = Hashtbl.create (module String) in
  let vars =
    let init = List.concat_map blocks ~f:(fun (lbl,instrs) ->
      vars_of_process_defs lbl defs instrs)
    in
    List.fold_left func_args ~init:init ~f:(fun acc (arg,typ) ->
      update_defs arg func_name defs; (arg,typ) :: acc)
    |> List.stable_dedup
  in
  let phi_map = Hashtbl.create (module String) in
  List.iter blocks ~f:(fun (lbl,_) ->
      Hashtbl.add_exn phi_map ~key:lbl ~data:(Hashtbl.create (module String)));
  List.iter vars ~f:(fun (v,t) ->
    let defs_v = Hashtbl.find_exn defs v in
    let defs_queue = Queue.create () in
    Hash_set.iter defs_v ~f:(Queue.enqueue defs_queue);
    while Queue.is_empty defs_queue |> not do
      let d = Queue.dequeue_exn defs_queue in
      let df = Hashtbl.find_exn df d in
      Hash_set.iter df ~f:(fun block ->
        let phi_tbl = Hashtbl.find_exn phi_map block in 
        if Hashtbl.mem phi_tbl v |> not then
          Hashtbl.add_exn phi_tbl ~key:v ~data:(Phi' (v,t,[]));
          if Hash_set.mem defs_v block |> not then begin
            Hash_set.add defs_v block;
            Queue.enqueue defs_queue block
          end)
    done);
  vars, phi_map

let mk_name v i = Printf.sprintf "%s_%d" v i

let process_arg stack arg =
  Hashtbl.find_exn stack arg |> fst |> Stack.top_exn

let process_dst stack dst =
  let stack, stream = Hashtbl.find_exn stack dst in
  let new_num = Stdlib.Stream.next stream in
  let new_name = mk_name dst new_num in
  Stack.push stack new_name;
  new_name

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

(*
let update_phi_reads stack l_prev block =
  let process_arg dst = process_arg stack dst in
  List.map block ~f:(fun instr ->
    match instr with
    | Phi (dst, typ, phis) ->
      let v' = process_arg dst in
      let phis' = (l_prev,v') :: phis in 
      Phi (dst, typ, phis')
    | _ -> instr)
*)

(*let update_func_args stack args =
  Option.(args >>| (fun args' ->
    List.map args' ~f:(fun (arg,typ) ->
      (process_arg stack arg,typ))))*)
let update_func_args stack args =
  List.map args ~f:(fun (arg,typ) -> (process_arg stack arg,typ))

let rename_vars vars func_name func_args phi_map blocks cfg_succ dt =
  let block_map = Hashtbl.of_alist_exn (module String) blocks in
  let stack = Hashtbl.create (module String) in
  List.iter vars ~f:(fun (v,_) ->
    let counter = Stdlib.Stream.from (fun i -> Some (i+1)) in
    Hashtbl.add_exn stack ~key:v ~data:((Stack.singleton v,counter)));
  let func_args' = update_func_args stack func_args in 
  let rec rename block_name =
    let block = Hashtbl.find_exn block_map block_name in
    let block_phis = Hashtbl.find_exn phi_map block_name in
    Hashtbl.map_inplace block_phis ~f:(fun (Phi' (dst,typ,args)) ->
      Phi' (process_dst stack dst, typ, args));
    let block', stack_hist = update_intrs stack block in
    Hashtbl.update block_map block_name ~f:(fun _ -> block');
    (* update phi-nodes *)
    let succs = Hashtbl.find_exn cfg_succ block_name in
    List.iter succs ~f:(fun succ_name ->
      let phis = Hashtbl.find_exn phi_map succ_name in
      Hashtbl.mapi_inplace phis ~f:(fun ~key:phi ~data:(Phi' (dst, typ, args)) ->
        let v' = process_arg stack phi in
        Phi' (dst, typ, (block_name,v') :: args)));
    (* for b in children in dt *)
    let d_children = Hashtbl.find_exn dt block_name in
    Hash_set.iter d_children ~f:rename;
    List.iter stack_hist ~f:(fun v ->
      ignore (Hashtbl.find_exn stack v |> fst |> Stack.pop_exn))
  in
  rename func_name;
  func_args', block_map

(** TODO *)
let _remove_bad_phis _phi_map _cfg_pred = ()

let insert_phis phi_map block_map =
  Hashtbl.mapi block_map ~f:(fun ~key:name ~data:block ->
    let phis = Hashtbl.find_exn phi_map name
      |> Hashtbl.to_alist
      |> List.map ~f:(fun (_,(Phi' (dst,typ,args))) -> Phi (dst,typ,args))
    in
    match block with
    | [] -> phis
    | hd :: tl -> hd :: phis @ tl)

let to_ssa prog blocks cfg_succ df dt =
  let funcs, blocks = List.map prog ~f:(fun func -> 
    let lbls = traverse_cfg_pre func.name cfg_succ |> Hash_set.of_list (module String) in
    let blocks' = List.filter blocks ~f:(fun (lbl,_) -> Hash_set.mem lbls lbl) in
    let func_args = match func.args with None -> [] | Some (args) -> args in
    let vars, phi_map = insert_nodes func.name func_args blocks' df in
    let func_args', block_map = rename_vars vars func.name func_args phi_map blocks' cfg_succ dt in
    let block_map' = insert_phis phi_map block_map in
    let func_args'' = if List.is_empty func_args' then None else Some (func_args') in
    let func' = { func with args = func_args'' } in
    let func'' = prog_from_block_map [func'] block_map' cfg_succ |> List.hd_exn in 
    (func'', Hashtbl.to_alist block_map'))
    |> List.unzip
  in
  funcs, List.concat blocks
  (*let _vars, block_map = insert_nodes prog blocks df in
  (*let prog', blocks' = rename_vars prog vars block_map cfg_succ dt in
  prog_from_block_list prog' blocks' cfg_succ, blocks'*)
  prog_from_block_map prog block_map cfg_succ, blocks*)

(*let from_ssa prog blocks cfg_pred = 
  List.iter blocks ~f:(fun (lbl, instrs) -> 
    List.iter instrs ~f:(function 
    | Phi (dst, typ, lst) -> List.iter lst 
    ~f:(fun (lbl, arg) -> 
    let preds = Hashtbl.find_exn cfg_pred lbl in 
      
    )
    | _ -> ()))*)