open Core 
open Cfg 
open Types

type phi_tmp =
  Phi' of dst * union_typ * (lbl * arg) list

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
        (dst, Val typ) :: acc
      | Binop (dst, typ, _, _, _) ->
        update_defs dst lbl defs; 
        (dst, Val typ) :: acc
      | Unop (dst, typ, _, _) -> 
        update_defs dst lbl defs;
        (dst, Val typ) :: acc
      | Jmp _ -> acc
      | Br (_, _, _) -> acc
      | Call (Some (dst), Some (typ), _, _) ->
        update_defs dst lbl defs;
        (dst, typ) :: acc
      | Call (_, _, _, _) -> acc
      | Ret (_) -> acc
      | Nop -> acc
      | Print (_) -> acc
      | Phi (dst, Val (typ), _phis) ->
        update_defs dst lbl defs;
        (dst, Val typ) :: acc
      | Phi (dst, Ptr (typ), _phis) ->
        update_defs dst lbl defs;
        (dst, Ptr typ) :: acc
      | Alloc (dst, typ, _) ->
        update_defs dst lbl defs;
        (dst, Ptr typ) :: acc
      | Free _ -> acc
      | Store (_, _) -> acc
      | Load (dst, typ, _) ->
        update_defs dst lbl defs;
        (dst, Ptr typ) :: acc
      | Ptradd (dst, typ, _, _) ->
        update_defs dst lbl defs;
        (dst, Ptr typ) :: acc
      | Ptrcpy (dst, typ, _) ->
        update_defs dst lbl defs;
        (dst, Ptr typ) :: acc
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
  phi_map

let mk_name v i = Printf.sprintf "%s_%d" v i

let mk_counter var = Stdlib.Stream.from (fun i -> Some (mk_name var i))

let process_arg stack counters arg =
  Hashtbl.find_or_add stack arg ~default:(fun () ->
    let counter = mk_counter arg in
    let new_name = Stdlib.Stream.next counter in
    Hashtbl.add_exn counters ~key:arg ~data:counter;
    Stack.singleton new_name)
  |> Stack.top_exn

let process_dst stack counters dst =
  let new_name = Hashtbl.find_or_add counters dst
    ~default:(fun () -> mk_counter dst) |> Stdlib.Stream.next
  in 
  Hashtbl.update stack dst ~f:(function
    | None -> Stack.singleton new_name
    | Some stck -> Stack.push stck new_name; stck);
  new_name

let update_intrs stack counters block =
  let process_arg arg = process_arg stack counters arg in
  let process_dst dst = process_dst stack counters dst in
  let block' = List.map block ~f:(fun instr ->
    match instr with
    | Label _ -> instr
    | Cst (dst, typ, cst) ->
        Cst (process_dst dst, typ, cst)
    | Binop (dst, typ, binop, arg1, arg2) ->
        let arg1', arg2' = process_arg arg1, process_arg arg2 in
        Binop (process_dst dst, typ, binop, arg1', arg2')
    | Unop (dst, typ, unop, arg) ->
        let arg' = process_arg arg in
        Unop (process_dst dst, typ, unop, arg')
    | Jmp _ -> instr
    | Br (arg, lbl1, lbl2) ->
        Br (process_arg arg, lbl1, lbl2)
    | Call (Some (dst), typ, name, Some (args)) ->
        let args' = List.map args ~f:process_arg in
        Call (Some (process_dst dst), typ, name, Some (args'))
    | Call (None, typ, name, Some (args)) ->
        let args' = List.map args ~f:process_arg in
        Call (None, typ, name, Some (args'))
    | Call (Some (dst), typ, name, None) ->
        Call (Some (process_dst dst), typ, name, None)
    | Call (None, _, _, None) -> instr
    | Ret (Some (arg)) ->
        Ret (Some (process_arg arg))
    | Ret (None) -> instr
    | Print (args) ->
        Print (List.map args ~f:process_arg)
    | Nop -> instr
    | Phi (dst, typ, phis) ->
        let phis' = phis (*List.map phis ~f:(fun (lbl,arg) -> (lbl,process_arg arg)) *)in
        Phi (process_dst dst, typ, phis')
    | Alloc (dst, typ, arg) ->
        let arg' = process_arg arg in
        Alloc (process_dst dst, typ, arg')
    | Free (arg) ->
        let arg' = process_arg arg in
        Free (arg')
    | Store (arg1, arg2) ->
        let arg1', arg2' = process_arg arg1, process_arg arg2 in
        Store (arg1', arg2')
    | Load (dst, typ, arg) ->
        let arg' = process_arg arg in
        Load (process_dst dst, typ, arg')
    | Ptradd (dst, typ, arg1, arg2) ->
        let arg1', arg2' = process_arg arg1, process_arg arg2 in
        Ptradd (process_dst dst, typ, arg1', arg2')
    | Ptrcpy (dst, typ, arg) ->
        let arg' = process_arg arg in
        Ptrcpy (process_dst dst, typ, arg'))
in
block'

let _update_func_args stack args =
  List.map args ~f:(fun (arg,typ) -> (process_arg stack arg,typ))

let copy_stack stack =
  let stack' = Hashtbl.create (module String) in
  Hashtbl.iteri stack ~f:(fun ~key:k ~data:d ->
    Hashtbl.add_exn stack' ~key:k ~data:(Stack.copy d));
  stack'

let rename_vars func_args func_name phi_map blocks cfg_succ dt =
  let block_map = Hashtbl.of_alist_exn (module String) blocks in
  let stack = Hashtbl.create (module String) in
  let counters = Hashtbl.create (module String) in
  List.iter func_args ~f:(fun (v,_) ->
    Hashtbl.add_exn stack ~key:v ~data:(Stack.singleton v);
    Hashtbl.add_exn counters ~key:v ~data:(mk_counter v));
  let rec rename block_name =
    let old_stack = copy_stack stack in 
    let block = Hashtbl.find_exn block_map block_name in
    let block_phis = Hashtbl.find_exn phi_map block_name in
    Hashtbl.map_inplace block_phis ~f:(fun (Phi' (dst,typ,args)) ->
      Phi' (process_dst stack counters dst, typ, args));
    let block' = update_intrs stack counters block in
    Hashtbl.update block_map block_name ~f:(fun _ -> block');
    (* update phi-nodes *)
    let succs = Hashtbl.find_exn cfg_succ block_name in
    List.iter succs ~f:(fun succ_name ->
      let succ_phis = Hashtbl.find_exn phi_map succ_name in
      Hashtbl.mapi_inplace succ_phis ~f:(fun ~key:phi ~data:((Phi' (dst, typ, args)) as d) ->
        if Hashtbl.mem stack phi then
          let v' = process_arg stack counters phi in
          Phi' (dst, typ, (block_name,v') :: args)
        else
          d));
    (* for b in children in dt *)
    let d_children = Hashtbl.find_exn dt block_name in
    Hash_set.iter d_children ~f:rename;
    Hashtbl.mapi_inplace stack ~f:(fun ~key:k ~data:d ->
      Hashtbl.find_and_call old_stack k
        ~if_found:(fun s -> Stack.copy s)
        ~if_not_found:(fun _ -> d))
  in
  rename func_name;
  block_map

let remove_bad_phis phi_map cfg_pred =
  let to_del = Hash_set.create (module String) in
  let finish = ref false in
  while !finish |> not do
    finish := true;
    Hashtbl.iteri phi_map ~f:(fun ~key:block ~data:phis ->
      Hashtbl.iter phis ~f:(fun (Phi' (d,_t,args)) ->
        let ok_args_len = List.fold_left args ~init:0
          ~f:(fun acc (_,arg) -> if Hash_set.mem to_del arg then acc else acc + 1)
        in
        let preds_len = Hashtbl.find_exn cfg_pred block |> List.length in 
        if ok_args_len < preds_len && Hash_set.mem to_del d |> not then begin
          finish := false;
          Hash_set.add to_del d
        end))
  done;
  Hashtbl.map_inplace phi_map ~f:(fun phi ->
    Hashtbl.filter phi ~f:(fun (Phi' (d,_,_)) -> Hash_set.mem to_del d |> not))

let insert_phis phi_map block_map =
  Hashtbl.mapi block_map ~f:(fun ~key:name ~data:block ->
    let phis = Hashtbl.find_exn phi_map name
      |> Hashtbl.to_alist
      |> List.map ~f:(fun (_,(Phi' (dst,typ,args))) -> Phi (dst,typ,args))
    in
    match block with
    | [] -> phis
    | hd :: tl -> hd :: phis @ tl)

let to_ssa cfg doms =
  let doms_map = Hashtbl.of_alist_exn (module String) doms in
  List.map cfg ~f:(fun cfg_func ->
    let func_name = cfg_func.func.name in
    let dom_info = Hashtbl.find_exn doms_map func_name in 
    let blocks = cfg_func.blocks in
    let func_args = match cfg_func.func.args with None -> [] | Some (args) -> args in
    let phi_map = insert_nodes func_name func_args blocks dom_info.df in
    let block_map = rename_vars func_args func_name phi_map blocks cfg_func.cfg_succ dom_info.dt in
    remove_bad_phis phi_map cfg_func.cfg_pred;
    let block_map' = insert_phis phi_map block_map in
    let blocks' = List.map blocks ~f:(fun (lbl,_) -> (lbl,Hashtbl.find_exn block_map' lbl)) in
    { cfg_func with blocks = blocks'}
  )

let func_from_ssa blocks =
  let block_map = Hashtbl.of_alist_exn (module String) blocks in
  let add_copies dst typ args =
    List.iter args ~f:(fun (lbl,arg) ->
      let instr = 
        match typ with
        | Val typ -> Unop (dst, typ, Id, arg)
        | Ptr typ -> Ptrcpy (dst, typ, arg)
      in
      let new_instrs = 
        match Hashtbl.find_exn block_map lbl |> List.rev with
        | [] -> [instr]
        | hd :: tl -> (hd :: instr :: tl) |> List.rev
      in
      Hashtbl.update block_map lbl ~f:(fun _ -> new_instrs))
  in 
  List.iter blocks ~f:(fun (_,blocks) ->
    List.iter blocks ~f:(fun instr ->
      match instr with
      | Phi (dst, typ, args) -> add_copies dst typ args
      | _ -> ()));
  Hashtbl.map_inplace block_map ~f:(fun instrs -> 
    List.filter instrs ~f:(fun instr ->
      match instr with
      | Phi (_,_,_) -> false
      | _ -> true));
  block_map

let from_ssa cfg = 
  List.map cfg ~f:(fun cfg_func ->
    let blocks = cfg_func.blocks in 
    let block_map = func_from_ssa blocks in
    let blocks' = List.map blocks ~f:(fun (lbl,_) -> (lbl,Hashtbl.find_exn block_map lbl)) in
    { cfg_func with blocks = blocks'})
