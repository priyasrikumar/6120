open Types
open Core
open Cfg

let used_vars_in_instrs used_vars instrs =
  List.iter instrs 
    ~f:(fun instr ->
      match instr with 
      | Label _ -> ()
      | Cst (_, _, _) -> ()
      | Binop (_, _, _, arg1, arg2) ->
          Hash_set.add used_vars arg1; 
          Hash_set.add used_vars arg2
      | Unop (_, _, _, arg) ->
          Hash_set.add used_vars arg
      | Jmp (_) -> ()
      | Br (arg, _, _) ->
          Hash_set.add used_vars arg
      | Call (_, _, _, Some (args)) ->
          List.iter args 
            ~f:(fun arg -> Hash_set.add used_vars arg)
      | Call (_, _, _, None) -> ()
      | Ret (Some (arg)) ->
          Hash_set.add used_vars arg
      | Ret (None) -> ()
      | Nop -> ()
      | Print (args) ->
          List.iter args ~f:(fun arg -> Hash_set.add used_vars arg)
      | Phi (_, _, phis) -> 
          List.iter phis ~f:(fun (_,arg) ->
            Hash_set.add used_vars arg)
      | Alloc (_, _, arg) ->
          Hash_set.add used_vars arg
      | Free (arg) ->
          Hash_set.add used_vars arg
      | Store (arg1, arg2) ->
          Hash_set.add used_vars arg1;
          Hash_set.add used_vars arg2
      | Load (_, _, arg) ->
          Hash_set.add used_vars arg
      | Ptradd (_, _, arg1, arg2) ->
          Hash_set.add used_vars arg1;
          Hash_set.add used_vars arg2
      | Ptrcpy (_, _, arg) ->
          Hash_set.add used_vars arg)

let instrs_to_eliminate cfg_func = 
  (*let block_map = Hashtbl.of_alist_exn (module String) cfg.blocks in
  let blocks = traverse_cfg_pre cfg.func.name cfg in 
  let used_vars = Hashtbl.create (module String) in
  List.iter blocks ~f:(fun lbl -> 
    Hashtbl.find_exn block_map lbl |> used_vars_in_instrs used_vars);
  used_vars*)
  let used_vars = Hash_set.create (module String) in
  List.iter cfg_func.blocks ~f:(fun (_,block) ->
    used_vars_in_instrs used_vars block);
  used_vars

let get_var instr =
  match instr with
  | Cst (d,_, _) | Binop (d, _, _,_ , _)
  | Unop (d, _, _, _) | Call (Some d,_, _, _)
  | Alloc (d, _, _) | Load (d, _, _)
  | Ptradd (d, _, _, _) | Ptrcpy (d, _, _) -> Some (d)
  | Label _| Jmp (_) | Br (_, _ , _ ) | Ret (_) | Print (_)
  | Call (None, _, _, _)| Nop -> None | Phi (_, _, _)
  | Free (_) | Store (_, _) -> None

let filter_instrs used_vars cfg_func =
  let is_deleted = ref false in 
  let blocks' = List.map cfg_func.blocks ~f:(fun (lbl,block) ->
    let block' = List.filter block ~f:(fun instr ->
        match get_var instr with
        | Some d -> Hash_set.mem used_vars d
        | None -> true)
    in
    is_deleted := !is_deleted || (List.length block) <> (List.length block');
    (lbl,block'))
  in

  (blocks', !is_deleted)

let global_elim_instrs cfg_func =
  let used_vars = instrs_to_eliminate cfg_func in
  let blocks', is_deleted = filter_instrs used_vars cfg_func in
  let cfg_func' = { cfg_func with blocks = blocks' } in
  cfg_func', is_deleted

let local_elim_instrs instrs =
  let is_deleted = ref false in
  let last_def = Hashtbl.create (module String) in
  let remove_idxs = Hashtbl.create (module Int) in
  List.iteri instrs ~f:(fun i instr ->
    let uses, def =
      match instr with
      | Label _ -> [], None
      | Cst (dst, _, _) -> [], Some (dst)
      | Binop (dst, _, _, arg1, arg2) -> [arg1; arg2], Some (dst)
      | Unop (dst, _, _, arg) -> [arg], Some (dst)
      | Jmp _ -> [], None
      | Br (dst, _, _) -> [], Some (dst)
      | Call (Some (dst), _, _, Some (args)) -> args, Some (dst)
      | Call (Some (dst), _, _, None) -> [], Some (dst)
      | Call (None, _, _, Some (args)) -> args, None
      | Call (None, _, _, None) -> [], None
      | Ret (Some (arg)) -> [arg], None
      | Ret (None) -> [], None
      | Nop -> [], None
      | Print (args) -> args, None
      | Phi (dst, _, phis) -> List.map phis ~f:snd, Some (dst)
      | Alloc (dst, _, arg) -> [arg], Some (dst)
      | Free (arg) -> [arg], None
      | Store (arg1, arg2) -> [arg1; arg2], None
      | Load (dst, _, arg) -> [arg], Some (dst)
      | Ptradd (dst, _, arg1, arg2) -> [arg1; arg2], Some (dst)
      | Ptrcpy (dst, _, arg) -> [arg], Some (dst)
    in
    (* remove uses *)
    List.iter uses ~f:(Hashtbl.remove last_def);
    (* process defs *)
    match def with
    | Some v -> begin
        if Hashtbl.mem last_def v then begin
          let i' = Hashtbl.find_exn last_def v in
          Hashtbl.add_exn remove_idxs ~key:i' ~data:();
          Hashtbl.remove last_def v;
          is_deleted := true
        end;
        Hashtbl.add_exn last_def ~key:v ~data:i;
      end
    | None -> ()
  );
  let instrs' = List.filter_mapi instrs ~f:(fun i instr ->
    if Hashtbl.mem remove_idxs i then None
    else Some instr)
  in
  instrs', !is_deleted

let local_elim_blocks cfg =
  let is_processed = ref false in
  let blocks' = List.map cfg.blocks ~f:(fun (name,block) ->
      let block', is_processed' = local_elim_instrs block in
      is_processed := !is_processed || is_processed';
      (name,block'))
  in
  let cfg' = { cfg with blocks = blocks' } in 
  cfg', !is_processed 

let dce cfg =
  List.map cfg ~f:(fun cfg_func ->
    let rec fix_local cfg_func stop =
      if stop then cfg_func
      else
        let cfg', is_changed = local_elim_blocks cfg_func in
        fix_local cfg' (not is_changed)
    in 
    let rec fix_global cfg_func stop = 
      if stop then cfg_func
      else
        let cfg', is_changed = global_elim_instrs cfg_func in
        let _cfg'' = fix_local cfg' false in
        fix_global cfg' (not is_changed)
    in
    fix_global cfg_func false)
