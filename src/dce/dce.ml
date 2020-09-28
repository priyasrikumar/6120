open Types
open Core
open Cfg

let to_hashtbl assoc_ls = Hashtbl.of_alist_exn (module String) assoc_ls

let print_table table = Hashtbl.iteri table ~f:(fun ~key ~data -> print_endline (Printf.sprintf "%s %b" key data)) 

let used_vars_in_instrs used_vars instrs =
  List.fold_left ~init:None instrs 
    ~f:(fun _ instr ->
      match instr with 
      | Label _ -> None
      | Cst (dst, _, _) ->
          Hashtbl.update used_vars dst ~f:(function _ -> false);
          None
      | Binop (dst, _, _, arg1, arg2) ->
          Hashtbl.update used_vars dst ~f:(function _ -> false);
          Hashtbl.update used_vars arg1 ~f:(function _ -> true); 
          Hashtbl.update used_vars arg2 ~f:(function _ -> true); 
          None
      | Unop (dst, _, _, arg) ->
          Hashtbl.update used_vars dst ~f:(function _ -> false);
          Hashtbl.update used_vars arg ~f:(function _ -> true); 
          None
      | Jmp (label) -> Some [label]
      | Br (arg, label1, label2) ->
          Hashtbl.update used_vars arg ~f:(function _ -> true);
          Some [label1; label2]
      | Call (Some (dst), _, _, Some (args)) ->
          Hashtbl.update used_vars dst ~f:(function _ -> false); 
          List.iter args 
            ~f:(fun arg -> Hashtbl.update used_vars arg ~f:(function _ -> true));
          None
      | Call (Some (dst), _, _, None) ->
          Hashtbl.update used_vars dst ~f:(function _ -> false); None
      | Call (None, _, _, Some (args)) ->
          List.iter args 
            ~f:(fun arg -> Hashtbl.update used_vars arg ~f:(function _ -> true));
          None
      | Call (None, _, _, None) -> None
      | Ret (Some (arg)) ->
          Hashtbl.update used_vars arg ~f:(function _ -> true);
          None
      | Ret (None) -> None
      | Nop -> None
      | Print (args) ->
          List.iter args ~f:(fun arg -> Hashtbl.update used_vars arg ~f:(function _ -> true)); 
          None)

let instrs_to_eliminate blocks block_map cfg_succ funcs = 
  List.filter_map blocks 
    ~f:(fun (name, _) -> if Hashtbl.mem funcs name then
            let used_vars = Hashtbl.create (module String) in
            let succ_lbls = traverse_cfg_pre name cfg_succ in
            let blocks = List.map succ_lbls
              ~f:(fun lbl -> Hashtbl.find_exn block_map lbl) in 
            List.iter blocks ~f:(fun block -> ignore (used_vars_in_instrs used_vars block));
            Some (name, used_vars)
         else None)

let get_var instr =
  match instr with
  | Cst (d,_, _) | Binop (d, _, _,_ , _)
  | Unop (d, _, _, _) | Call (Some d,_, _, _)-> Some (d)
  | Label _| Jmp (_) | Br (_, _ , _ ) | Ret (_) | Print (_)
  | Call (None, _, _, _)| Nop -> None

let filter_instrs used_vars instrs =
  let is_deleted = ref false in 
  let instrs' = List.filter_map instrs ~f:(fun instr ->
      match get_var instr with
      | Some d -> 
        if Hashtbl.find_exn used_vars d |> not then begin
          is_deleted := true;
          None
        end else Some instr
      | None -> Some instr)
  in
  (instrs', !is_deleted)

let make_funcs name block_map used_vars cfg_succ funcs =
  let traversed_lbls = traverse_cfg_pre name cfg_succ in
  let is_deleted = ref false in
  let instrs' = List.concat_map traversed_lbls ~f:(fun lbl -> 
      let block', is_deleted' = Hashtbl.find_exn block_map lbl |>
                                filter_instrs used_vars
      in
      is_deleted := !is_deleted || is_deleted';
      Hashtbl.update block_map lbl ~f:(fun k ->
        match k with
        | Some _ -> block'
        | None -> invalid_arg (Printf.sprintf "label %s should be in block map" lbl));
      block')
  in
  let func = Hashtbl.find_exn funcs name in 
  { func with instrs = instrs' }, !is_deleted

let global_elim_instrs blocks block_map cfg_succ prog =
  let prog' = List.map prog ~f:(fun func -> (func.name, func)) in
  let funcs = to_hashtbl prog' in 
  let is_processed = ref false in 
  Stdlib.flush_all ();
  let block_vars_map = instrs_to_eliminate blocks block_map cfg_succ funcs in 
  Stdlib.flush_all ();
  let new_funcs = List.map block_vars_map ~f:(fun (name, used_vars) ->
      let (func', is_processed') = make_funcs name block_map used_vars cfg_succ funcs
      in
      is_processed := !is_processed || is_processed'; func')
  in
  let new_blocks = Hashtbl.to_alist block_map in 
  new_funcs, new_blocks, !is_processed

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

let local_elim_blocks blocks = 
  let is_processed = ref false in
  let blocks' = List.map blocks ~f:(fun (name,block) ->
    let block', is_processed' = local_elim_instrs block in
    is_processed := !is_processed || is_processed';
    (name,block')
  )
  in
  blocks', !is_processed 

let dce prog blocks cfg_succ =
  let rec fix_local blocks stop =
    if stop then blocks
    else
      let blocks', is_changed = local_elim_blocks blocks in
      fix_local blocks' (not is_changed)
  in 
  let rec fix_global prog blocks stop = 
    if stop then prog
    else
      let block_map = to_hashtbl blocks in
      let prog', blocks', is_changed = global_elim_instrs blocks block_map cfg_succ prog in
      let blocks'' = fix_local blocks' false in
      fix_global prog' blocks'' (not is_changed)
  in
  fix_global prog blocks false
