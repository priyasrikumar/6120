open Types 
open Core 
open Cfg

let fn_names = Hashtbl.create (module String)
let free_vars func_args instrs =
  let func_vars = if Option.is_none func_args 
    then Hash_set.of_list (module String) [] 
    else Hash_set.of_list (module String) (Option.value_exn func_args) in 
  let decl_vars = Hash_set.create (module String) in
  let free_vars = Hash_set.create (module String) in
  let process_args args = List.iter args ~f:(fun arg ->
      if (Hash_set.mem func_vars arg || Hash_set.mem decl_vars arg) |> not then
        Hash_set.add free_vars arg)
  in
  List.iter instrs ~f:(fun instr ->
      match instr with
      | Label _ -> ()
      | Cst (dst, _, _) ->
        Hash_set.add decl_vars dst
      | Binop (dst, _, _, arg1, arg2) -> 
        process_args [arg1; arg2];
        Hash_set.add decl_vars dst
      | Unop (dst, _, _, arg) ->
        process_args [arg];
        Hash_set.add decl_vars dst
      | Jmp _ -> ()
      | Br (arg, _, _) ->
        process_args [arg]
      | Call (Some (dst), _, _, Some (args)) ->
        process_args args;
        Hash_set.add decl_vars dst
      | Call (None, _, _, Some (args)) ->
        process_args args
      | Call (Some (dst), _, _, None) ->
        Hash_set.add decl_vars dst
      | Call (None, _, _, None) -> ()
      | Ret (Some (arg)) ->
        process_args [arg]
      | Ret (None) -> ()
      | Print (args) ->
        process_args args
      | Nop -> ()
      | Phi (dst, _, args) ->
        List.map args ~f:snd |> process_args;
        Hash_set.add decl_vars dst
      | Alloc (dst, _, arg) ->
        process_args [arg];
        Hash_set.add decl_vars dst
      | Free (arg) ->
        process_args [arg]
      | Store (arg1, arg2) ->
        process_args [arg1; arg2]
      | Load (dst, _, arg) -> 
        process_args [arg];
        Hash_set.add decl_vars dst 
      | Ptradd (dst, _, arg1, arg2) ->
        process_args [arg1; arg2];
        Hash_set.add decl_vars dst
      | Ptrcpy (dst, _, arg) ->
        process_args [arg];
        Hash_set.add decl_vars dst
      | Anon (dst, _, Some (args), _) ->
        process_args args;
        Hash_set.add decl_vars dst
      | Anon (dst, _, None, _) ->
        Hash_set.add decl_vars dst
      | Fncall (Some dst, _, _, Some args) ->
        process_args args;
        Hash_set.add decl_vars dst
      | Fncall (Some dst, _, _, None) ->
        Hash_set.add decl_vars dst
      | Fncall (None, _, _, Some args) -> 
        process_args args;
      | Fncall (None, _, _, None) -> ());
  Hash_set.to_list free_vars

(* do this later...
   (* these are all functions that are essentially arguments, we'll have to figure this out later *)
   let fun_func_args = Hashtbl.create (module String) in
   List.iter func.args ~f:(fun (f,t) ->
    match t with
    | Some (Fun _) -> Hashtbl.add_exn ~key:fun_func_args ~data:f
    | _ -> ()); *)

let get_ret typ = 
  match typ with 
  | Fun (_, r) -> r
  | _ as typ -> failwith ("unsupported typ" ^ show_union_typ typ)

let zip typ args = 
  if Option.is_some args then 
    let args = Option.value_exn args in 
    match typ with 
    | Fun (Some ps, _) -> 
      Some (List.map2_exn args ps 
              ~f:(fun arg typ -> (arg, typ)))
    | _ as typ -> failwith ("unsupported typ" ^ show_union_typ typ)
  else 
    match typ with 
    | Fun (None, _) -> None 
    | _ -> failwith "extra arg types"

let zip_outer fn_args lst blocks = 
  let fn_args_hash = if Option.is_none fn_args
    then Hashtbl.of_alist_exn (module String) [] 
    else Hashtbl.of_alist_exn (module String) (Option.value_exn fn_args) in 
  let blocks = List.rev blocks in 
  List.concat_map lst ~f:(fun name -> 
      List.concat_map blocks ~f:(fun (_, block) -> 
          List.filter_map block ~f:(fun instr -> 
              match instr with 
              | _ when Hashtbl.mem fn_args_hash name -> 
                Some (name, Hashtbl.find_exn fn_args_hash name)
              | Cst (dst, typ, _)
              | Binop (dst, typ, _, _, _) 
              | Unop (dst, typ, _, _) when String.equal dst name -> 
                Some (dst, Val typ)
              | Call (Some dst, Some typ, _, _) 
              | Phi (dst, typ, _) 
              | Anon (dst, typ, _, _)
              | Fncall (Some dst, Some typ, _, _) when String.equal dst name -> 
                Some (dst, typ)
              | Alloc (dst, typ, _) 
              | Load (dst, typ, _) 
              | Ptradd (dst, typ, _, _)
              | Ptrcpy (dst, typ, _) when String.equal dst name -> 
                Some (dst, Ptx typ)
              | _ -> None)))

let filter_instrs instrs = 
  List.filter_map instrs ~f:(fun instr -> 
      match instr with 
      | Anon _ as instr -> Some instr 
      | _ -> None
    )

let process_func fn_args blocks =
  let new_funcs = ref [] in 
  (* process blocks piece by piece *)
  let new_blocks = List.mapi blocks ~f:(fun i (lbl, block) ->
       (lbl, List.filter_map block ~f:(fun instr -> 
            match instr with
            | Anon (dst, typ, args, instrs) -> 
              let free_vars = free_vars args instrs in
              let typed_args =
                match free_vars with
                | [] when Option.is_none args -> None 
                | [] -> zip typ args 
                | lst when Option.is_none args -> 
                  Some (zip_outer fn_args lst (List.take blocks (i+1))) 
                | lst -> 
                  Some (Option.value_exn (zip typ args) 
                        @ (zip_outer fn_args lst (List.take blocks (i+1))))
              in
              let new_func = 
                {
                  name = dst;
                  args = typed_args;
                  rtyp = get_ret typ;
                  instrs = instrs; 
                }
              in 
              Hashtbl.add_exn fn_names ~key:dst ~data:new_func;
              new_funcs := new_func :: !new_funcs;
              None 
            | Fncall (dst, typ, name, Some args) when Hashtbl.mem fn_names name -> 
              if (List.fold ~init:false args ~f:(fun acc arg -> acc && Hashtbl.mem fn_names arg)) then None
              else 
              Some (Call (dst, typ, (Hashtbl.find_exn fn_names name).name, Some args)) 
            | _ -> Some (instr))))
            in new_blocks, !new_funcs 

let process_q (cfg : cfg) = 
  List.iter cfg ~f:(fun cfg_func -> Hashtbl.add_exn fn_names ~key:cfg_func.func.name ~data:cfg_func.func);
  let res = ref [] in 
  let q = Queue.of_list cfg in 
  while (Queue.is_empty q |> not) do 
    let cfg_func = Queue.dequeue_exn q in 
    let blocks', new_funcs = process_func cfg_func.func.args cfg_func.blocks in 
    let fn = Hashtbl.find_exn fn_names cfg_func.func.name in 
      res := List.append [{
        func = fn ;
        blocks = blocks';
        cfg_succ = cfg_func.cfg_succ;
        cfg_pred = cfg_func.cfg_pred;
      }] !res;       
      Queue.enqueue_all q (extract_cfg new_funcs)
  done; 
  !res 

(*  List.map cfg ~f:(fun cfg_func ->
       match cfg_func.func.rtyp with
      | Some (Fun _) -> failwith "Returning functions is not supported!"
      | _ -> 
        let new_blocks, more_work = process_func func.args func.instrs in)
*)

