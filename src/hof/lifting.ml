open Types 
open Core 


let rec free_vars func_args blocks =
  let func_vars = Hash_set.of_list (module String) func_args in 
  let decl_vars = Hash_set.create (module String) in
  List.concat_map blocks ~f:(fun block ->
    let free_vars = Hash_set.create (module String) in
    let process_args args = List.iter args ~f:(fun arg ->
      if (Hash_set.mem func_vars arg || Hash_set.mem decl_vars arg) |> not then
        Hash_set.add free_vars arg)
    in
    List.iter block ~f:(fun instr ->
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
      | Fncall (dst, _, _, Some args) ->
        process_args args;
        Hash_set.add decl_vars dst
      | Fncall (dst, _, _, None) ->
        Hash_set.add decl_vars dst);
    Hast_set.to_list free_vars)


(* do this later...
(* these are all functions that are essentially arguments, we'll have to figure this out later *)
  let fun_func_args = Hashtbl.create (module String) in
  List.iter func.args ~f:(fun (f,t) ->
    match t with
    | Some (Fun _) -> Hashtbl.add_exn ~key:fun_func_args ~data:f
    | _ -> ()); *)

let rec process_func blocks =
  (* process blocks piece by piece *)
  List.map blocks ~f:(fun block ->
    List.filter_map block ~f:(fun instr ->
      match instr with
      | Anon (dst, rtyp, args, instr_inner) ->
      | _ -> Some (instr)
    )
  )


let process_rest cfg =
  List.map cfg ~f:(fun cfg_func ->
    match cfg_func.rtype with
    | Some (Fun _) -> failwith "Returning functions is not supported!"
    | _ -> ())
