open Core 
open Types.Bril_types
open Lvn_types

let update_tbls (arg_tbl,num_tbl,exp_tbl) is_cst ((num,exp,var) as entry) = 
  Hashtbl.update arg_tbl var ~f:(function _ -> entry);
  if not is_cst then Hashtbl.update num_tbl num ~f:(function _ -> entry);
  Hashtbl.update exp_tbl exp ~f:(function _ -> entry)

let lvn_block block =
  let new_val_stream = Stdlib.Stream.from (fun i -> Some (i+1)) in
  let new_val () = Stdlib.Stream.next new_val_stream in
  let arg_tbl = Hashtbl.create (module String) in
  let num_tbl = Hashtbl.create (module Int) in
  let exp_tbl = Hashtbl.create (module LvnExpHashable) in
  let update_tbls ?is_cst:(b=false) = update_tbls (arg_tbl,num_tbl,exp_tbl) b in
  let get_correct_arg default ((_num,_exp,var) as entry) =
    let entry' = Hashtbl.find_exn arg_tbl var in
    if is_tbl_val_eq entry entry' then entry
    else
      let new_num = new_val () in
      let new_exp = Un (Id, new_num) in
      update_tbls (new_num, new_exp, default);
      (new_num, new_exp, default)
  in
  let get_correct_call_arg arg =
    match Hashtbl.find arg_tbl arg with
    | None ->
        let new_num = new_val () in
        let new_exp = Un (Id, new_num) in
        update_tbls (new_num, new_exp, arg);
        arg
    | Some (_,Un (Id,num),_) -> 
        let (_, _, arg') as entry = Hashtbl.find_exn num_tbl num in
        let entry' = Hashtbl.find_exn arg_tbl arg' in
        if is_tbl_val_eq entry entry' then arg' else arg
    | Some (_,_,_) -> arg 
  in
  List.map block ~f:(fun instr ->
    match instr with
    | Label _ -> instr
    | Cst (dst, _, IntC i) -> 
        let new_exp = CstI i in
        let new_num = new_val () in 
        let new_var = dst in
        update_tbls ~is_cst:true (new_num,new_exp,new_var); 
        instr
    | Cst (dst, _ , BoolC b) -> 
        let new_exp = CstB b in
        let new_num = new_val () in 
        let new_var = dst in
        update_tbls ~is_cst:true (new_num,new_exp,new_var);
        instr
    | Binop (dst, typ, op, arg1, arg2) -> begin
        match Hashtbl.find arg_tbl arg1, Hashtbl.find arg_tbl arg2 with 
        | None, None -> 
            let l_val = new_val () in
            let l_exp = Un (Id, l_val) in
            let l_var = arg1 in
            update_tbls (l_val, l_exp, l_var);
            let r_val = new_val () in
            let r_exp = Un (Id, r_val) in
            let r_var = arg2 in
            update_tbls (r_val, r_exp, r_var);
            let new_val = new_val () in
            let new_exp = Bin (op, l_val, r_val) in
            let new_arg = dst in 
            update_tbls (new_val, new_exp, new_arg);
            instr
        | None, Some (r_entry) -> 
            let l_val = new_val () in
            let l_exp = Un (Id, l_val) in
            let l_var = arg1 in
            update_tbls (l_val, l_exp, l_var);
            let r_val, _, r_arg = get_correct_arg arg2 r_entry in 
            let new_val = new_val () in
            let new_exp = Bin (op, l_val, r_val) in
            let new_arg = dst in 
            update_tbls (new_val, new_exp, new_arg);            
            Binop (dst, typ, op, arg1, r_arg)
        | Some l_entry, None ->
            let r_val = new_val () in
            let r_exp = Un (Id, r_val) in
            let r_var = arg2 in
            update_tbls (r_val, r_exp, r_var);
            let l_val, _, l_arg = get_correct_arg arg1 l_entry in 
            let new_val = new_val () in 
            let new_exp = Bin (op, l_val, r_val) in 
            let new_arg = dst in 
            update_tbls (new_val, new_exp, new_arg);
            Binop (dst, typ, op, l_arg, arg2)
        | Some (l_val,l_expr,_l_arg), Some (r_val,r_expr,_r_arg) -> begin
            match l_expr, r_expr with 
            | CstI cst1, CstI cst2 -> begin
                match binop_to_fun op with
                | Arith f -> begin
                  try 
                    let new_val = new_val () in
                    let new_exp = CstI (f cst1 cst2) in
                    update_tbls ~is_cst:true (new_val,new_exp,dst);
                    Cst (dst, Int, IntC (f cst1 cst2))
                  with Division_by_zero -> begin
                    let new_val = new_val () in
                    let new_exp = Bin (op, l_val, r_val) in
                    update_tbls (new_val,new_exp,dst);
                    instr
                  end
                end
                | Cmp f ->
                    let new_val = new_val () in
                    let new_exp = CstB (f cst1 cst2) in
                    update_tbls ~is_cst:true (new_val,new_exp,dst);
                    Cst (dst, Bool, BoolC (f cst1 cst2)) 
                | _ -> Stdlib.invalid_arg "Shouldn't encounter Logic here."
              end
            | CstB bool1, CstB bool2 -> begin
                match binop_to_fun op with 
                | Logic f ->
                    let new_val = new_val () in 
                    let new_exp = (f bool1 bool2) in 
                    update_tbls ~is_cst:true (new_val, CstB new_exp, dst); 
                    Cst (dst, Bool, BoolC (new_exp))
                | _ -> Stdlib.invalid_arg "No Arith allowed!"
              end 
            | _ ->
                let canonc_expr =
                  if is_add_mul op then
                    if l_val < r_val then Bin (op, l_val, r_val)
                    else Bin (op, r_val, l_val)
                  else if is_cmp op then
                    if l_val < r_val then Bin (op, l_val, r_val)
                    else Bin (flip_cmp_op op, r_val, l_val)
                  else if is_bool op then 
                    if l_val < r_val then Bin (op, l_val, r_val)
                    else Bin (op, r_val, l_val)
                  else
                    Bin (op, l_val, r_val)
                in
                match Hashtbl.find exp_tbl canonc_expr with
                | Some ((_num,_exp,arg) as entry) ->
                  let entry' =  Hashtbl.find_exn arg_tbl arg in 
                  if is_tbl_val_eq entry entry' then begin
                    Hashtbl.update arg_tbl dst ~f:(function _ -> entry);
                    Unop (dst, typ, Id, arg)
                  end else
                    let new_val = new_val () in
                    update_tbls (new_val,canonc_expr,dst);
                    instr
              | None ->
                  let new_val = new_val () in
                  update_tbls (new_val,canonc_expr,dst);
                  instr
          end
      end
    | Unop (dst, typ, Id, arg) -> begin
        match Hashtbl.find arg_tbl arg with 
        | None ->
            let right_val = new_val () in 
            let right_exp = Un (Id, right_val) in 
            let right_var = arg in 
            update_tbls (right_val, right_exp, right_var);
            let new_val = new_val () in
            let new_exp = Un (Id, right_val) in
            update_tbls (new_val, new_exp, dst);
            instr
        | Some (_, (CstI (_) as exp), _)
        | Some (_, (CstB (_) as exp), _) ->
            let new_val = new_val () in
            update_tbls ~is_cst:true (new_val,exp,dst);
            let typ', cst' =
              match exp with
              | CstI i -> Int, IntC (i)
              | CstB b -> Bool, BoolC (b)
              | _ -> Stdlib.invalid_arg "Should be unreachable."
            in
            Cst (dst, typ', cst')
        | Some (_, (Un (Id, num) as new_exp), _) ->
            let (_,_,arg') = Hashtbl.find_exn num_tbl num in
            let new_val = new_val () in
            update_tbls ~is_cst:true (new_val, new_exp, dst);
            Unop (dst, typ, Id, arg')
        | Some (_, _, arg') ->
            let new_val = new_val () in
            let new_exp = Un (Id, new_val) in
            update_tbls (new_val, new_exp, dst);
            Unop (dst, typ, Id, arg')
        (*| Some (entry) ->
          Format.printf "AA1: %a\n" pp_tbl_val entry;
          let (new_val, new_exp, new_var) = get_correct_arg arg entry in
          if String.equal new_var arg |> not then
            Unop (dst, typ, Id, new_var)
          else begin
            Format.printf "AA2: %a\n" pp_tbl_val (new_val, new_exp, new_var);
            (*update_tbls (new_val, new_exp, dst);*)
            Hashtbl.add_exn arg_tbl ~key:dst ~data:(new_val, new_exp, new_var);
            Unop (dst, typ, Id, new_var) end*)
      end
    | Unop (dst, _, Not, arg) -> begin
        match Hashtbl.find arg_tbl arg with
        | None ->
            let r_val = new_val () in
            let r_exp = Un (Id, r_val) in
            let r_var = arg in
            update_tbls (r_val, r_exp, r_var);
            let new_val = new_val () in
            let new_exp = Un (Id, r_val) in
            update_tbls (new_val, new_exp, dst);
            instr
        | Some (_val,CstB (b),_arg) ->
            let new_val = new_val () in
            let new_exp = CstB (not b) in
            update_tbls ~is_cst:true (new_val,new_exp,dst);
            Cst (dst, Bool, BoolC (not b))
        | Some (num,_expr,_arg) ->
            let new_val = new_val () in 
            let new_exp = Un (Not, num) in
            update_tbls (new_val,new_exp,dst);
            instr
      end
    | Jmp _ -> instr
    | Br (arg, lbl1, lbl2) ->
        Br (get_correct_call_arg arg, lbl1, lbl2)
    | Call (Some (dst), typ, name, Some (args)) ->
        Call (Some (dst), typ, name, Some (List.map args ~f:get_correct_call_arg))
    | Call (None, typ, name, Some (args)) ->
        Call (None, typ, name, Some (List.map args ~f:get_correct_call_arg))
    | Call (Some (_), _, _, None) ->
        instr
    | Call (None, _, _, None) ->
        instr
    | Ret (Some arg) ->
        Ret (Some (get_correct_call_arg arg))
    | Ret (None) ->
        instr
    | Print (args) ->
        Print (List.map args ~f:get_correct_call_arg)
    | Nop ->
        instr
  )

let lvn prog blocks cfg =
  let blocks' = List.map blocks ~f:(fun (name,instrs) -> (name,lvn_block instrs)) in
  let block_map = Hashtbl.of_alist_exn (module String) blocks' in
  let prog' = List.map prog ~f:(fun func ->
    let lbls = Cfg.traverse_cfg func.name cfg in
    let instrs' = List.concat_map lbls ~f:(Hashtbl.find_exn block_map)(*(fun lbl ->
      if String.equal lbl func.name then Hashtbl.find_exn block_map lbl
      else Label (lbl) :: Hashtbl.find_exn block_map lbl)*)
    in
    { func with instrs = instrs' })
  in
  prog', blocks', cfg 
