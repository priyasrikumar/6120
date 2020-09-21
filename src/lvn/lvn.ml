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
  let get_correct_call_arg default arg =
    if true then false e
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
                    binop_to_fun op |> function
                    | Arith f ->
                      let new_val = new_val () in
                      let new_exp = CstI (f cst1 cst2) in
                      update_tbls ~is_cst:true (new_val,new_exp,dst);
                      Cst (dst, Int, IntC (f cst1 cst2))
                    | Cmp f ->
                      let new_val = new_val () in
                      let new_exp = CstB (f cst1 cst2) in
                      update_tbls ~is_cst:true (new_val,new_exp,dst);
                      Cst (dst, Bool, BoolC (f cst1 cst2)) 
                    | _ -> Stdlib.invalid_arg "Shouldn't encounte `Logic here."
                  end
                | CstB bool1, CstB bool2 -> begin
                    binop_to_fun op |> function 
                    | Logic f ->
                      let new_val = new_val () in 
                      let new_exp = (f bool1 bool2) in 
                      update_tbls ~is_cst:true (new_val, CstB new_exp, dst); 
                      Cst (dst, Bool, BoolC (new_exp))
                    | _ -> Stdlib.invalid_arg "No `Arith allowed!"
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
              let new_val = new_val () in 
              let new_exp = Un (Id, new_val) in 
              let new_var = arg in 
              update_tbls (new_val, new_exp, new_var);
              instr
            | Some (entry) -> 
              let (new_val, new_exp, new_var) = get_correct_arg arg entry in 
              update_tbls (new_val, new_exp, new_var); 
              Unop (dst, typ, Id, new_var)
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
          let _, _, arg' = get_correct_arg arg
        | Call (Some (dst), typ, name, Some (args))
        | Call (None, typ, name, Some (args))
        | Call (Some (dst), typ, name, None)
        | Call (None, _, _, None) -> instr
        | Ret (Some arg) ->
        | Ret (None) -> instr
        | Print (args) ->
        | Nop -> instr
      )



(*| Jmp of lbl
  | Br of arg * lbl * lbl
  | Call of dst option * typ option * string * arg list option
  | Ret of arg option
  | Print of arg list
  | Nop*)

let lvn blocks =
  List.map blocks ~f:(fun (name,instrs) -> (name,lvn_block instrs))
  List.map blocks ~f:(fun (name,instrs) -> (name,lvn_block instrs))