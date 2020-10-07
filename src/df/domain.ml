open Core
open Types

module type Domain = sig
  type t

  val init : unit -> t

  val to_string : t -> string
  val print : Format.formatter -> t -> unit 

  val leq : t -> t -> bool

  val merge : t -> t -> t

  val transfer : instr -> t -> t 
end

module ReachingDomain : Domain = struct
  type t = (arg, instr_list) Hashtbl.t

  let init () = Hashtbl.create (module String)

  let to_string t = 
    Hashtbl.to_alist t|> List.concat_map ~f:snd |> show_instr_list
  let print fmt t =
    let ls = Hashtbl.to_alist t |> List.concat_map ~f:snd in
    Format.fprintf fmt "@[%a@]" pp_instr_list ls

  let leq t1 t2 = 
    Hashtbl.fold t1 ~init:true ~f:(fun ~key:k ~data:d acc ->
        match Hashtbl.find t2 k with
        | None -> false
        | Some instrs ->
          acc && (List.fold_left ~init:true ~f:(fun acc' e ->
              acc' && List.exists instrs ~f:(equal_instr e)) d))

  let merge t1 t2 =
    let t3 = Hashtbl.copy t1 in 
    Hashtbl.iteri t2 ~f:(fun ~key:var ~data:instrs -> 
        Hashtbl.update t3 var ~f:(function 
            | None -> instrs 
            | Some instrs' -> (List.append instrs' instrs) |> List.stable_dedup));
    t3

  let transfer instr t =
    let t' = Hashtbl.copy t in
    match instr with 
    | Cst (dst, _, _)
    | Binop (dst, _, _, _, _)
    | Unop (dst, _, _, _)
    | Call (Some (dst), _, _, _) ->    
      Hashtbl.update t' dst ~f:(function
          | None -> [instr]
          | Some _ -> [instr]); t'
    | Call (None, _, _, _)
    | Label _
    | Jmp _
    | Br (_, _, _)
    | Ret _
    | Print _
    | Nop 
    | Phi (_, _, _) -> t'
end 

module LiveVarsDomain : Domain = struct
  type t = arg Hash_set.t

  let init () = Hash_set.create (module String)

  let to_string t =
    Hash_set.to_list t |> show_arg_list
  let print fmt t =
    Format.fprintf fmt "@[%a@]" pp_arg_list (Hash_set.to_list t)

  let leq t1 t2 =
    Hash_set.fold t1 ~init:true ~f:(fun acc e ->
        acc && Hash_set.mem t2 e)

  let merge = Hash_set.union

  let transfer instr t =
    let t' = Hash_set.copy t in
    begin match instr with
      | Label _ -> ()
      | Cst (dst, _, _) ->
        Hash_set.remove t' dst
      | Binop (dst, _, _, arg1, arg2) ->
        Hash_set.add t' arg1; Hash_set.add t' arg2;
        Hash_set.remove t' dst
      | Unop (dst, _, _, arg) ->
        Hash_set.add t' arg; Hash_set.remove t' dst
      | Call (Some (dst), _, _, Some (args)) ->
        List.iter args ~f:(Hash_set.add t');
        Hash_set.remove t' dst
      | Call (Some (dst), _, _, None) ->
        Hash_set.remove t' dst
      | Call (None, _, _, Some (args)) ->
        List.iter args ~f:(Hash_set.add t')
      | Call (None, _, _, None) -> ()
      | Jmp _ -> ()
      | Br (arg, _, _) ->
        Hash_set.add t' arg
      | Ret (Some (arg)) ->
        Hash_set.add t' arg
      | Ret (None) -> ()
      | Print (args) ->
        List.iter args ~f:(Hash_set.add t')
      | Nop -> ()
      | Phi (dst, _, pihis) ->
          List.iter phis ~f:(fun (_,arg) -> Hash_set.add t' arg);
          Hash_set.remove t' dst
    end; t'
end

module ConstantPropDomain : Domain = struct
  type prop_lattice = Bot | Top | ConstI of int | ConstB of bool
  [@@deriving show, eq]
  type prop_lattice_list = (arg * prop_lattice) list [@@deriving show]
  type t = (arg, prop_lattice) Hashtbl.t

  let leq p1 p2 =
    match p1, p2 with
    | Top, _ -> true
    | _, Bot -> true
    | _ -> equal_prop_lattice p1 p2

  let meet p1 p2 =
    match p1, p2 with
    | _, Top -> p1
    | Top, _ -> p2
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | _, _ -> if equal_prop_lattice p1 p2 then p1 else Bot

  let init () = Hashtbl.create (module String)

  let to_string t = 
    Hashtbl.to_alist t |> show_prop_lattice_list
  let print fmt t =
    Format.fprintf fmt "@[%a@]" pp_prop_lattice_list (Hashtbl.to_alist t)

  let leq t1 t2 =
    Hashtbl.fold t1 ~init:true ~f:(fun ~key:k ~data:d acc ->
        match Hashtbl.find t2 k with
        | None -> acc && true
        | Some d' -> acc && leq d d')

  let merge t1 t2 =
    let t3 = Hashtbl.copy t1 in
    Hashtbl.iteri t2 ~f:(fun ~key:var ~data:prop ->
        Hashtbl.update t3 var ~f:(function
            | None -> prop
            | Some prop' -> meet prop prop'));
    t3

  let is_arith = function
    | Add | Mul | Sub | Div -> true
    | _ -> false 
  let is_cmp = function
    | Lt | Gt | Le | Ge | Eq -> true
    | _ -> false
  let _is_bool = function
    | Or | And -> true
    | _ -> false

  let process_args t args =
    List.iter args ~f:(fun arg ->
        Hashtbl.update t arg ~f:(function None -> Top | Some p -> p))

  let process_binop t dst op arg1 arg2 =
    process_args t [arg1; arg2];
    if is_arith op then begin
      match Hashtbl.find_exn t arg1, Hashtbl.find_exn t arg2 with
      | Bot, _ | _, Bot -> Hashtbl.update t dst ~f:(fun _ -> Bot)
      | Top, _ | _, Top -> Hashtbl.update t dst ~f:(fun _ -> Top)
      | ConstI (i1), ConstI (i2) -> begin 
          let i3 =
            match op with
            | Add -> ConstI (i1 + i2)
            | Mul -> ConstI (i1 * i2)
            | Sub -> ConstI (i1 - i2)
            | Div -> if i2 = 0 then Bot else ConstI (i1 / i2)
            | _ -> failwith "Should be unreachable, always guaranteed arithmetic op."
          in
          Hashtbl.update t dst ~f:(fun _ -> i3)
        end 
      | _ -> failwith "Type error in processing constant propagation binop."
    end else if is_cmp op then begin
      match Hashtbl.find_exn t arg1, Hashtbl.find_exn t arg2 with
      | Bot, _ | _, Bot -> Hashtbl.update t dst ~f:(fun _ -> Bot)
      | Top, _ | _, Top -> Hashtbl.update t dst ~f:(fun _ -> Top)
      | ConstI (i1), ConstI (i2) -> begin 
          let b3 =
            match op with
            | Eq -> ConstB (i1 = i2)
            | Lt -> ConstB (i1 < i2)
            | Gt -> ConstB (i1 > i2)
            | Le -> ConstB (i1 <= i2)
            | Ge -> ConstB (i1 >= i2)
            | _ -> failwith "Should be unreachable, always guaranteed comparison op."
          in
          Hashtbl.update t dst ~f:(fun _ -> b3)
        end 
      | _ -> failwith "Type error in processing constant propagation binop."
    end else begin
      match Hashtbl.find_exn t arg1, Hashtbl.find_exn t arg2 with
      | Bot, _ | _, Bot -> Hashtbl.update t dst ~f:(fun _ -> Bot)
      | Top, _ | _, Top -> Hashtbl.update t dst ~f:(fun _ -> Top)
      | ConstB (b1), ConstB (b2) -> begin 
          let b3 =
            match op with
            | And -> ConstB (b1 && b2)
            | Or -> ConstB (b1 || b2)
            | _ -> failwith "Should be unreachable, always guaranteed comparison op."
          in
          Hashtbl.update t dst ~f:(fun _ -> b3)
        end 
      | _ -> failwith "Type error in processing constant propagation binop."
    end

  let process_unop t dst op arg =
    process_args t [arg];
    match op with 
    | Not -> begin
        match Hashtbl.find_exn t arg with 
        | Bot -> Hashtbl.update t dst ~f:(fun _ -> Bot)
        | Top -> Hashtbl.update t dst ~f:(fun _ -> Top)
        | ConstB (b) -> Hashtbl.update t dst ~f:(fun _ -> ConstB (not b))
        | _ -> failwith "Type error in processing constant propagation unop."
      end
    | Id -> Hashtbl.update t dst ~f:(fun _ -> Hashtbl.find_exn t arg)

  let transfer instr t =
    let t' = Hashtbl.copy t in
    begin match instr with
      | Label _ -> ()
      | Cst (dst, _, IntC (i)) ->
        Hashtbl.update t' dst ~f:(fun _ -> ConstI i)
      | Cst (dst, _, BoolC (b)) ->
        Hashtbl.update t' dst ~f:(fun _ -> ConstB b)
      | Binop (dst, _, op, arg1, arg2) ->
        process_binop t' dst op arg1 arg2
      | Unop (dst, _, op, arg) ->
        process_unop t' dst op arg
      | Call (Some (dst), _, _, Some (args)) ->
        process_args t' args;
        Hashtbl.update t' dst ~f:(fun _ -> Top)
      | Call (Some (dst), _, _, None) ->
        Hashtbl.update t' dst ~f:(fun _ -> Top)
      | Call (None, _, _, Some (args)) ->
        process_args t' args
      | Call (None, _, _, None) -> ()
      | Jmp _ -> ()
      | Br (arg, _, _) ->
        process_args t' [arg]
      | Ret (Some (arg)) -> 
        process_args t' [arg]
      | Ret (None) -> ()
      | Print (args) ->
        process_args t' args
      | Phi (dst, _, phis) -> 
          process_args t' (List.map phis ~f:snd)
          Hashtbl.update t' dst ~f:(fun _ -> Top)
      | Nop -> ()
    end; t'
end
