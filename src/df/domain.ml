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
  type t = (string, instr_list) Hashtbl.t

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
    | Nop -> t'
end 

module LiveVarsDomain : Domain = struct
  type t = string Hash_set.t

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
    end; t'
end
