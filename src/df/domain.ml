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
    Format.fprintf fmt "%a" pp_instr_list ls

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