open Bril

type blocks_t = (label * instr list) list
type cfg_t = (label, label list) Hashtbl.t

let gen_block_name d = Printf.sprintf "block%d" d

let retrieve_funcs prog = List.to_seq prog |>
    Seq.map (fun func -> (func.name, func)) |>
    Hashtbl.of_seq

let make_blocks prog =
  let blocks = ref [] in
  let get_blocks func = List.fold_left (fun (name,curr_block) instr ->
      match instr with
      | Label lbl -> if curr_block = [] then (lbl,curr_block) else begin
          blocks := (name,List.rev curr_block) :: !blocks;
          (lbl, []) end
      | Jmp _ | Br _ | Ret _ ->
          blocks := (name,List.rev (instr::curr_block)) :: !blocks;
          (List.length !blocks |> gen_block_name, [])
      | _ -> (name,instr::curr_block)
    ) (func.name,[]) func.instrs |> (fun (name,block) -> blocks := (name,List.rev block)::!blocks)
  in
  List.iter (fun func -> get_blocks func) prog;
  List.rev !blocks
  (*List.filter (fun (name,block) -> block <> [])*)

let make_cfg blocks =
  List.mapi (fun i (name,block) ->
    if block = [] then
      match List.nth_opt blocks (i+1) with
      | None -> (name,[])
      | Some (lbl, _) -> (name,[lbl])
    else
      match List.length block - 1 |> List.nth block with
      | Jmp lbl -> (name,[lbl])
      | Br (_, lbl1, lbl2) -> (name,[lbl1;lbl2])
      | Ret _ -> (name,[])
      | _ -> match List.nth_opt blocks (i+1) with
          | None -> (name,[])
          | Some (lbl, _) -> (name,[lbl])
  ) blocks

let add_phantom_jmps blocks cfg =
  List.mapi (fun i (name,block) ->
    if block = [] then 
      match List.nth_opt blocks (i+1) with
        | None -> (name,block@[Ret None])
        | Some (lbl, _) -> (name,block@[Jmp (Hashtbl.find cfg lbl |> List.hd)])
    else
      match List.length block - 1 |> List.nth_opt block with
      | None -> (name,[Jmp (List.nth blocks (i+1) |> fst)])
      | Some instr -> match instr with
        | Jmp _ | Br _ | Ret _ -> (name,block)
        | _ -> match List.nth_opt blocks (i+1) with
            | None -> (name,block@[Ret None])
            | Some (lbl, _) -> (name,block@[Jmp (Hashtbl.find cfg lbl |> List.hd)])
  ) blocks

let extract_cfg prog =
  let funcs = retrieve_funcs prog in 
  let blocks = make_blocks prog in
  let cfg = make_cfg blocks in
  let cfg_map = List.to_seq cfg |> Hashtbl.of_seq in
  let blocks = add_phantom_jmps blocks cfg_map in
  blocks, cfg_map, funcs 