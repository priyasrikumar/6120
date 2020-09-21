open Types
open Core

type blocks_t = (lbl * instr list) list
type cfg_t = (lbl, lbl list) Hashtbl.t

let gen_block_name d = Printf.sprintf "block%d" d

let make_blocks prog =
  (*let funcs = List.to_seq prog |>
    Seq.map (fun func -> (func.name, func)) |>
    Hashtbl.of_seq
    in *)
  let blocks = ref [] in
  let get_blocks func = List.fold_left ~init:(func.name,[]) func.instrs
      ~f:(fun (name,curr_block) instr ->
          match instr with
          | Label lbl -> (* if List.is_empty curr_block then (lbl,curr_block) else begin
              blocks := (name,List.rev (Jmp lbl :: curr_block)) :: !blocks;
              (lbl, []) end *)
              blocks := (name,List.rev (Jmp lbl :: curr_block)) :: !blocks;
              (lbl, [Label lbl])
          | Jmp _ | Br _ | Ret _ ->
            blocks := (name,List.rev (instr::curr_block)) :: !blocks;
            (List.length !blocks |> gen_block_name, [])
          | _ -> (name,instr::curr_block)
        ) |>
    (fun (name,block) -> blocks := (name,List.rev block)::!blocks)
  in
  List.iter prog ~f:(fun func -> get_blocks func);
  List.rev !blocks
(*List.filter (fun (name,block) -> block <> [])*)

let make_cfg_succ blocks =
  List.mapi ~f:(fun i (name,block) ->
      if List.is_empty block then
        match List.nth blocks (i+1) with
        | None -> (name,[])
        | Some (lbl, _) -> (name,[lbl])
      else
        match List.length block - 1 |> List.nth_exn block with
        | Jmp lbl -> (name,[lbl])
        | Br (_, lbl1, lbl2) -> (name,[lbl1;lbl2])
        | Ret _ -> (name,[])
        | _ -> match List.nth blocks (i+1) with
          | None -> (name,[])
          | Some (lbl, _) -> (name,[lbl])
    ) blocks

let make_cfg_pred cfg_succ =
  let pred_map = Hashtbl.create (module String) in
  Hashtbl.iteri cfg_succ ~f:(fun ~key:k ~data:d ->
    List.iter d ~f:(fun v ->
      match Hashtbl.find cfg_succ v with
      | None -> Hashtbl.update pred_map v ~f:(fun _ -> [k])
      | Some (lbls) -> Hashtbl.update pred_map v ~f:(fun _ -> k::lbls))
    );
  pred_map

let add_phantom_jmps blocks cfg =
  List.mapi ~f:(fun i (name,block) ->
      if List.is_empty block then 
        match List.nth blocks (i+1) with
        | None -> (name,block@[Ret None])
        | Some (lbl, _) ->
            let jmp =
              match Hashtbl.find_exn cfg lbl with 
              | [] -> [Ret None]
              | hd :: _ -> [Jmp hd]
            in
            (name,block@jmp)
      else
        match List.length block - 1 |> List.nth block with
        | None -> (name,[Jmp (List.nth_exn blocks (i+1) |> fst)])
        | Some instr -> match instr with
          | Jmp _ | Br _ | Ret _ -> (name,block)
          | _ -> match List.nth blocks (i+1) with
            | None -> (name,block@[Ret None])
            | Some (lbl, _) ->
                let jmp =
                match Hashtbl.find_exn cfg lbl with 
                | [] -> [Ret None]
                | hd :: _ -> [Jmp hd]
              in
              (name,block@jmp)
    ) blocks

let extract_cfg prog =
  let blocks = make_blocks prog in
  let cfg_succ = make_cfg_succ blocks in
  let cfg_succ_map = Hashtbl.of_alist_exn (module String) cfg_succ in
  let cfg_pred_map = make_cfg_pred cfg_succ_map in
  let blocks = add_phantom_jmps blocks cfg_succ_map in
  blocks, cfg_succ_map, cfg_pred_map

let traverse_cfg start_lbl cfg = 
  let rec collect_lbls lbl acc = 
    let lbls = Hashtbl.find_exn cfg lbl in
    lbl::List.concat_map lbls ~f:(fun lbl -> collect_lbls lbl (lbl::acc))
  in
  collect_lbls start_lbl [] |> List.stable_dedup