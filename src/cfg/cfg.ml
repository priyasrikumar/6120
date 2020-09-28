open Types
open Core

type blocks_t = (lbl * instr list) list
type cfg_t = (lbl, lbl list) Hashtbl.t

let gen_pref = "gen_lbl_"
let gen_pref_len = String.length gen_pref

let is_gen_pref lbl =
  String.equal (String.prefix lbl gen_pref_len) gen_pref

let gen_block_name = Stdlib.Stream.from (fun d ->
  Some (Printf.sprintf "%s%d" gen_pref d))

let orig_pref = "og_lbl_"
let add_pref lbl = orig_pref^lbl

let mangle_instr instr =
  match instr with 
  | Label lbl -> Label (add_pref lbl)
  | Jmp lbl -> Jmp (add_pref lbl)
  | Br (cond, lbl1, lbl2) -> Br (cond, add_pref lbl1, add_pref lbl2)
  | _ -> instr 

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
              let lbl' = add_pref lbl in
              if (is_gen_pref name && List.is_empty curr_block) |> not then
                blocks := (name,List.rev (Jmp (lbl') :: curr_block)) :: !blocks;
              (lbl', [Label (lbl')])
          | Jmp _ | Br _ | Ret _ ->
            blocks := (name,List.rev (mangle_instr instr::curr_block)) :: !blocks;
            (Stdlib.Stream.next gen_block_name, [])
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

let traverse_cfg_succ start_lbl cfg_succ =
  let seen_lbls = Hash_set.create (module String) in 
  let rec collect_lbls lbl acc =
    Hash_set.add seen_lbls lbl; 
    let lbls = Hashtbl.find_exn cfg_succ lbl in
    lbl::List.concat_map lbls ~f:(fun lbl -> 
      if Hash_set.mem seen_lbls lbl |> not then collect_lbls lbl (lbl::acc)
      else acc)
  in
  collect_lbls start_lbl [] |> List.stable_dedup

let filter_cfg_succ prog cfg_succ =
  let reachable = Hash_set.of_list (module String) @@ 
    List.concat_map prog ~f:(fun func ->
      traverse_cfg_succ func.name cfg_succ)
  in
  Hashtbl.filter_keys cfg_succ ~f:(fun key -> Hash_set.mem reachable key)

let make_cfg_pred cfg_succ =
  let pred_map = Hashtbl.create (module String) in
  Hashtbl.iteri cfg_succ ~f:(fun ~key:k ~data:d ->
    Hashtbl.update pred_map k ~f:(function None -> [] | Some d -> d);
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
  let cfg_succ_map = Hashtbl.of_alist_exn (module String) cfg_succ |>
    filter_cfg_succ prog
  in
  let cfg_pred_map = make_cfg_pred cfg_succ_map in
  let blocks = add_phantom_jmps blocks cfg_succ_map in
  blocks, cfg_succ_map, cfg_pred_map
