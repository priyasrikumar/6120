open Types
open Core

type cfg_func =
  {
    func: func ;
    blocks : (lbl * instr list) list ;
    cfg_succ : (lbl, lbl list) Hashtbl.t ;
    cfg_pred : (lbl, lbl list) Hashtbl.t ;
  }
type cfg = cfg_func list

type dom_t =
  {
    dom : (arg, arg Hash_set.t) Hashtbl.t ;
    dt : (lbl, lbl Hash_set.t) Hashtbl.t ;
    df : (lbl, lbl Hash_set.t) Hashtbl.t ;
  }
type doms = (lbl * dom_t) list

let gen_pref = "gen_lbl_"
let gen_pref_len = String.length gen_pref

let is_gen_pref lbl =
  String.equal (String.prefix lbl gen_pref_len) gen_pref

let gen_block_name = Stdlib.Stream.from (fun d ->
    Some (Printf.sprintf "%s%d" gen_pref d))

let orig_pref = "og_"
let add_pref lbl = orig_pref^lbl

let mangle_instr instr =
  match instr with 
  | Label lbl -> Label (add_pref lbl)
  | Jmp lbl -> Jmp (add_pref lbl)
  | Br (cond, lbl1, lbl2) -> Br (cond, add_pref lbl1, add_pref lbl2)
  | Phi (dst, typ, args) ->
    Phi (dst, typ, List.map args ~f:(fun (lbl,args) -> (add_pref lbl,args)))
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
            if (is_gen_pref name && List.is_empty curr_block) |> not then begin
              blocks := (name,List.rev (Jmp (lbl') :: curr_block)) :: !blocks
            end;
            (lbl', [Label (lbl')])
          | Jmp _ | Br _ | Ret _ ->
            blocks := (name,List.rev (mangle_instr instr::curr_block)) :: !blocks;
            (Stdlib.Stream.next gen_block_name, [])
          | _ -> (name,mangle_instr instr::curr_block))
      |> (fun (name,block) ->
            if is_gen_pref name |> not then 
              blocks := (name,List.rev block) :: !blocks)
  in
  List.map prog ~f:(fun func ->
    let func' =
       match func.instrs with
      | Label _ :: _ -> func
      | _ -> {func with instrs = Label func.name :: func.instrs}
    in
    get_blocks func';
    let blocks' = !blocks in
    blocks := [];
    (func', List.rev blocks'))

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

let _traverse_cfg_pre start_lbl cfg_succ =
  let seen_lbls = Hash_set.create (module String) in 
  let rec collect_lbls lbl acc =
    Hash_set.add seen_lbls lbl; 
    let lbls = Hashtbl.find_exn cfg_succ lbl in
    lbl :: List.fold_left lbls ~init:acc ~f:(fun acc lbl ->
        if Hash_set.mem seen_lbls lbl then acc
        else collect_lbls lbl acc)
  in
  collect_lbls start_lbl [] |> List.stable_dedup

let traverse_cfg_pre start_lbl cfg =
  _traverse_cfg_pre start_lbl cfg.cfg_succ

let _traverse_cfg_post start_lbl cfg_succ =
  let seen_lbls = Hash_set.create (module String) in 
  let rec collect_lbls lbl acc =
    Hash_set.add seen_lbls lbl; 
    let lbls = Hashtbl.find_exn cfg_succ lbl in
    List.fold_left lbls ~init:(lbl::acc) ~f:(fun acc lbl -> 
        if Hash_set.mem seen_lbls lbl then acc
        else collect_lbls lbl acc)
  in
  collect_lbls start_lbl [] |> List.stable_dedup

let traverse_cfg_post start_lbl cfg =
  _traverse_cfg_post start_lbl cfg.cfg_succ

let traverse_cfg_post_rev start_lbl cfg =
  traverse_cfg_post start_lbl cfg |> List.rev

let filter_cfg_succ func_name cfg_succ =
  let reachable = _traverse_cfg_pre func_name cfg_succ
    |> Hash_set.of_list (module String)
  in
  Hashtbl.filter_keys_inplace cfg_succ
    ~f:(fun key -> Hash_set.mem reachable key)

let make_cfg_pred cfg_succ =
  let pred_map = Hashtbl.create (module String) in
  Hashtbl.iteri cfg_succ ~f:(fun ~key:k ~data:d ->
      Hashtbl.update pred_map k ~f:(function None -> [] | Some d -> d);
      List.iter d ~f:(fun v ->
          Hashtbl.update pred_map v
            ~f:(function None -> [k] | Some lbls -> k::lbls))
      (*match Hashtbl.find pred_map v with
        | None -> Hashtbl.update pred_map v ~f:(fun _ -> [k])
        | Some (lbls) -> Hashtbl.update pred_map v ~f:(fun _ -> k::lbls)) *)
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
        match List.length block - 1 |> List.nth_exn block with
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
  let cfg_succs = List.map blocks ~f:(fun (func,blocks) ->
      (func,blocks,make_cfg_succ blocks))
  in
  let cfg_succs_map = List.map cfg_succs ~f:(fun (func,blocks,cfg_succ) ->
      let cfg_succ_map = Hashtbl.of_alist_exn (module String) cfg_succ in
      filter_cfg_succ func.name cfg_succ_map;
      (func,blocks,cfg_succ_map))                
  in
  let cfg = List.map cfg_succs_map ~f:(fun (func,blocks,cfg_succ) ->
    {
      func = func ;
      blocks = add_phantom_jmps blocks cfg_succ ;
      cfg_succ = cfg_succ ;
      cfg_pred = make_cfg_pred cfg_succ ;
    })
  in
  cfg

let inv_dom dom =
  let inv_dom = Hashtbl.create (module String) in
  Hashtbl.iteri dom ~f:(fun ~key:k ~data:d ->
      Hash_set.iter d ~f:(fun e -> 
          Hashtbl.update inv_dom e ~f:(function
              | None ->
                let set = Hash_set.create (module String) in
                Hash_set.add set k; set 
              | Some set -> Hash_set.add set k; set)));
  inv_dom

let df dom cfg =
  let cfg_succ = cfg.cfg_succ in 
  let inv_dom = inv_dom dom in
  let df = Hashtbl.create (module String) in
  Hashtbl.iter_keys dom ~f:(fun k ->
      let k_dominates = Hashtbl.find_exn inv_dom k in 
      let dom_succs = Hash_set.create (module String) in
      Hash_set.iter k_dominates ~f:(fun node ->
          let succs = Hashtbl.find_exn cfg_succ node in
          List.iter succs ~f:(Hash_set.add dom_succs));
      let frontier = Hash_set.filter dom_succs ~f:(fun k' ->
          Hash_set.mem k_dominates k' |> not || String.equal k' k)
      in
      Hashtbl.add_exn df ~key:k ~data:frontier);
  df

let dt dom = 
  let inv_dom = inv_dom dom in 
  let inv_dom_strict = Hashtbl.copy inv_dom in
  Hashtbl.iteri inv_dom ~f:(fun ~key ~data -> 
      Hash_set.remove data key); 
  let inv_dom_strict_dom = Hashtbl.map inv_dom ~f:(fun d ->
      Hash_set.fold d ~init:(Hash_set.create (module String)) ~f:(fun acc curr ->
        (Hashtbl.find_exn inv_dom_strict curr) |> Hash_set.union acc  
      ) 
    )
  in
  Hashtbl.mapi inv_dom_strict ~f:(fun ~key:k ~data:d ->
    Hash_set.filter d ~f:(fun e -> 
      Hash_set.mem (Hashtbl.find_exn inv_dom_strict_dom k) e |> not))

let doms cfg =
  List.map cfg ~f:(fun cfg_func ->
    let func_name = cfg_func.func.name in 
    let cfg_pred = cfg_func.cfg_pred in
    let dom = Hashtbl.create (module String) in
    let rev_post_order = traverse_cfg_post_rev func_name cfg_func in
    (*List.iter rev_post_order ~f:(fun lbl ->
        Hashtbl.add_exn dom ~key:lbl ~data:(Hash_set.create (module String)));*)
    let dom_change = ref true in
    while !dom_change do
      dom_change := false; 
      List.iter rev_post_order ~f:(fun vertex -> 
          let preds = Hashtbl.find_exn cfg_pred vertex in
          let curr_dom = match preds with
            | [] -> Hash_set.create (module String)
            | _ -> 
              let preds' = List.filter_map preds ~f:(Hashtbl.find dom) in
              match preds' with
              | [] -> Hash_set.create (module String)
              | h :: [] -> Hash_set.copy h
              | h :: t -> List.fold t ~init:(Hash_set.copy h) ~f:Hash_set.inter
          in
          Hash_set.add curr_dom vertex;
          Hashtbl.find_and_call dom vertex
            ~if_found:(fun prev_dom ->
              dom_change := !dom_change ||
              Hash_set.equal prev_dom curr_dom |> not)
            ~if_not_found:(fun _ -> dom_change := true);
          Hashtbl.update dom vertex ~f:(fun _ -> curr_dom))
    done;
    let dom_info = 
      {
        dom = dom ;
        dt = dt dom ;
        df = df dom cfg_func ;
      }
    in
    (func_name,dom_info))

let remove_phantom_jmps blocks =
  let rec fix blocks =
    let blocks' = List.filter_mapi blocks ~f:(fun i (lbl,block) ->
      let block' =
        if List.is_empty block then []
        else begin
          match List.length block - 1 |> List.nth_exn block with
          | Jmp (lbl') -> begin
            match List.nth blocks (i+1) with 
            | None -> block
            | Some (lbl'',_) ->
              if String.equal lbl' lbl'' then
                let block' = List.take block (List.length block - 1) in
                if List.is_empty block' then []
                else block'
              else  
                block
            end
          | _ -> block
        end
      in
      if List.is_empty block' then None
      else Some (lbl,block'))
    in
    if List.length blocks = List.length blocks' then blocks'
    else fix blocks'
  in
  fix blocks

let prog_from_cfg cfg =
  List.map cfg ~f:(fun cfg_func ->
    let blocks' = remove_phantom_jmps cfg_func.blocks in
    { cfg_func.func with instrs = List.concat_map blocks' ~f:snd }
    (*let func = cfg_func.func in 
    let block_map = Hashtbl.of_alist_exn (module String) cfg_func.blocks in
    let lbls = _traverse_cfg_pre func.name cfg_func.cfg_succ in
    let instrs' = List.concat_map lbls ~f:(Hashtbl.find_exn block_map)(*(fun lbl ->
      if String.equal lbl func.name then Hashtbl.find_exn block_map lbl
      else Label (lbl) :: Hashtbl.find_exn block_map lbl)*)
    in
    { func with instrs = instrs' }*))
