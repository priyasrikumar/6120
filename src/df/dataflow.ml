open Core 
open Domain
open Types 
open Cfg

module type AnalysisType = sig
  type t

  val print : Format.formatter -> t -> unit
  val algo : blocks_t -> cfg_t -> cfg_t -> t
end

module AnalysisBase (D : Domain) = struct

  type data = {
    in_b : D.t;
    out_b : D.t;
    instrs : (instr * D.t) list;
  }
  type t = (lbl * data) list 
  (*type t = (lbl * D.t * D.t) list*) 

  let print fmt t =
    Format.fprintf fmt "@[<hv 0>";
    List.iter (List.rev t) ~f:(fun (lbl,data) -> 
      Format.fprintf fmt "@[<hov 2>%s =@ @[IN: %a@]@ @[OUT: %a@]@ @]@ "
        lbl D.print data.out_b D.print data.in_b);
     Format.fprintf fmt "@]"

  let merge cfg workhash name =
    let cfg_lbls = Hashtbl.find_exn cfg name in
    let blocks = List.map cfg_lbls ~f:(Hashtbl.find_exn workhash) in
    let ps = List.filter_map blocks ~f:(fun data ->
      List.length data.instrs - 1 |> List.nth data.instrs)
    in 
    List.fold_left ps ~init:(D.init ())
      ~f:(fun acc curr -> snd curr |> D.merge acc)

  let transfer init data =
    let ref_d = ref init in
    let instrs' = List.map data.instrs ~f:(fun (instr, _) ->
      ref_d := D.transfer instr !ref_d;
      (instr,!ref_d))
    in
    instrs', !ref_d

  let enqueue_next workhash worklist cfg name =
    let next = Hashtbl.find_exn cfg name in 
    List.iter next ~f:(fun name' -> 
      let next_block = Hashtbl.find_exn workhash name' in
      Deque.enqueue_front worklist (name',next_block))

  let algo ~is_back:flag (blocks : blocks_t) cfg_succ cfg_pred =
    let blocks' = List.map blocks ~f:(fun (name,instrs) ->
      let instrs' = List.map instrs ~f:(fun instr -> (instr,D.init ())) in
      let instrs'' = if flag then List.rev instrs' else instrs' in
      let init = { in_b = D.init (); out_b = D.init (); instrs = instrs'' } in 
      (name, init))
    in
    let workhash = Hashtbl.of_alist_exn (module String) blocks' in
    let worklist = Array.of_list blocks' |> Deque.of_array in
    let merge =
      if flag then (fun name -> merge cfg_succ workhash name)
      else (fun name -> merge cfg_pred workhash name)
    in
    let get_next =
      if flag then (fun () -> Deque.dequeue_back_exn worklist)
      else (fun () -> Deque.dequeue_front_exn worklist)
    in
    let add_next =
      if flag then (fun name -> enqueue_next workhash worklist cfg_pred name)
      else (fun name -> enqueue_next workhash worklist cfg_succ name)
    in
    while Deque.is_empty worklist |> not do
      let (name,data) = get_next () in
      if List.is_empty data.instrs |> not then
        let in_b = merge name in
        let out_block, out_b = transfer in_b data in
        Hashtbl.update workhash name ~f:(fun _ -> 
          { in_b = in_b; out_b = out_b; instrs = out_block });
        if D.leq out_b data.out_b |> not then
          add_next name
    done;
    List.map (Hashtbl.to_alist workhash) ~f:(fun (lbl, data) ->
      (lbl, { data with instrs = List.rev data.instrs }))

end

module ForwardAnalysis (D : Domain) : AnalysisType = struct
  include AnalysisBase(D)

  let algo blocks cfg_succ cfg_pred =
    algo ~is_back:false blocks cfg_succ cfg_pred
end

module BackwardAnalysis (D : Domain) : AnalysisType = struct
  include AnalysisBase(D)

  let algo blocks cfg_succ cfg_pred =
    algo ~is_back:true blocks cfg_succ cfg_pred
end

  (*type data = {
    in_b : D.t;
    out_b : D.t;
    instrs : (instr * D.t) list;
  }
  type t = (lbl * D.t * D.t) list 

  let print fmt t =
    List.iter t ~f:(fun (lbl,in_b,out_b) -> 
      Format.fprintf fmt "@[%s = @[IN: %a@]@ @[OUT: %a@]@ @]@ "
        lbl D.print in_b D.print out_b)
        
  let algo (blocks: blocks_t) cfg_succ cfg_pred =
    let blocks' = List.map blocks ~f:(fun (name,instrs) ->
      let instrs' = List.map instrs ~f:(fun instr -> (instr,D.init ())) in
      let init = { in_b = D.init (); out_b = D.init (); instrs = instrs' } in 
      (name, init))
    in
    let workhash = Hashtbl.of_alist_exn (module String) blocks' in
    let worklist = Queue.of_list blocks' in
    while Queue.is_empty worklist |> not do
      let (name,data) = Queue.dequeue_exn worklist in 
      if List.is_empty data.instrs |> not then
        let in_b = 
          let preds = Option.value ~default:[] (Hashtbl.find cfg_pred name) in
          let out_blocks = List.map preds ~f:(Hashtbl.find_exn workhash) in
          let out_ps = List.map out_blocks ~f:(fun data ->
              match List.length data.instrs - 1 |> List.nth data.instrs with
              | None -> None
              | Some (_, d) -> Some (d))
            |> List.filter_map ~f:(function None -> None | Some (d) -> Some d)
          in
          List.fold_left out_ps ~init:(D.init ()) ~f:(fun acc curr -> D.merge acc curr);
        in
        let in_b_ref = ref in_b in
        let out_block = List.map data.instrs ~f:(fun (instr, _) ->
            in_b_ref := D.transfer instr !in_b_ref;
            (instr,!in_b_ref))
        in 
        let old_out_b = data.out_b in
        let out_b = !in_b_ref in 
        Hashtbl.update workhash name ~f:(fun _ -> 
          { in_b = in_b; out_b = out_b; instrs = out_block });
        if D.leq out_b old_out_b |> not then begin
          let succs = Hashtbl.find_exn cfg_succ name in 
          List.iter succs ~f:(fun name -> 
            let succ_block = Hashtbl.find_exn workhash name in
            Queue.enqueue worklist (name,succ_block))
        end
    done;
    List.map (Hashtbl.to_alist workhash) ~f:(fun (lbl, data) ->
      (lbl, data.in_b, data.out_b))*)