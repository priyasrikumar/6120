open Core 
open Domain
open Types 
open Cfg

module type AnalysisType = sig
  type t

  val print : Format.formatter -> t -> unit
  val algo : blocks_t -> cfg_t -> cfg_t -> t
end

module ForwardAnalysis (D : FwdDomain) : AnalysisType = struct

  type data = {
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
      (lbl, data.in_b, data.out_b))
end