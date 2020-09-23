open Core 
open Domain
open Types 
open Cfg

module ForwardAnalysis (D : FwdDomain) = struct

  let print (instr, d) =
    Format.printf "INSTR %a : BLOCK %a\n" pp_instr instr D.print d

  let algo (blocks: blocks_t) cfg_pred =
    let blocks' = List.map blocks ~f:(fun (name,instrs) ->
      (name, List.map instrs ~f:(fun instr -> (instr,D.init ()))))
    in
    let workhash = Hashtbl.of_alist_exn (module String) blocks' in
    let worklist = Queue.of_list blocks' in
    while Queue.is_empty worklist |> not do
      let (name,block) = Queue.dequeue_exn worklist in 
      let in_b = 
        let preds = Option.value ~default:[] (Hashtbl.find cfg_pred name) in
        let out_blocks = List.map preds ~f:(Hashtbl.find_exn workhash) in
        let out_ps = List.map out_blocks ~f:(fun block ->
            match List.length block - 1 |> List.nth block with
            | None -> None
            | Some (_, d) -> Some (d))
          |> List.filter_map ~f:(function None -> None | Some (d) -> Some d)
        in
        List.fold_left out_ps ~init:(D.init ()) ~f:(fun acc curr -> D.merge acc curr);
      in
      let in_b_ref = ref in_b in
      let out_block = List.map block ~f:(fun (instr, _) ->
          in_b_ref := D.transfer instr !in_b_ref;
          (instr,!in_b_ref))
      in 
      let _, old_out_b = List.length block - 1 |> List.nth_exn block in 
      let out_b = !in_b_ref in 
      Hashtbl.update workhash name ~f:(fun _ -> out_block);
      List.iter out_block ~f:print;
      if D.leq out_b old_out_b |> not then Queue.enqueue worklist (name,out_block)
    done; List.map (Hashtbl.to_alist workhash) ~f:(fun (lbl, lst) -> lbl, List.map lst ~f:fst)
end