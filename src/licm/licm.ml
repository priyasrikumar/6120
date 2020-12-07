open Core 
open Types 
open Cfg 
open Df.Dataflow
open Df.Domain

module Reach = ForwardAnalysis(ReachingDomain) 

let licm cfg doms = 
  let _reaching = Reach.algo cfg |> Reach.get_result |> Hashtbl.of_alist_exn (module String) in 
  let _entry_block = [] in 
  let candidate_doms = List.filter doms ~f:(fun (_lbl, dom) -> List.length dom.natloops <> 0) in 
  let cfg_map = List.map cfg ~f:(fun c -> (c.func.name,c)) |> Hashtbl.of_alist_exn (module String) in 
  let nat_loops = List.map candidate_doms
      ~f:(fun (func_name,dom) ->
          let cfg_func = Hashtbl.find_exn cfg_map func_name in
          let block_map = cfg_func.blocks |> Hashtbl.of_alist_exn (module String) in
          let natloops = dom.natloops |> List.map ~f:(fun (a,b) -> a::b) in
          let res = List.concat_map natloops ~f:(fun natloop -> 
              List.map natloop ~f:(fun name ->
                  (name, Hashtbl.find_exn block_map name)))
          in
          (func_name, res))
  in
  List.map nat_loops ~f:(fun (func_name, natloops) -> 
      let res = List.map natloops ~f:(fun (block, instrs) -> block, instrs) 
      in (func_name, res))