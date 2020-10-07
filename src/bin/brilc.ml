open Core
open Yojson.Basic
open Cfg 
open Json_processor
open Dce 
open Lvn 
open Df.Domain
open Df.Dataflow

module DCE = struct 
  let spec = Command.Spec.(empty)

  let run () = let prog = parse_in in 
      let blocks, cfg_succ, _cfg_pred = extract_cfg prog in 
      let res = dce prog blocks cfg_succ |> to_json in 
      to_channel stdout res 
end

let dce_cmd : Command.t = 
  Command.basic_spec ~summary:"dead code elimination"
    DCE.spec
    DCE.run 

module LVN = struct 
  let spec = Command.Spec.(empty)


  let run () = let prog = parse_in in 
      let blocks, cfg_succ, _cfg_pred = extract_cfg prog in 
      let (res, _, _) = lvn prog blocks cfg_succ in 
      to_channel stdout (res |> to_json) 
end

let lvn_cmd : Command.t = 
  Command.basic_spec ~summary:"local value numbering"
    LVN.spec
    LVN.run 

module LVN_DCE = struct 
  let spec = Command.Spec.(empty)

  let run () = let prog = parse_in in 
      Printf.printf "here";
      let blocks, cfg_succ, _cfg_pred = extract_cfg prog in 
      let prog', blocks', cfg_succ' = lvn prog blocks cfg_succ in 
      let res = dce prog' blocks' cfg_succ' in 
      to_channel stdout (res |> to_json) 
end

let lvn_dce_cmd : Command.t = 
  Command.basic_spec ~summary:"local value numbering, then dead code elimination"
    LVN_DCE.spec
    LVN_DCE.run 

module Reach = struct 
  let spec = Command.Spec.(empty)

  module Reach = ForwardAnalysis(ReachingDomain)

  let run () = let prog = parse_in in 
      let blocks, cfg_succ, cfg_pred = extract_cfg prog in
      let res = Reach.algo blocks cfg_succ cfg_pred in
      Reach.print Format.std_formatter res
end

let reach_cmd : Command.t = 
  Command.basic_spec ~summary:"reaching definitions dataflow analysis"
    Reach.spec
    Reach.run 

module LiveVars = struct 
  let spec = Command.Spec.(empty)

  module LiveVars = BackwardAnalysis(LiveVarsDomain)

  let run () = let prog = parse_in in 
      let blocks, cfg_succ, cfg_pred = extract_cfg prog in
      let res = LiveVars.algo blocks cfg_succ cfg_pred in
      LiveVars.print Format.std_formatter res
end

let live_vars_cmd : Command.t = 
  Command.basic_spec ~summary:"live variables dataflow analysis"
    LiveVars.spec
    LiveVars.run 

module ConstantPropagation = struct 
  let spec = Command.Spec.(empty)

  module CPD = ForwardAnalysis(ConstantPropDomain)

  let run () = let prog = parse_in in 
      let blocks, cfg_succ, cfg_pred = extract_cfg prog in
      let res = CPD.algo blocks cfg_succ cfg_pred in
      CPD.print Format.std_formatter res
end

let cpd_cmd : Command.t = 
  Command.basic_spec ~summary:"constant propagation dataflow analysis"
    ConstantPropagation.spec
    ConstantPropagation.run 

module Doms = struct 
  let spec = Command.Spec.(empty)

  let run () = let prog = parse_in in 
      let blocks, cfg_succ, cfg_pred = extract_cfg prog in
      let res = doms prog blocks cfg_succ cfg_pred in 
      Hashtbl.iteri res ~f:(fun ~key:key ~data:data -> 
        Format.printf "@[%s %a@]@ " key Types.pp_lbl_list (Hash_set.to_list data))
end

let dom_cmd : Command.t = 
  Command.basic_spec ~summary:"compute dominators"
    Doms.spec
    Doms.run 

(* module DomTree = struct 
  let spec = Command.Spec.empty

  let run () = let prog = parse_in in 
  let blocks, cfg_succ, cfg_pred = extract_cfg prog in 
  let doms = doms prog blocks cfg_succ cfg_pred in 
  let dom_tree = dom_tree doms in 
  Hashtbl.iteri dom_tree ~f:(fun ~key ~data -> 
        Format.printf "@[%s %s@]@ " key data.label)
end

let dom_tree_cmd : Command.t = 
  Command.basic_spec ~summary:"construct dominator tree"
    DomTree.spec
    DomTree.run  *)

module DomFrontiers = struct 
  let spec = Command.Spec.empty

  let run () = let prog = parse_in in 
  let blocks, cfg_succ, cfg_pred = extract_cfg prog in 
  let dom = doms prog blocks cfg_succ cfg_pred in 
  let dom_frontiers = df dom cfg_succ in 
  let tmp = Hashtbl.to_alist dom_frontiers in
  let to_print = List.map tmp ~f:(fun (a,b) -> (a, Hash_set.to_list b)) in
  Format.printf "df : %a" Types.pp_dom_list to_print
end

let dom_frontier_cmd : Command.t = 
  Command.basic_spec ~summary:"construct dominator tree"
    DomFrontiers.spec
    DomFrontiers.run 


let main : Command.t = 
  Command.group ~summary:"pick an optimization or two"
  [("dce", dce_cmd);
    ("lvn", lvn_cmd);
    ("lvn-dce", lvn_dce_cmd);
    ("reach", reach_cmd);
    ("live-vars", live_vars_cmd);
    ("const-prop", cpd_cmd);
    ("doms", dom_cmd);
    (* ("domtree", dom_tree_cmd); *)
    ("domfrontiers", dom_frontier_cmd)
    ]

let () = Command.run main



