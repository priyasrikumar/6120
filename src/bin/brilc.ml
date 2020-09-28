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

  module Res = ForwardAnalysis(ReachingDomain)

  let run () = let prog = parse_in in 
      let blocks, cfg_succ, cfg_pred = extract_cfg prog in
      let res = Res.algo blocks cfg_succ cfg_pred in
      Res.print Format.std_formatter res
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

let main : Command.t = 
  Command.group ~summary:"pick an optimization or two"
  [("dce", dce_cmd);
    ("lvn", lvn_cmd);
    ("lvn-dce", lvn_dce_cmd);
    ("reach", reach_cmd);
    ("live-vars", live_vars_cmd);
    ("doms", dom_cmd)]

let () = Command.run main



