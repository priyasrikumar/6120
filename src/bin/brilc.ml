open Core
open Cfg

let print_res res =
  prog_from_cfg res
  |> Json_processor.to_json
  |> Yojson.Basic.to_channel stdout

module DCE = struct 
  open Dce
  let spec = Command.Spec.(empty)

  let _dce_with_cfg cfg =
    dce cfg

  let _dce prog =
    let cfg = extract_cfg prog in
    _dce_with_cfg cfg

  let run () =
    let prog = Json_processor.parse_in in
    let res = _dce prog in
    print_res res

end

let dce_cmd : Command.t = 
  Command.basic_spec ~summary:"dead code elimination"
    DCE.spec
    DCE.run 

module LVN = struct 
  open Lvn
  let spec = Command.Spec.(empty)

  let _lvn_with_cfg cfg =
    lvn cfg

  let _lvn prog =
    let cfg = extract_cfg prog in
    _lvn_with_cfg cfg

  let run () =
    let prog = Json_processor.parse_in in 
    let res = _lvn prog in
    print_res res

end

let lvn_cmd : Command.t = 
  Command.basic_spec ~summary:"local value numbering"
    LVN.spec
    LVN.run 

module LVN_DCE = struct 
  let spec = Command.Spec.(empty)

  let _lvn_dce_with_cfg cfg =
    LVN._lvn_with_cfg cfg |> DCE._dce_with_cfg

  let _lvn_dce prog =
    let cfg = extract_cfg prog in
    _lvn_dce_with_cfg cfg

  let run () =
    let prog = Json_processor.parse_in in 
    let res = _lvn_dce prog in
    print_res res
end

let lvn_dce_cmd : Command.t = 
  Command.basic_spec ~summary:"local value numbering, then dead code elimination"
    LVN_DCE.spec
    LVN_DCE.run 

module Reach = struct 
  open Df.Domain
  open Df.Dataflow
  let spec = Command.Spec.(empty)

  module Reach = ForwardAnalysis(ReachingDomain)

  let _reach prog =
    let cfg = extract_cfg prog in
    Reach.algo cfg

  let run () =
    let prog = Json_processor.parse_in in 
    let res = _reach prog in
    Reach.print Format.std_formatter res

end

let reach_cmd : Command.t = 
  Command.basic_spec ~summary:"reaching definitions dataflow analysis"
    Reach.spec
    Reach.run 

module LiveVars = struct 
  open Df.Domain
  open Df.Dataflow
  let spec = Command.Spec.(empty)

  module LiveVars = BackwardAnalysis(LiveVarsDomain)

  let _reach prog =
    let cfg = extract_cfg prog in
    LiveVars.algo cfg

  let run () =
    let prog = Json_processor.parse_in in 
    let res = _reach prog in
    LiveVars.print Format.std_formatter res

end

let live_vars_cmd : Command.t = 
  Command.basic_spec ~summary:"live variables dataflow analysis"
    LiveVars.spec
    LiveVars.run 

module ConstantPropagation = struct
  open Df.Domain
  open Df.Dataflow 
  let spec = Command.Spec.(empty)

  module CPD = ForwardAnalysis(ConstantPropDomain)

  let _cpd prog =
    let cfg = extract_cfg prog in
    CPD.algo cfg

  let run () =
    let prog = Json_processor.parse_in in 
    let res = _cpd prog in
    CPD.print Format.std_formatter res

end

let cpd_cmd : Command.t = 
  Command.basic_spec ~summary:"constant propagation dataflow analysis"
    ConstantPropagation.spec
    ConstantPropagation.run 

module Doms = struct 
  let spec = Command.Spec.(empty)

  let _doms prog =
    let cfg = extract_cfg prog in
    doms cfg

  let print_doms doms =
    Format.printf "@[";
    List.iter doms ~f:(fun (lbl,dom) -> 
      Format.printf "@[Doms of %s:@ @[" lbl;
        Hashtbl.iteri dom.dom ~f:(fun ~key:key ~data:data -> 
      Format.printf "@[%s %a@]@ " key Types.pp_lbl_list (Hash_set.to_list data));
      Format.printf "@]@]@ ");
    Format.printf "@]"

  let print_dom_trees doms =
    Format.printf "@[";
    List.iter doms ~f:(fun (lbl,dom) -> 
      Format.printf "@[Dom tree of %s:@ @[" lbl;
      Hashtbl.iteri dom.dt ~f:(fun ~key ~data -> 
        let domlst = Hash_set.to_list data in 
        Format.printf "@[%s %a@]@ " key Types.pp_lbl_list domlst);
      Format.printf "@]@]@ ");
    Format.printf "@]"

  let print_dom_frontiers doms =
    Format.printf "@[";
    List.iter doms ~f:(fun (lbl,dom) -> 
      Format.printf "@[Dom frontier of %s:@ @[" lbl;
      let df = Hashtbl.to_alist dom.df in 
      let df = List.map df ~f:(fun (lbl,df) -> (lbl,Hash_set.to_list df)) in
      Format.printf "@[%a@]" Types.pp_dom_list df;
      Format.printf "@]@]@ ");
    Format.printf "@]"

  let print_all doms =
    print_doms doms;
    print_dom_trees doms;
    print_dom_frontiers doms

  let run () =
    let prog = Json_processor.parse_in in
    let res = _doms prog in
    print_all res

end

let dom_cmd : Command.t = 
  Command.basic_spec ~summary:"compute dominators"
    Doms.spec
    Doms.run 

module SSA = struct 
  open Ssa
  let spec = Command.Spec.empty

  let _to_ssa prog =
    let cfg = extract_cfg prog in
    let dom = doms cfg in
    to_ssa cfg dom

  let to_ssa () =
    let prog = Json_processor.parse_in in
    let res = _to_ssa prog in
    print_res res

  let _from_ssa prog = 
    let cfg = extract_cfg prog in
    from_ssa cfg

  let from_ssa () =
    let prog = Json_processor.parse_in in
    let res = _from_ssa prog in
    print_res res

  let _from_ssa_opt prog = 
    let cfg = extract_cfg prog in
    let from_ssa = Ssa.from_ssa cfg in
    LVN_DCE._lvn_dce_with_cfg from_ssa

  let from_ssa_opt () =
    let prog = Json_processor.parse_in in
    let res = _from_ssa_opt prog in
    print_res res

end

let to_ssa_cmd : Command.t = 
  Command.basic_spec ~summary:"convert program to ssa form"
    SSA.spec
    SSA.to_ssa

let from_ssa_cmd : Command.t =  
  Command.basic_spec ~summary:"convert program from ssa form"
    SSA.spec
    SSA.from_ssa

let from_ssa_opt_cmd : Command.t =
  Command.basic_spec ~summary:"convert program from ssa form and optimize using LVN+DCE"
    SSA.spec
    SSA.from_ssa_opt

let main : Command.t = 
  Command.group ~summary:"pick an optimization"
    [("dce", dce_cmd);
     ("lvn", lvn_cmd);
     ("lvn-dce", lvn_dce_cmd);
     ("reach", reach_cmd);
     ("live-vars", live_vars_cmd);
     ("const-prop", cpd_cmd);
     ("doms", dom_cmd);
     ("to-ssa", to_ssa_cmd);
     ("from-ssa", from_ssa_cmd);
     ("from-ssa-opt", from_ssa_opt_cmd);
    ]

let () = Command.run main



