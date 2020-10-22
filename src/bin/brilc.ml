open Core
open Cfg

let print_res res =
  prog_from_cfg res
  |> Json_processor.to_json
  |> Yojson.Basic.to_channel stdout

module CFG = struct 
  let spec = Command.Spec.empty

  let run () = 
    let prog = Json_processor.parse_in in 
    let res = extract_cfg prog in 
    let prog' = prog_from_cfg res in 
    Json_processor.to_json prog' 
    |> Yojson.Basic.to_channel stdout
end

let cfg_cmd : Command.t = 
  Command.basic_spec ~summary:"test cfg conv"
    CFG.spec
    CFG.run

module DCE = struct 
  open Dce
  let spec = Command.Spec.(empty)

  let _dce_with_cfg cfg = dce cfg

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

  let _lvn_with_cfg cfg = lvn cfg

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

  let _print_doms doms =
    Format.printf "@[<hv 0>";
    List.iter doms ~f:(fun (lbl,dom) -> 
        Format.printf "@[<hov 2>Doms of %s:@ @[" lbl;
        Hashtbl.iteri dom.dom ~f:(fun ~key:key ~data:data -> 
            Format.printf "@[%s -> %a;@]@ " key Types.pp_lbl_list (Hash_set.to_list data));
        Format.printf "@]@]@ ");
    Format.printf "@]@ "

  let _print_dom_trees doms =
    Format.printf "@[<hv 0>";
    List.iter doms ~f:(fun (lbl,dom) -> 
        Format.printf "@[<hov 2>Dom tree of %s:@ @[" lbl;
        Hashtbl.iteri dom.dt ~f:(fun ~key ~data -> 
            let domlst = Hash_set.to_list data in 
            Format.printf "@[%s -> %a;@]@ " key Types.pp_lbl_list domlst);
        Format.printf "@]@]@ ");
    Format.printf "@]@ "

  let _print_dom_frontiers doms =
    Format.printf "@[<hv 0>";
    List.iter doms ~f:(fun (lbl,dom) -> 
        Format.printf "@[<hov 2>Dom frontier of %s:@ @[<hov 2>" lbl;
        let df = Hashtbl.to_alist dom.df in 
        let df = List.map df ~f:(fun (lbl,df) -> (lbl,Hash_set.to_list df)) in
        Format.printf "@[%a@]" Types.pp_dom_list df;
        Format.printf "@]@]@ ");
    Format.printf "@]"

  let print_all doms =
    _print_doms doms;
    _print_dom_trees doms;
    _print_dom_frontiers doms

  let print_doms () =
    let prog = Json_processor.parse_in in
    let res = _doms prog in
    _print_doms res

  let print_dom_trees () =
    let prog = Json_processor.parse_in in
    let res = _doms prog in
    _print_dom_trees res

  let print_dom_frontiers () =
    let prog = Json_processor.parse_in in
    let res = _doms prog in
    _print_dom_frontiers res

  let dom_run_all () =
    let prog = Json_processor.parse_in in
    let res = _doms prog in
    print_all res

end

let dom_cmd : Command.t = 
  Command.basic_spec ~summary:"compute dominators"
    Doms.spec
    Doms.print_doms

let dom_t_cmd : Command.t = 
  Command.basic_spec ~summary:"compute dominance trees"
    Doms.spec
    Doms.print_dom_trees

let dom_f_cmd : Command.t = 
  Command.basic_spec ~summary:"compute dominance frontiers"
    Doms.spec
    Doms.print_dom_frontiers

let dom_all_cmd : Command.t =
  Command.basic_spec ~summary:"compute all aspects of dominators"
    Doms.spec
    Doms.dom_run_all

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

module NatLoop = struct 
  let spec = Command.Spec.(empty)

  let _natloops prog =
    let cfg = extract_cfg prog in
    let doms = doms cfg in
    List.iter2_exn cfg doms ~f:(fun cfg_func (_,dom_func) -> 
      Cfg.natloops cfg_func dom_func |>
      List.iter ~f:(fun lst -> Format.printf "@[%a@]@ " Types.pp_arg_list lst))

  let run () =
    let prog = Json_processor.parse_in in 
    _natloops prog
end

let natloop_cmd : Command.t =
  Command.basic_spec ~summary:"get natural loops"
    NatLoop.spec
    NatLoop.run

let main : Command.t = 
  Command.group ~summary:"pick an optimization"
    [("cfg", cfg_cmd);
     ("dce", dce_cmd);
     ("lvn", lvn_cmd);
     ("lvn-dce", lvn_dce_cmd);
     ("reach", reach_cmd);
     ("live-vars", live_vars_cmd);
     ("const-prop", cpd_cmd);
     ("doms", dom_cmd);
     ("dom-tree", dom_t_cmd);
     ("dom-frontiers", dom_f_cmd);
     ("dom-all", dom_all_cmd);
     ("to-ssa", to_ssa_cmd);
     ("from-ssa", from_ssa_cmd);
     ("from-ssa-opt", from_ssa_opt_cmd);
     ("natloops", natloop_cmd)
    ]

let () = Command.run main



