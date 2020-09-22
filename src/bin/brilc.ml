open Core
open Yojson.Basic
open Cfg 
open Json_processor
open Dce 
open Lvn 

module DCE = struct 
  let spec = Command.Spec.(
    empty 
    +> anon (maybe ("file" %: string)))

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
  let spec = Command.Spec.(
    empty 
    +> anon (maybe ("file" %: string)))

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
  let spec = Command.Spec.(
    empty 
    +> anon (maybe ("file" %: string)))

  let run file () = 
     
    | None -> let prog = parse_in in 
      let blocks, cfg_succ, _cfg_pred = extract_cfg prog in 
      let prog', blocks', cfg_succ' = lvn prog blocks cfg_succ in 
      let res = dce prog' blocks' cfg_succ' in 
      to_channel stdout (res |> to_json) 
    | Some file -> let prog = parse_prog file in 
      let blocks, cfg_succ, _cfg_pred = extract_cfg prog in 
      let prog', blocks', cfg_succ' = lvn prog blocks cfg_succ in 
      let res = dce prog' blocks' cfg_succ' in 
      to_channel stdout (res |> to_json) 
end

let lvn_dce_cmd : Command.t = 
  Command.basic_spec ~summary:"local value numbering, then dead code elimination"
    LVN_DCE.spec
    LVN_DCE.run 

let main : Command.t = 
  Command.group ~summary:"pick an optimization or two"
  [("dce", dce_cmd);
    ("lvn", lvn_cmd);
    ("lvn-dce", lvn_dce_cmd)]

let () = Command.run main



