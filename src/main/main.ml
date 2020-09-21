open Core
open Cfg
open Dce
open Lvn
open Json_processor

let test_out out fp ext = out ^ "/" ^ (List.nth_exn (Filename.parts (Filename.chop_extension fp))(List.length (Filename.parts (Filename.chop_extension fp)) - 1)) ^ ext 

let valid_file src pth = 
  let fp = Filename.concat src pth in 
  if not (Stdlib.Sys.file_exists fp) then 
    failwith ("Could not find " ^ pth) else
  if Filename.check_suffix pth ".json" then fp 
  else failwith ("pls give json at" ^ pth)

let write_dir p = Unix.mkdir_p (Filename.dirname p)

type opts = {
  dce : bool; 
  lvn : bool; 
  lvn_dce : bool
}

let opt_param : opts Command.Param.t = 
  let open Command.Let_syntax in 
  [%map_open 
    let dce = flag "--dce" no_arg ~doc:"Run local + global dead code elimination"
    and lvn = flag "--lvn" no_arg ~doc:"Run local value numbering on each block of the program"
    and lvn_dce = flag "--lvn-dce" no_arg ~doc:"Run LVN, then DCE"
    in {dce; lvn; lvn_dce}]

type info = {
  output_dir : string;
  source_dir : string;
  pipeline : bool 
}

let run_param : info Command.Param.t = 
  let open Command.Let_syntax in 
  [%map_open 
    let source_dir = flag_optional_with_default_doc "-source" string sexp_of_string 
        ~default:(Sys.getcwd ()) ~doc:"source file directory"
    and output_dir = flag_optional_with_default_doc "-out" string sexp_of_string
        ~default:(Sys.getcwd ()) ~doc:"output file directory"
    and pipeline = flag "-p" no_arg ~doc:"piping?" 
    in {source_dir; output_dir; pipeline}]

let run info opts path = 
  let file = valid_file info.source_dir path in 
  let prog = parse_prog file in 
  let blocks, cfg_succ, _cfg_pred = extract_cfg prog in 
  let _ =
    if opts.dce 
    then let res = dce prog blocks cfg_succ |> to_json in
      let out = test_out info.output_dir path "_dce.json" in 
      let oc = Out_channel.create out in 
      Yojson.Basic.to_channel oc res 
  in let _ = 
    if opts.lvn 
      then let (res, _, _) = lvn prog blocks cfg_succ in 
      let out = test_out info.output_dir path "_lvn.json" in 
      let oc = Out_channel.create out in 
      Yojson.Basic.to_channel oc (res |> to_json) 
  in let _ = 
     if opts.lvn_dce
      then let (prog', blocks', cfg_succ') = lvn prog blocks cfg_succ in
      let res = dce prog' blocks' cfg_succ' in 
      let out = test_out info.output_dir path "_lvn_dce.json" in 
      let oc = Out_channel.create out in 
      Yojson.Basic.to_channel oc (res |> to_json)
  in ()

let pipe opts = 
  let prog = parse_pipe in 
  let blocks, cfg_succ, _cfg_pred = extract_cfg prog in 
  let _ =
    if opts.dce 
    then let res = dce prog blocks cfg_succ |> to_json in
      let oc = stdout in 
      Yojson.Basic.to_channel oc res 
  in let _ = 
    if opts.lvn 
      then let (res, _, _) = lvn prog blocks cfg_succ in 
      let oc = stdout in    
      Yojson.Basic.to_channel oc (res |> to_json) 
  in let _ = 
     if opts.lvn_dce
      then let (prog', blocks', cfg_succ') = lvn prog blocks cfg_succ in
      let res = dce prog' blocks' cfg_succ' in 
      let oc = stdout in     
      Yojson.Basic.to_channel oc (res |> to_json)
  in ()

let command = let open Command.Let_syntax in
  Command.basic ~summary:"6120"
    [%map_open
      let files = anon (Command.Anons.sequence ("FILE" %: string))
      and opts = opt_param 
      and info = run_param
      and help_output = help in
      fun () ->
        if info.pipeline then
          pipe opts
        else (if List.is_empty files then
          print_endline (Lazy.force help_output)
        else
          List.iter files ~f:(fun x -> run info opts x))]

let () = Command.run command


