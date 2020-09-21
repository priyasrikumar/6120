open Core
open Cfg
open Dce
open Lvn
open Json_processor

let mems = ["alloc"; "free"; "store"; "load"; "ptradd"]

let in_mems op = List.mem mems op ~equal:String.equal

let dce (prog,blocks,cfg) = (*let prog = parse_prog path in*)
  (*let blocks, cfg = extract_cfg prog in *)
  let prog' = dce prog blocks cfg in
  Format.printf "%a" Types.Bril_types.pp_prog prog';
  let res = to_json prog' in
  let oc = Out_channel.stdout in
  Yojson.Basic.to_channel oc res
  (*let new_path = Stdlib.String.sub path 0 (String.length path - 5) |>
                 (fun x -> x^"_dce.json")
  in
  let oc = Out_channel.create new_path in
  Yojson.Basic.to_channel oc res*)

let lvn path = let prog = parse_prog path in
  let blocks, cfg = extract_cfg prog in 
  let prog', blocks', cfg' = lvn prog blocks cfg in
  Format.printf "%a \n %a \n %a" Types.Bril_types.pp_blocks_list blocks
    Types.Bril_types.pp_blocks_list blocks'
    Types.Bril_types.pp_prog prog';
  ignore (prog',blocks',cfg')
(*let new_path = Stdlib.String.sub path 0 (String.length path - 5) |>
               (fun x -> x^"_lvn.json")
  in
  let oc = Out_channel.create new_path in
  Yojson.Basic.to_channel oc res*)

let command = let open Command.Let_syntax in
  Command.basic ~summary:"6120"
    [%map_open
      let files = anon (Command.Anons.sequence ("FILE" %: string))
      and help_flag =
        flag "--help" no_arg
          ~doc:"help"
      and help_output = help in
      fun () ->
        if help_flag || Base.List.is_empty files then
          print_endline (Lazy.force help_output)
        else
          List.iter files ~f:(fun x -> lvn x)]

let () = Command.run command


