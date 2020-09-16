open Core
open Yojson.Basic

let mems = ["alloc"; "free"; "store"; "load"; "ptradd"]

let in_mems op = List.mem mems op ~equal:String.equal

let count (file : string) = 
  let functions = from_file file |> Util.member "functions" |> Util.to_list in
  let ct = List.fold functions ~init:0 ~f:(fun ct f -> 
      let ins = Util.member "instrs" f |> Util.to_list in 
      List.fold ins ~init:ct ~f:(fun ct i-> match  
                           Util.member "op" i with 
                       | `String s when in_mems s -> ct + 1
                       | _ -> ct)) in Printf.fprintf stdout "Saw %d memory instructions!" ct 

let run path = count path

let command = 
  let open Command.Let_syntax in
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
          List.iter files ~f:run]

let () = Command.run command

