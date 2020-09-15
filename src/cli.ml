open Core

let run path = Analyze.count path

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

