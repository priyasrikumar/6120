open Types
open Yojson.Basic

exception Not_yet_implemented of string

let binop_tbl =
  Seq.(empty |>
       cons ("add", Add) |>
       cons ("mul", Mul) |>
       cons ("sub", Sub) |>
       cons ("div", Div) |>
       cons ("eq", Eq) |>
       cons ("lt", Lt) |>
       cons ("gt", Gt) |>
       cons ("le", Le) |>
       cons ("ge", Ge) |>
       cons ("and", And) |>
       cons ("or", Or)
      ) |> Hashtbl.of_seq

let unop_tbl =
  Seq.(empty |>
       cons ("not", Not) |>
       cons ("id", Id)
      ) |> Hashtbl.of_seq

let raise_not_impl str json =
  Not_yet_implemented (Printf.sprintf "%s: %s" str (to_string json)) |> raise
let raise_invalid_arg str json =
  Invalid_argument (Printf.sprintf "%s: %s" str (to_string json)) |> raise

let to_typ = function
  | `String "int" -> Int
  | `String "bool" -> Bool
  | json -> raise_not_impl "Invalid type" json

let rec to_ptr_typ json : ptr_typ =
  match json |> Util.member "ptr" with
  | `String "int" -> Ptr (Base (Int))  
  | `String "bool" -> Ptr (Base (Bool))
  | more ->
    let t = to_ptr_typ more in
    Ptr (t)

let to_typ_opt = function
  | `Null -> None
  | json -> Some (to_typ json)

let to_ptr_typ_opt = function
  | `Null -> None
  | json -> Some (to_ptr_typ json)

let parse_instr json =
  let open Util in
  match json |> member "label" with
  | `String lbl -> Label (lbl) 
  | `Null -> begin
      let dst () = json |> member "dest" |> to_string in
      let typ () = json |> member "type" |> to_typ in
      let ptr_typ () : ptr_typ = json |> member "type" |> to_ptr_typ in
      match json |> member "op" with
      | `String "const" ->
        let typ = typ () in 
        let cst = match typ with
          | Int -> IntC (json |> member "value" |> to_int)
          | Bool -> BoolC (json |> member "value" |> to_bool)
        in
        Cst (dst (), typ, cst) 
      | `String op when Hashtbl.mem binop_tbl op ->
        let op = Hashtbl.find binop_tbl op in
        let arg1, arg2 = json |> member "args" |> to_list |> function
          | arg1 :: arg2 :: [] -> arg1 |> to_string, arg2 |> to_string
          | _ -> raise_invalid_arg "Invalid binop" json
        in
        Binop (dst (), typ (), op, arg1, arg2)
      | `String op when Hashtbl.mem unop_tbl op ->
        let op = Hashtbl.find unop_tbl op in
        let arg = json |> member "args" |> to_list |> function
          | arg :: [] -> arg |> to_string
          | _ -> raise_invalid_arg "Invalid unop" json
        in
        begin try
          ignore (json |> member "type" |> to_assoc);
          Ptrcpy (dst (), ptr_typ (), arg)
        with Type_error (_, _) -> Unop (dst (), typ (), op, arg) end
      | `String "jmp" ->
        let lbl = json |> member "labels" |> to_list |> function
          | lbl :: [] -> lbl |> to_string
          | _ -> raise_invalid_arg "Invalid jump" json
        in
        Jmp (lbl)
      | `String "br" ->
        let arg = json |> member "args" |> to_list |> function
          | arg :: [] -> arg |> to_string
          | _ -> raise_invalid_arg "Invalid branch" json
        in
        let lbl1, lbl2 = json |> member "labels" |> to_list |> function
          | lbl1 :: lbl2 :: [] -> lbl1 |> to_string, lbl2 |> to_string
          | _ -> raise_invalid_arg "Invalid branch" json
        in
        Br (arg, lbl1, lbl2)
      | `String "call" ->
        let dst = match json |> member "dest" with
          | `Null -> None
          | _ -> Some (dst ())
        in
        let typ = if dst = None then None else Some (typ ()) in
        let name = json |> member "funcs" |> to_list |> function
          | name :: [] -> name |> to_string
          | _ -> raise_invalid_arg "Invalid function call" json
        in 
        let args = json |> member "args" |> to_list |> List.map to_string in 
        Call (dst, typ, name, if args = [] then None else Some args)
      | `String "ret" ->
        let arg = json |> member "args" |> function
          | `Null -> None
          | json -> to_list json |> function  
            | arg :: [] -> Some (arg |> to_string)
            | _ -> raise_invalid_arg "Invalid return" json
        in
        Ret (arg)
      | `String "print" ->
        Print (json |> member "args" |> to_list |> List.map to_string)
      | `String "nop" ->
        Nop
      | `String "phi" ->
        let labels = json |> member "labels" |> to_list |> List.map to_string in
        let args = json |> member "args" |> to_list |> List.map to_string in
        let phis = List.map2 (fun lbl arg -> (lbl,arg)) labels args in
        let typ =
          try
            ignore (json |> member "type" |> to_assoc);
            Ptr (ptr_typ ())
          with Type_error (_, _) -> Val (typ ())
        in 
        Phi (dst (), typ, phis)
      | `String "alloc" ->
        let arg = json |> member "args" |> to_list |> List.hd |> to_string in
        Alloc (dst (), ptr_typ (), arg)
      | `String "free" ->
        let arg = json |> member "args" |> to_list |> List.hd |> to_string in
        Free (arg)
      | `String "store" ->
        let args = json |> member "args" |> to_list |> List.map to_string in
        let arg1 = args |> List.hd in
        let arg2 = args |> List.tl |> List.hd in
        Store (arg1, arg2)
      | `String "load" ->
        let typ =
          try
            ignore (json |> member "type" |> to_assoc);
            ptr_typ ()
          with Type_error (_, _) -> Base (typ ())
        in
        let arg = json |> member "args" |> to_list |> List.hd |> to_string in
        Load (dst (), typ, arg)
      | `String "ptradd" ->
        let typ =
          try
            ignore (json |> member "type" |> to_assoc);
            ptr_typ ()
          with Type_error (_, _) -> Base (typ ())
        in
        let args = json |> member "args" |> to_list |> List.map to_string in
        let arg1 = args |> List.hd in
        let arg2 = args |> List.tl |> List.hd in
        Ptradd (dst (), typ, arg1, arg2)
      | _ ->
        raise_invalid_arg "Invalid op" json
    end
  | _ -> raise_invalid_arg "Invalid instruction" json

let parse_func json =
  let open Util in
  let name = json |> member "name" |> to_string in
  let args = json |> member "args" |> function
    | `Null -> None
    | (`List _) as ls -> Some (ls |> to_list |> List.map (fun json ->
        let arg = json |> member "name" |> to_string in
        let typ =
          try
            ignore (json |> member "type" |> to_assoc);
            Ptr (json |> member "type" |> to_ptr_typ)
          with Type_error (_, _) -> Val (json |> member "type" |> to_typ)
        in
        arg, typ))
    | _ -> raise_invalid_arg "Invalid function arguments" json
  in
  let rtyp =
    try
      ignore (json |> member "type" |> to_assoc);
      match json |> member "type" |> to_ptr_typ_opt with
      | None -> None
      | Some (typ) -> Some (Ptr (typ))
    with Type_error (_, _) -> begin
      match json |> member "type" |> to_typ_opt with
      | None -> None
      | Some (typ) -> Some (Val (typ))
    end
  in
  let instrs = json |> member "instrs" |> to_list |> List.map parse_instr in
  {
    name = name ;
    args = args ;
    rtyp = rtyp ;
    instrs = instrs ;
  }

let parse_prog fname = 
  let json = from_file fname in
  Util.(json |> member "functions" |> to_list |> List.map parse_func)

let parse_in = 
  let json = from_channel stdin in
  Util.(json |> member "functions" |> to_list |> List.map parse_func)

let rev_typ = function
  | Int -> `String "int" 
  | Bool -> `String "bool"

let rev_binop_tbl =
  Hashtbl.to_seq binop_tbl |>
  Seq.map (fun (a,b) -> (b,a)) |>
  Hashtbl.of_seq

let rev_unop_tbl =
  Hashtbl.to_seq unop_tbl |>
  Seq.map (fun (a,b) -> (b,a)) |>
  Hashtbl.of_seq

let ptr_typ_to_json ptr_typ =
  let rec to_json = function 
    | Base _ -> failwith "should be unreachable"
    | Ptr (Base (t)) -> `Assoc [("ptr", rev_typ t)]
    | Ptr (rest) ->
      let inner = to_json rest in
      `Assoc [("ptr", inner)]
  in
  to_json ptr_typ

let load_typ_to_json ptr_typ =
  match ptr_typ with
  | Base (t) -> rev_typ t
  | _ -> ptr_typ_to_json ptr_typ

let instr_to_json instr =
  let to_args args = `List (List.map (fun x -> `String x) args) in
  let to_labels labels = to_args labels in 
  let assoc = match instr with
    | Label (lbl) -> [("label", `String lbl)]
    | Cst (d, t, cst) ->
      let cst = match cst with
        | IntC (i) -> ("value", `String (string_of_int i))
        | BoolC (b) -> if b then ("value", `String "true") else ("value", `String "false")
      in
      [
        ("dest", `String d) ; 
        ("op", `String "const") ;
        ("type", rev_typ t) ; 
        cst ;
      ]
    | Binop (d, t, op, arg1, arg2) ->
      [
        ("args", `List [`String arg1 ; `String arg2]) ;
        ("dest", `String d) ;
        ("op", `String (Hashtbl.find rev_binop_tbl op)) ;
        ("type", rev_typ t) ;
      ]
    | Unop (d, t, op, arg) ->
      [
        ("args", `List [`String arg]) ;
        ("dest", `String d) ;
        ("type", rev_typ t) ;
        ("op", `String (Hashtbl.find rev_unop_tbl op)) ;
      ]
    | Jmp (lbl) ->
      [
        ("labels", `List [`String lbl]) ;
        ("op", `String "jmp") ;
      ]
    | Br (arg, l1, l2) ->
      [
        ("args", `List [`String arg]) ;
        ("labels", `List [`String l1; `String l2]) ;
        ("op", `String "br")
      ]
    | Call (None, _t, n, None) ->
      [
        ("funcs", `List [`String n]) ; 
        ("op", `String "call")
      ]
    | Call (None, _t, n, Some (args)) ->
      [
        ("args", to_args args);
        ("funcs", `List [`String n]) ;
        ("op", `String "call") ;
      ]
    | Call (Some (d), t, n, None) ->
      [
        ("dest", `String d) ;
        ("funcs", `List [`String n]) ;
        ("op", `String "call") ;
        ("type", Option.get t |> rev_typ) ;
      ] 
    | Call (Some (d), t, n, Some (args)) ->
      [
        ("args", to_args args) ;
        ("dest", `String (d)) ;
        ("funcs", `List [`String n]) ;
        ("op", `String "call") ;
        ("type", Option.get t |> rev_typ) ;
      ]
    | Ret (None) -> [ ("op", `String "ret") ]
    | Ret (Some (arg)) ->
      [
        ("args", `List [`String arg]) ;
        ("op", `String "ret") ;
      ]
    | Print args ->
      [
        ("args", to_args args) ;
        ("op", `String "print") ; 
      ]
    | Nop -> [("op", `String "nop")]
    | Phi (d, Val t, phis) ->
        let labels, args = List.split phis in
        [
          ("args", to_args args) ;
          ("dest", `String (d)) ;
          ("labels", to_labels labels) ;
          ("op", `String ("phi")) ;
          ("type", rev_typ t) ;
        ]
    | Phi (d, Ptr t, phis) ->
        let labels, args = List.split phis in
        [
          ("args", to_args args) ;
          ("dest", `String (d)) ;
          ("labels", to_labels labels) ;
          ("op", `String ("phi")) ;
          ("type", ptr_typ_to_json t) ;
        ]
    | Alloc (d, pt, arg) ->
      [
        ("args", `List [`String arg]) ;
        ("dest", `String (d)) ;
        ("op", `String ("alloc")) ;
        ("type", ptr_typ_to_json pt)
      ]
    | Free (arg) ->
      [
        ("args", `List [`String arg]) ;
        ("op", `String ("free"))
      ]
    | Store (arg1, arg2) ->
      [
        ("args", `List [`String arg1 ; `String arg2]) ;
        ("op", `String ("store"))
      ]
    | Load (d, pt, arg) ->
      [
        ("args", `List [`String arg]) ;
        ("dest", `String (d)) ;
        ("op", `String ("free")) ;
        ("type", load_typ_to_json pt)
      ]
    | Ptradd (d, pt, arg1, arg2) ->
      [
        ("args", `List [`String arg1 ; `String arg2]) ;
        ("dest", `String (d)) ;
        ("op", `String ("ptradd")) ;
        ("type", ptr_typ_to_json pt)
      ]
    | Ptrcpy (d, pt, arg) ->
      [
        ("args", `List [`String arg]) ;
        ("dest", `String (d)) ;
        ("op", `String ("id")) ;
        ("type", ptr_typ_to_json pt)
      ]
    (*| instr -> failwith ("unimplemented : "^show_instr (instr))*)
  in
  `Assoc assoc

let func_to_json func =
  let convert_instrs instrs = `List (List.map instr_to_json instrs) in
  let convert_args args = `List (List.map (fun (d,t) -> 
      match t with
      | Val (t) -> `Assoc ([("name", `String d); ("type", rev_typ t)])
      | Ptr (t) -> `Assoc ([("name", `String d); ("type", ptr_typ_to_json t)])) args)
  in
  let assoc = match func.name, func.args, func.rtyp, func.instrs with
    | name, None, None, instrs ->
      [
        ("instrs", convert_instrs instrs) ;
        ("name", `String name) ;
      ]
    | name, Some (args), None, instrs ->
      [
        ("args", convert_args args) ;
        ("instrs", convert_instrs instrs) ;
        ("name", `String name) ;
      ]
    | name, None, Some (Val (typ)), instrs ->
      [
        ("instrs", convert_instrs instrs) ;
        ("name", `String name) ;
        ("type", rev_typ typ) ;
      ]
    | name, None, Some (Ptr (typ)), instrs ->
      [
        ("instrs", convert_instrs instrs) ;
        ("name", `String name) ;
        ("type", ptr_typ_to_json typ) ;
      ]
    | name, Some (args), Some (Val (typ)), instrs ->
      [
        ("args", convert_args args) ;
        ("instrs", convert_instrs instrs) ;
        ("name", `String name) ;
        ("type", rev_typ typ) ;
      ]
    | name, Some (args), Some (Ptr (typ)), instrs ->
      [
        ("args", convert_args args) ;
        ("instrs", convert_instrs instrs) ;
        ("name", `String name) ;
        ("type", ptr_typ_to_json typ) ;
      ]
  in
  `Assoc assoc

let to_json prog =
  `Assoc (["functions", `List (List.map func_to_json prog)])
