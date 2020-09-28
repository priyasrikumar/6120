open Sexplib0.Sexp_conv

(* for now lets not support pointer types *)
(* also lets not support floating-point types *)
type typ = 
    Int
  | Bool (*| Pointer of typ | Float of float*)
[@@deriving show, eq, sexp]

type cst = 
    IntC of int
  | BoolC of bool
[@@deriving show, eq, sexp]

type lbl = string [@@deriving show, eq, sexp]
type arg = string [@@deriving show, eq, sexp]
type dst = string [@@deriving show, eq, sexp]

(* binary operations *)
type binop =
    Add  (* arithmetic operations *)
  | Mul
  | Sub
  | Div
  | Eq   (* compare operations *)
  | Lt
  | Gt
  | Le
  | Ge 
  | And  (* logical operations *)
  | Or
[@@deriving show, eq, compare, sexp]

(*| Store | Ptradd*)

(* unary operations *)
type unop =
    Not
  | Id  (*| Alloc | Free | Load*)
[@@deriving show, eq, compare, sexp]

(* types of instructions *)
type instr =
    Label of lbl
  | Cst of dst * typ * cst
  | Binop of dst * typ * binop * arg * arg
  | Unop of dst * typ * unop * arg
  | Jmp of lbl
  | Br of arg * lbl * lbl
  | Call of dst option * typ option * string * arg list option
  | Ret of arg option
  | Print of arg list
  | Nop
[@@deriving show, eq, sexp]

type func = {
  name : string ;
  args : ((arg * typ) list) option ;
  rtyp : typ option ;
  instrs : instr list ;
}
[@@deriving show, eq, sexp]

type prog = func list
[@@deriving show, eq, sexp]

type instr_list = instr list 
[@@deriving show, eq]
type blocks_list = (lbl * instr list) list
[@@deriving show, eq, sexp]
type cfg_list = (lbl * lbl list) list
[@@deriving show, eq, sexp]
type lbl_list = lbl list 
[@@deriving show, eq]