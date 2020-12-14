open Sexplib0.Sexp_conv

type typ = 
  | Int
  | Bool
[@@deriving show, eq, sexp]

type cst = 
  | IntC of int
  | BoolC of bool
[@@deriving show, eq, sexp]

type ptr_typ =
  | Base of typ
  | Ptr of ptr_typ
[@@deriving show, eq, sexp]

type union_typ =
  | Val of typ
  | Ptx of ptr_typ
  | Fun of fun_typ
[@@deriving show, eq, sexp]

and fun_typ = (union_typ list option) * union_typ option

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
  | Call of dst option * union_typ option * string * arg list option
  | Ret of arg option
  | Print of arg list
  | Nop
  | Phi of dst * union_typ * (lbl * arg) list
  | Alloc of dst * ptr_typ * arg
  | Free of arg
  | Store of arg * arg
  | Load of dst * ptr_typ * arg
  | Ptradd of dst * ptr_typ * arg * arg
  | Ptrcpy of dst * ptr_typ * arg
  | Anon of dst * union_typ * arg list option * instr list 
  | Fncall of dst * union_typ * string * arg list option
[@@deriving show, eq, sexp]

type func = {
  name : string ;
  args : ((arg * union_typ) list) option ;
  rtyp : union_typ option ;
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
type arg_list = arg list
[@@deriving show, eq]
type backedge_list = (lbl * lbl) list
[@@deriving show]
 type dom_list = (lbl * lbl list) list
 [@@deriving show]