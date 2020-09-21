open Types.Bril_types
open Core
open Sexplib0.Sexp_conv

type lvn_num = int
[@@deriving show, sexp]

type lvn_exp =
    Bin of binop * lvn_num * lvn_num
  | Un of unop * lvn_num
  | CstI of int
  | CstB of bool
[@@deriving show, sexp]

type tbl_val = lvn_num * lvn_exp * arg
[@@deriving show]

let is_tbl_val_eq (num1,exp1,arg1) (num2,exp2,arg2) =
  num1 = num2 && exp1 = exp2 && arg1 = arg2

let is_add_mul = function
  | Add | Mul -> true
  | _ -> false

let is_cmp = function
  | Lt | Gt | Le | Ge | Eq -> true
  | _ -> false

let is_bool = function
  | Or | And -> true
  | _ -> false

let flip_cmp_op op = match op with
  | Lt -> Gt | Gt -> Lt
  | Le -> Ge | Ge -> Le
  | Eq -> Eq | _ -> Stdlib.invalid_arg "Should not encounter non-cmp op here."

type binop_fun = Arith of (int -> int -> int)
               | Cmp of (int -> int -> bool)
               | Logic of (bool -> bool -> bool)

let binop_to_fun op = match op with
  | Add -> Arith (+) | Mul -> Arith ( * )
  | Sub -> Arith (-) | Div -> Arith (/)
  | Le -> Cmp (<=) | Ge -> Cmp (>=)
  | Lt -> Cmp (<) | Gt -> Cmp (>)
  | Eq -> Cmp (=)
  | And -> Logic (&&) | Or -> Logic (||)
let unop_to_fun op = match op with
  | Not -> `Logic (not) | Id -> `Id (Fun.id)

module LvnExpHashable = struct
  type t = lvn_exp [@@deriving sexp]
  let compare = Stdlib.compare
  let hash = Hashtbl.hash
end
