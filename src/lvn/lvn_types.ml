open Types.Bril_types
open Core
open Sexplib0.Sexp_conv

type lvn_num = int
[@@deriving show, eq, compare, sexp]

type lvn_exp =
    Bin of binop * lvn_num * lvn_num
  | Un of unop * lvn_num
  | CstI of int
  | CstB of bool
[@@deriving show, eq, compare, sexp]

type tbl_val = lvn_num * lvn_exp * arg
[@@deriving show, eq]

module LvnExpHashable = struct
  type t = lvn_exp [@@deriving compare, sexp]
  let hash = Hashtbl.hash
end

let is_add_mul = function
  | Add | Mul -> true
  | _ -> false

let is_cmp_op = function
  | Lt | Gt | Le | Ge | Eq -> true
  | _ -> false

let is_bool_op = function
  | Or | And -> true
  | _ -> false

let flip_cmp_op op = match op with
  | Lt -> Gt | Gt -> Lt
  | Le -> Ge | Ge -> Le
  | Eq -> Eq | _ -> Stdlib.invalid_arg "Should not encounter non-cmp op here."

let binop_to_fun op = match op with
  | Add -> `Arith (+) | Mul -> `Arith ( * )
  | Sub -> `Arith (-) | Div -> `Arith (/)
  | Le -> `Cmp (<=) | Ge -> `Cmp (>=)
  | Lt -> `Cmp (<) | Gt -> `Cmp (>)
  | Eq -> `Cmp (=)
  | And -> `Logic (&&) | Or -> `Logic (||)
let unop_to_fun op = match op with
  | Not -> `Logic (not) | Id -> `Id (Fun.id)