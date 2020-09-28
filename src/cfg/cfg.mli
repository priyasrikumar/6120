open Core
open Types

type blocks_t = (lbl * instr list) list
type cfg_t = (lbl, lbl list) Hashtbl.t
type dom_t = (arg, arg Hash_set.t) Hashtbl.t

val extract_cfg : prog -> blocks_t * cfg_t * cfg_t

val traverse_cfg_pre : lbl -> cfg_t -> lbl list
val traverse_cfg_post : lbl -> cfg_t -> lbl list

val doms : prog -> blocks_t -> cfg_t -> cfg_t -> dom_t
