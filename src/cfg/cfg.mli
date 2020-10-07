open Core
open Types

type blocks_t = (lbl * instr list) list
type block_map_t = (lbl, instr list) Hashtbl.t
type cfg_t = (lbl, lbl list) Hashtbl.t
type dom_t = (arg, arg Hash_set.t) Hashtbl.t
type dt_t = (lbl, lbl Hash_set.t) Base.Hashtbl.t
type df_t = (lbl, lbl Hash_set.t) Hashtbl.t

val extract_cfg : prog -> blocks_t * cfg_t * cfg_t

val traverse_cfg_pre : lbl -> cfg_t -> lbl list
val traverse_cfg_post : lbl -> cfg_t -> lbl list

val doms : prog -> blocks_t -> cfg_t -> cfg_t -> dom_t
val dt : dom_t -> dt_t
val df : dom_t -> cfg_t -> df_t

val prog_from_block_list : prog -> blocks_t -> cfg_t -> prog
val prog_from_block_map : prog -> block_map_t -> cfg_t -> prog
