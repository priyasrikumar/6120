open Core
open Types

type blocks_t = (lbl * instr list) list
type cfg_t = (lbl, lbl list) Hashtbl.t

val extract_cfg : prog -> blocks_t * cfg_t * cfg_t

val traverse_cfg_succ : lbl -> cfg_t -> lbl list