open Core
open Types.Bril_types

type blocks_t = (lbl * instr list) list
type cfg_t = (lbl, lbl list) Hashtbl.t

val extract_cfg : func list -> blocks_t * cfg_t 

val traverse_cfg : lbl -> cfg_t -> lbl list