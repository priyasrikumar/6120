open Bril

type blocks_t = (label * instr list) list
type cfg_t = (label, label list) Hashtbl.t

val extract_cfg : func list -> (blocks_t * cfg_t * (label, func) Hashtbl.t) 