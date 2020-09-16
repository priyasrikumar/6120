open Bril

type blocks_t = (label, instr list) Hashtbl.t
type cfg_t = (label, label list) Hashtbl.t

val extract_cfg : t -> (blocks_t * cfg_t)