open Core
open Types

type cfg_func =
  {
    func: func ;
    blocks : (lbl * instr list) list ;
    cfg_succ : (lbl, lbl list) Hashtbl.t ;
    cfg_pred : (lbl, lbl list) Hashtbl.t ;
  }
type cfg = cfg_func list

type dom_t =
  {
    dom : (arg, arg Hash_set.t) Hashtbl.t ;
    dt : (lbl, lbl Hash_set.t) Hashtbl.t ;
    df : (lbl, lbl Hash_set.t) Hashtbl.t ;
    natloops : (lbl * lbl list) list ;
  }
type doms = (lbl * dom_t) list

val extract_cfg : prog -> cfg

val traverse_cfg_pre : lbl -> cfg_func -> lbl list
val traverse_cfg_post : lbl -> cfg_func -> lbl list

val doms : cfg -> doms

val prog_from_cfg : cfg -> prog

val scc : cfg_func -> arg list list

val natloops : cfg_func -> (arg, arg Hash_set.t) Hashtbl.t -> (lbl * lbl list) list
