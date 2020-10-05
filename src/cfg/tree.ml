(**
  * this tree implementation is inspired by the BLAST model checker
  * version 2.5, see http://mtc.epfl.ch/software-tools/blast/index-epfl.php
  * to get the relevant version and download the source distribution
  * see utils/biDirectionalLabeledTree.ml for the inspiration file
  * instead of modifying the existing file we create a new functor
  * that takes a Hashable type (Hashtbl.Key) and make it the label of
  * tree nodes (in our case it will be strings but this implementation
  * is generic enough), then we can create BLAST-like mutable tree nodes
  * and hook them up, find descendants, predecessors etc.
  * at some point it might be useful to also have more functions
  * similar to iter_ancestors or traverse_bfs given in the blast lib
  *)

open Core

module Tree (K : Hashtbl.Key) = struct
  type node = {
    mutable label : K.t ;
    mutable parents : node list ;
    mutable children : node list ;
  }
  module KTbl = Hashtbl.Make(K)

  (* get stuff *)
  let get_label ~node:n = n.label
  let get_parents ~node:n = n.parents
  let get_children ~node:n = n.children

  (* auxilaries *)
  let has_parents ~node:n = List.is_empty n.parents |> not
  let has_children ~node:n = List.is_empty n.children |> not

  (* some node creation *)
  let create_node ~label:l =
    {
      label = l ;
      parents = [] ;
      children = [] ;
    }

  let create_child ~label:l ~parent:p =
    let child = {
        label = l ;
        parents = [p] ;
        children = [] ;
      }
    in
    p.children <- child :: p.children;
    child

  let create_parent ~label:l ~child:c =
    let parent = {
        label = l;
        parents = [] ;
        children = [c] ;
      }
    in
    c.parents <- parent :: c.parents;
    parent

  (* hookup two nodes *)
  let connect_nodes ~parent:p ~child:c =
    p.children <- c::p.children;
    c.parents <- p::c.parents

  (* get all successors of a given node, watch out for cycles *)
  let successors ~node:n =
    let visit_tbl = KTbl.create () in
    let rec succs n = n :: List.concat_map n.children ~f:(fun n ->
      if KTbl.mem visit_tbl n.label then []
      else begin
        KTbl.add_exn visit_tbl ~key:n.label ~data:();
        succs n
      end)
    in
    List.concat_map n.children ~f:succs  

  (* get all parents of a given node, watch out for cycles *)
  let predecessors ~node:n =
    let visit_tbl = KTbl.create () in
    let rec preds n = n :: List.concat_map n.parents ~f:(fun n ->
      if KTbl.mem visit_tbl n.label then []
      else begin
        KTbl.add_exn visit_tbl ~key:n.label ~data:();
        preds n
      end)
    in
    List.concat_map n.parents ~f:preds
end
