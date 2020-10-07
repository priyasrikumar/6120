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
    mutable parent : node option ;
    mutable children : node list ;
  }
  module KTbl = Hashtbl.Make(K)

  (* get stuff *)
  let get_label ~node:n = n.label
  let get_parents ~node:n = n.parent
  let get_children ~node:n = n.children

  (* auxilaries *)
  let has_parent ~node:n = Option.is_some n.parent
  let has_children ~node:n = List.is_empty n.children |> not

  (* some node creation *)
  let create_node ~label:l =
    {
      label = l ;
      parent = None ;
      children = [] ;
    }

  let create_child ~label:l ~parent:p =
    let child = {
        label = l ;
        parent = Some (p) ;
        children = [] ;
      }
    in
    p.children <- child :: p.children;
    child

  let create_parent ~label:l ~child:c =
    let parent = {
        label = l;
        parent = None ;
        children = [c] ;
      }
    in
    c.parent <- Some (parent);
    parent

  (* hookup two nodes *)
  let connect_nodes ~parent:p ~child:c =
    if has_parent ~node:c then
      Invalid_argument "Nodes can only have one parent." |> raise
    else
      p.children <- c::p.children;
      c.parent <- Some (p)

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
    (* let visit_tbl = KTbl.create () in *)
    let rec preds n =
      match n.parent with
      | None -> []
      | Some (p) -> p :: preds p
    in
    (*List.concat_map (Option.) ~f:(fun n ->
      if KTbl.mem visit_tbl n.label then []
      else begin
        KTbl.add_exn visit_tbl ~key:n.label ~data:();
        preds n
      end)
    in*)
    List.concat_map n ~f:preds
end

module DominatorTree = Tree(struct 
      type t = string 
      let hash = String.hash
      let compare = String.compare 
      let sexp_of_t = String.sexp_of_t
      let t_of_sexp = String.t_of_sexp
    end)