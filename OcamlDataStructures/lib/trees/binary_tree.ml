open Models

(*** 
  First exercises:
    Count all nodes.
    Count leaves.
    Calculate depth.
    Search for a value.
    Count occurencies. (extra)
***)
let rec count_nodes tree =  
  match tree with  
  | Empty -> 0  
  | BNode (_,Empty,Empty) -> 1
  | BNode (_, left, right) -> 1 + count_nodes left + count_nodes right

let rec count_leaves tree =
  match tree with
  | Empty -> 0
  | BNode (v, Empty, Empty) -> 1 
  | BNode (v, left, right) -> count_leaves left + count_leaves right

let rec  calculate_depth tree =
  match tree with
  | Empty -> 0
  | BNode (_,left,right) -> 1 + max (calculate_depth left) (calculate_depth right)

let rec find_value value tree =
  match tree with
  | Empty -> false
  | BNode (v, left, right) -> v = value || find_value value left || find_value value right

let rec find_by predicate tree =
  match tree with
  | Empty -> false
  | BNode (v, left, right) ->
      predicate v || find_by predicate left || find_by predicate right

let rec count_by predicate tree = 
  match tree with
  | Empty -> 0
  | BNode (v, left, right) -> 
        let current = if predicate v then 1 else 0 in
        current + count_by predicate left + count_by predicate right



(***********************************************************************)
(*
     10  
    /  \  
  5    20
         \
          15
*)
let tree =
  BNode (10,
    BNode (5, Empty, Empty),
    BNode (20, Empty, BNode (15, Empty, Empty))
  )

let wo_tree =
  BNode ({wo_number="AB123"; wo_class=Event},
    BNode ({wo_number="AB22L"; wo_class=Invalid "bad data"}, Empty, Empty),
    BNode ({wo_number="AB22R"; wo_class=Event}, Empty, BNode ({wo_number="AB321"; wo_class=Network}, Empty, Empty))
  )  

let run() =
  print_endline("Number of nodes = " ^ string_of_int (count_nodes tree));
  print_endline("Number of leaves = " ^ string_of_int (count_leaves tree));
  print_endline("Depth = " ^ string_of_int (calculate_depth tree));
  print_endline("Has 55 = " ^ string_of_bool (find_value 55 tree));
  print_endline("Has 5 = " ^ string_of_bool (find_value 5 tree)); 
  print_endline("Has Event = " ^ string_of_bool (find_by (fun wo -> wo.wo_class = Event) wo_tree));
  print_endline("Count Event = " ^ string_of_int (count_by (fun wo -> wo.wo_class = Event) wo_tree));
  print_endline("Count Network = " ^ string_of_int (count_by (fun wo -> wo.wo_class = Network) wo_tree));
  print_endline("Count NonExistent woNumber = " ^ string_of_int (count_by (fun wo -> wo.wo_number = "XXX") wo_tree));
  print_endline("Count Invalid \"bad data\" = " ^ string_of_int (count_by (fun wo -> wo.wo_class = Invalid "bad data") wo_tree));
  print_endline("Count Invalid \"other thing\" = " ^ string_of_int (count_by (fun wo -> wo.wo_class = Invalid "other thing") wo_tree));



