open Models
open Utils

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

let rec preorder (tree: 'a b_tree):'a list =
  match tree with
  | Empty -> []
  | BNode (v, left, right) -> v :: ( preorder left @ preorder right )

let rec inorder (tree: 'a b_tree):'a list =
  match tree with
  | Empty -> []
  | BNode (v, left, right) -> inorder left @ [v] @ inorder right 

let rec postorder (tree: 'a b_tree):'a list =
  match tree with
  | Empty -> []
  | BNode (v, left, right) -> postorder left @ postorder right @ [v] 

let rec preorder2 (tree: 'a b_tree) (acc: 'a list):'a list =
  match tree with
  | Empty -> acc
  | BNode (v, left, right) -> v :: preorder2 left (preorder2 right  acc)

let rec order (tree: 'a b_tree) (o: order_variant) (acc: 'a list) :'a list =
  match tree with
  | Empty -> acc
  | BNode (v, left, right) ->
    match o with
    | Pre -> v :: order left Pre (order right Pre acc)
    | In -> order left In ( v ::  order right In acc)
    | Post -> order left Post (order right Post (v :: acc))



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

(*
       10  
      /  \  
     5    20
    / \   / \
   7   9 12  15

preorder: 10,5,7,9,20,12,15
inorder: 7,5,9,10,12,20,15
postorder: 7,9,5,12,15,20,10
*)

let tree2 =
  BNode (10,
    BNode (5,BNode(7,Empty,Empty),BNode(9,Empty,Empty)),
    BNode (20, BNode(12,Empty,Empty), BNode (15, Empty, Empty))
  )


let run() = 
(*   print_endline("Number of nodes = " ^ string_of_int (count_nodes tree));
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
  print_endline("Preorder tree: [" ^ String.concat ("; ") (ints_to_strings (preorder tree)) ^ "]");
  print_endline("Preorder tree2: [" ^ String.concat ("; ") (ints_to_strings (preorder tree2)) ^ "]");
  print_endline("Inorder tree: [" ^ String.concat ("; ") (ints_to_strings (inorder tree)) ^ "]");
  print_endline("Inorder tree2: [" ^ String.concat ("; ") (ints_to_strings (inorder tree2)) ^ "]");
  print_endline("Postorder tree: [" ^ String.concat ("; ") (ints_to_strings (postorder tree)) ^ "]");
  print_endline("Postorder tree2: [" ^ String.concat ("; ") (ints_to_strings (postorder tree2)) ^ "]");*)
  print_endline("Preorder tree2: [" ^ String.concat ("; ") (ints_to_strings (order tree2 Pre [])) ^ "]");
  print_endline("Inorder tree2: [" ^ String.concat ("; ") (ints_to_strings (order tree2 In [])) ^ "]");
  print_endline("Postorder tree2: [" ^ String.concat ("; ") (ints_to_strings (order tree2 Post [])) ^ "]");
