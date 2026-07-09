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



