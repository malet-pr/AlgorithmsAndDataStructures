open Models


let leaf value =
  BNode (value, Empty, Empty)

let node value left right =
  BNode (value, left, right)

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

let rec map f tree =
  match tree with
  | Empty -> Empty
  | BNode (v, left, right) -> BNode(f v, map f left, map f right)

let rec sum_values tree =
  match tree with
  | Empty -> 0
  | BNode (v, left, right) ->
      v + sum_values left + sum_values right

let rec product_values tree =
  match tree with
  | Empty -> 1
  | BNode (v, left, right) ->
      v * product_values left * product_values right

let rec all_positive tree =
  match tree with
  | Empty -> true
  | BNode (v, left, right) ->
      v > 0
      && all_positive left
      && all_positive right

let rec contains_negative tree =
  match tree with
  | Empty -> false
  | BNode (v, left, right) ->
      v < 0
      || contains_negative left
      || contains_negative right

let rec count_even tree =
  match tree with
  | Empty -> 0
  | BNode (v, left, right) -> (if v mod 2 = 0 then 1  else 0) + count_even left + count_even right

let rec contains_value_greater_than limit tree =
  match tree with
  | Empty -> false
  | BNode (v, left, right) -> (if v > limit then true else false) || contains_value_greater_than limit left || contains_value_greater_than limit right

let rec collect_positive tree =
  match tree with
  | Empty -> []
  | BNode (v, left, right) -> 
    if v > 0 then v :: ( collect_positive left @ collect_positive right )
    else  ( collect_positive left @ collect_positive right )

let rec triple_values tree =
  match tree with
  | Empty -> Empty
  | BNode (v, left, right) -> BNode (3 * v, triple_values left, triple_values right)

let rec fold_tree acc init tree =
  match tree with
  | Empty -> init
  | BNode (v, left, right) -> acc v (fold_tree acc init left ) (fold_tree acc init right)
  
let count_nodes_fold tree =
  fold_tree (fun v left right -> 1 + left + right) 0 tree

let count_by_fold pr tree =
  fold_tree (fun v left right -> (if pr v  then 1 else 0) + left + right) 0 tree

let find_by_fold pr tree =
  fold_tree (fun v left right -> (pr v || left || right)) false (tree : 'a b_tree)

let rec filter_prune g tree =
  match tree with
  | Empty -> Empty
  | BNode(v, left, right) -> if g v then BNode(v, (filter_prune g left ), (filter_prune g right)) else Empty

let rec filter_option g tree =
  match tree with
  | Empty -> Empty
  | BNode (v, left, right) -> BNode ((if g v then Some v else None),filter_option g left,filter_option g right)

let rec mirror tree =
  match tree with
  | Empty -> Empty
  | BNode (v, left, right) -> BNode (v, mirror right, mirror left)

let rec find_path tree value =
  match tree with
  | Empty -> []
  | BNode (v, left, right) ->
      if v = value then [v]
      else
        let left_path = find_path left value in
        if left_path <> [] then
          v :: left_path
        else
          let right_path = find_path right value in
          if right_path <> [] then
            v :: right_path
          else
            []