open Models
open Binary_tree


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

let sum_values_fold tree = 
  fold_tree (fun v left right -> v + left + right) 0 tree

let collect_positive_fold tree =
  fold_tree (fun v left right -> 
                let list = left @ right in
                if v > 0 then v :: list else list
            ) [] tree

            