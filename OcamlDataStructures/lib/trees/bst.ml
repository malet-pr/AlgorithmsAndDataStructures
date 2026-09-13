open Models
open Binary_tree

let rec search_bst comp x tree =
  match tree with
  | Empty -> false
  | BNode(v,left,right) -> 
      let c = comp v x in
        if c = 0 then true 
        else if c < 0 then search_bst comp x right 
        else search_bst comp x left

