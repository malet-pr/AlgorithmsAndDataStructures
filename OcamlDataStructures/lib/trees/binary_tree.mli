open Models


val leaf : 'a -> 'a b_tree
val node : 'a -> 'a b_tree -> 'a b_tree -> 'a b_tree

val count_nodes : 'a b_tree -> int
val count_leaves : 'a b_tree -> int
val calculate_depth : 'a b_tree -> int

val preorder : 'a b_tree -> 'a list
val inorder : 'a b_tree -> 'a list
val postorder : 'a b_tree -> 'a list
val order : 'a b_tree -> order_variant -> 'a list -> 'a list

val find_value : 'a -> 'a b_tree -> bool
val find_by : ('a -> bool) -> 'a b_tree -> bool
val count_by : ('a -> bool) -> 'a b_tree -> int

val map : ('a -> 'b) -> 'a b_tree -> 'b b_tree