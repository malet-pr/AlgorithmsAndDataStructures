open Models

val sum_values : int b_tree -> int
val product_values : int b_tree -> int
val all_positive : int b_tree -> bool
val contains_negative : int b_tree -> bool
val count_even : int b_tree -> int
val contains_value_greater_than : 'a -> 'a b_tree -> bool
val collect_positive : int b_tree -> int list
val triple_values : int b_tree -> int b_tree
val sum_values_fold : int b_tree -> int
val collect_positive_fold : int b_tree -> int list