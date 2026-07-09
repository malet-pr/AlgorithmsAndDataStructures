
type 'a b_tree =
  | Empty
  | BNode of 'a * 'a b_tree * 'a b_tree

type order_variant =
  | Pre
  | In
  | Post


