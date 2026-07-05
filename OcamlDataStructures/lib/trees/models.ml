
type 'a b_tree =
  | Empty
  | BNode of 'a * 'a b_tree * 'a b_tree


(*********************** TYPES FOR TESTS ********************************)  

type class_variant =
| Event
| Network
| Invalid of string

type wo = {wo_number: string; wo_class: class_variant}
