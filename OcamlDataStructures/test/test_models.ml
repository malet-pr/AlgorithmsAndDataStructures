
(*********************** TYPES FOR TESTS ****************************)  

type class_variant =
| Event
| Network
| Invalid

type wo = {wo_number: string; wo_class: class_variant}


(*********************** COMPARATORS ********************************)  


let compare_wo x y =
  compare (int_of_string x.wo_number) (int_of_string y.wo_number)
