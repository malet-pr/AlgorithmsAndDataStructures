
let ints_to_strings intList =
  List.rev (List.fold_left (fun acc x -> string_of_int x :: acc) [] intList)
