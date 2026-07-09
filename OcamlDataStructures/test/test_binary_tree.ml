open DataStructures
open Trees.Binary_tree
open Trees.Models
open Test_models

let leaf v = BNode (v, Empty, Empty)

let node v left right = BNode (v, left, right)

let tree1 =
  node 10
    (leaf 5)
    (node 20
       Empty
       (leaf 15))

let tree2 =
  node 10
    (node 5
       (leaf 7)
       (leaf 9))
    (node 20
       (leaf 12)
       (leaf 15))

(**************************************)
let count_nodes_cases =
  [
    ("small tree", 4, tree1);
    ("larger tree", 7, tree2);
    ("empty tree", 0, Empty);
  ]

let count_leaves_cases =
  [
    ("small tree", 2, tree1);
    ("larger tree", 4, tree2);
    ("empty tree", 0, Empty);
  ]

let calculate_depth_cases = 
  [
    ("small tree", 3, tree1);
    ("larger tree", 3, tree2);
    ("empty tree", 0, Empty);
  ]  

let find_value_cases =
  [
    ("small tree has 5", 5,true, tree1);
    ("small tree does not have 55", 55,false, tree1);
    ("larger tree has 9", 9,true, tree2);
    ("larger tree does not have 60", 60,false, tree2);
    ("empty tree does not have any", 5,false, Empty);

  ]

(***********************************************)

let make_count_test t_case f (name, expected, tree) = 
  Alcotest.test_case name `Quick (fun () ->
    Alcotest.(check int)
      t_case
      expected
      (f tree)
  )  

let make_bool_test t_case f (name, param, expected, tree) = 
  Alcotest.test_case name `Quick (fun () ->
    Alcotest.(check bool)
      t_case
      expected
      (f param tree)
  )    

let () =
  Alcotest.run "Binary tree tests"
    [
      ("count_nodes", List.map (make_count_test "count_nodes" count_nodes) count_nodes_cases);
      ("count_leaves", List.map (make_count_test "count_leaves" count_leaves) count_leaves_cases);
      ("calculate_depth", List.map (make_count_test "calculate_depth" calculate_depth) calculate_depth_cases);
      ("find_value", List.map (make_bool_test "find_value" find_value ) find_value_cases);
    ]

