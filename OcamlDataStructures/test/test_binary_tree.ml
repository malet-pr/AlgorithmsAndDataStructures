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

(***********************************************)

let make_count_test t_case f (name, expected, tree) = 
  Alcotest.test_case name `Quick (fun () ->
    Alcotest.(check int)
      t_case
      expected
      (f tree)
  )  

let () =
  Alcotest.run "Binary tree tests"
    [
      ("count_nodes", List.map (make_count_test "count_nodes" count_nodes) count_nodes_cases);
      ("count_leaves", List.map (make_count_test "count_leaves" count_leaves) count_leaves_cases);
    ]

