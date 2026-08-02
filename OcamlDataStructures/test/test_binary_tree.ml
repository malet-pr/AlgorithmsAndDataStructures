open DataStructures
open Test_models

module BT = DataStructures.Trees.Binary_tree
module TM = DataStructures.Trees.Models

let leaf v = TM.BNode (v, TM.Empty, TM.Empty)

let node v left right = TM.BNode (v, left, right)

let tree1 =
  node 10
    (leaf 5)
    (node 20
       TM.Empty
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
    ("empty tree", 0, TM.Empty);
  ]

let count_leaves_cases =
  [
    ("small tree", 2, tree1);
    ("larger tree", 4, tree2);
    ("empty tree", 0, TM.Empty);
  ]

let calculate_depth_cases = 
  [
    ("small tree", 3, tree1);
    ("larger tree", 3, tree2);
    ("empty tree", 0, TM.Empty);
  ]  

let find_value_cases =
  [
    ("small tree has 5", 5,true, tree1);
    ("small tree does not have 55", 55,false, tree1);
    ("larger tree has 9", 9,true, tree2);
    ("larger tree does not have 60", 60,false, tree2);
    ("empty tree does not have any", 5,false, TM.Empty);
  ]

let preorder_test_cases = 
  [
    ("small tree preorder",[10;5;20;15],tree1);
    ("large tree preorder",[10;5;7;9;20;12;15],tree2);
  ]

  let inorder_test_cases =
  [
    ("small tree inorder", [5; 10; 20; 15], tree1);
    ("large tree inorder", [7; 5; 9; 10; 12; 20; 15], tree2);
  ]

let postorder_test_cases =
  [
    ("small tree postorder", [5; 15; 20; 10], tree1);
    ("large tree postorder", [7; 9; 5; 12; 15; 20; 10], tree2);
  ]

let order_test_cases =
  [
    ("preorder", TM.Pre, [], [10; 5; 20; 15], tree1);
    ("inorder", TM.In, [], [5; 10; 20; 15], tree1);
    ("postorder", TM.Post, [], [5; 15; 20; 10], tree1);
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

let make_int_list_test t_case f (name,expected,tree) =
  Alcotest.test_case name `Quick (fun () -> 
    Alcotest.(check (list int))
    t_case
    expected
    (f tree)
  )  
  
let make_int_list_test_with_param t_case f (name,param,acc,expected,tree) =
  Alcotest.test_case name `Quick (fun () -> 
    Alcotest.(check (list int))
    t_case
    expected
    (f tree param acc)
  )  


let () =
  Alcotest.run "Binary tree tests"
    [
      ("count_nodes", List.map (make_count_test "count_nodes" BT.count_nodes) count_nodes_cases);
      ("count_leaves", List.map (make_count_test "count_leaves" BT.count_leaves) count_leaves_cases);
      ("calculate_depth", List.map (make_count_test "calculate_depth" BT.calculate_depth) calculate_depth_cases);
      ("find_value", List.map (make_bool_test "find_value" BT.find_value ) find_value_cases);
      ("preorder", List.map (make_int_list_test "preorder" BT.preorder) preorder_test_cases);
      ("inorder", List.map (make_int_list_test "inorder" BT.inorder) inorder_test_cases);
      ("postorder", List.map (make_int_list_test "postorder" BT.postorder) postorder_test_cases);
      ("order", List.map (make_int_list_test_with_param "order" BT.order) order_test_cases);
    ]

