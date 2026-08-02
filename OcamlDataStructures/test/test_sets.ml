open DataStructures
open Test_data
open Test_models

module TM = DataStructures.Trees.Models

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

let map_test_cases_int =
  [
    ("map double", (fun x -> x * 2),expected_double,tree1);
  ] 

let map_test_cases_string =
  [
    ("map to string",(fun x -> string_of_int x),expected_string,tree1);
  ]   


(************************************************************************************)  


let find_by_cases =
  [
    ("finds Event", (fun wo -> wo.wo_class = Event), true, wo_tree);
    ("finds Network", (fun wo -> wo.wo_class = Network), true, wo_tree);
    ("does not find XXX", (fun wo -> wo.wo_number = "XXX"), false, wo_tree);
  ]  

let count_by_cases =
  [
    ("counts Event", (fun wo -> wo.wo_class = Event), 1, wo_tree);
    ("counts Network", (fun wo -> wo.wo_class = Network), 1, wo_tree);
    ("counts Invalid", (fun wo ->
       match wo.wo_class with
       | Invalid _ -> true
       | _ -> false),
     0,
     wo_tree);
  ]  
