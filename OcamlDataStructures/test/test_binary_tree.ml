open DataStructures
open Test_models
open Test_data
open Test_sets

module BT = DataStructures.Trees.Binary_tree
module TM = DataStructures.Trees.Models


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

let make_int_return t_case f (name, g, expected,tree) =
  Alcotest.test_case name `Quick (fun () -> 
    Alcotest.(check (list int))
      (t_case ^ " preorder")
      (BT.preorder expected)
      (BT.preorder( f g tree)
    );
    Alcotest.(check (list int))
      (t_case ^ " inorder")
      (BT.inorder expected)
      (BT.inorder( f g tree)
    );
    Alcotest.(check (list int))
      (t_case ^ " postorder")
      (BT.postorder expected)
      (BT.postorder( f g tree)
    )  
  )

let make_string_return t_case f (name, g, expected,tree) =
  Alcotest.test_case name `Quick (fun () -> 
    Alcotest.(check (list string))
      (t_case ^ " preorder")
      (BT.preorder expected)
      (BT.preorder( f g tree)
    );
    Alcotest.(check (list string))
      (t_case ^ " inorder")
      (BT.inorder expected)
      (BT.inorder( f g tree)
    );
    Alcotest.(check (list string))
      (t_case ^ " postorder")
      (BT.postorder expected)
      (BT.postorder( f g tree)
    )  
  )

let make_predicate_bool_test t_case f (name, predicate, expected, tree) =
  Alcotest.test_case name `Quick (fun () ->
    Alcotest.(check bool)
      t_case
      expected
      (f predicate tree)
  )

let make_predicate_int_test t_case f (name, predicate, expected, tree) =
  Alcotest.test_case name `Quick (fun () ->
    Alcotest.(check int)
      t_case
      expected
      (f predicate tree)
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
      ("map_to_int", List.map (make_int_return "map_to_int" BT.map) map_test_cases_int);
      ("map_to_string", List.map (make_string_return "map_to_string" BT.map) map_test_cases_string);
      ("find_by", List.map (make_predicate_bool_test "find_by" BT.find_by) find_by_cases);
      ("count_by", List.map (make_predicate_int_test "count_by" BT.count_by) count_by_cases);
    ]

