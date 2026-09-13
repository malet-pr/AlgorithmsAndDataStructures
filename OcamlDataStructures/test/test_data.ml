open DataStructures
open Test_models

module TM = DataStructures.Trees.Models

let leaf v = TM.BNode (v, TM.Empty, TM.Empty)

let node v left right = TM.BNode (v, left, right)

let wo_tree =
  node {wo_number="123"; wo_class=Event}
    TM.Empty
    (leaf {wo_number="224"; wo_class=Network})

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

let tree3 =
  node 10
    (node 5
       (leaf (-7))
       (leaf 9))
    (node 20
       (leaf (-12))
       (leaf 15))

let expected_double =
  node 20
    (leaf 10)
    (node 40 TM.Empty (leaf 30))

let expected_string =
  node "10"
    (leaf "5")
    (node "20" TM.Empty (leaf "15"))

let expected_triple1 =
  node 30
    (leaf 15)
    (node 60 TM.Empty (leaf 45))


let expected_triple2 =
  node 30
    (node 15
       (leaf 21)
       (leaf 27))
    (node 60
       (leaf 36)
       (leaf 45))

let expected_filter_prune1 =
  node 10
    TM.Empty
    (node 20
       (leaf 12)
       (leaf 15))

let expected_filter_prune2 =
  node 10
    (node 5
       TM.Empty
       (leaf 9))
    (node 20
       TM.Empty
       (leaf 15))

let expected_filter_option1 =
  node (Some 10)
    (node None
      (leaf (Some 7))
      (leaf (Some 9)))
    (node (Some 20)
       (leaf (Some 12))
       (leaf (Some 15)))

let expected_filter_option2 =
  node (Some 10)
    (node (Some 5)
       (leaf None)
       (leaf (Some 9)))
    (node (Some 20)
       (leaf None)
       (leaf (Some 15)))       

let mirror_tree1 =
  node 10
    (node 20
       (leaf 15)
       (leaf 12))
    (node 5
       (leaf 9)
       (leaf 7))

let wo_bst1 = 
  node {wo_number="123"; wo_class=Event}
    (leaf {wo_number="100"; wo_class=Network})
    (leaf {wo_number="224"; wo_class=Network})

let bst1 =
  node 10
    (leaf 5)
    (node 20
      (leaf 15)
      TM.Empty)
   