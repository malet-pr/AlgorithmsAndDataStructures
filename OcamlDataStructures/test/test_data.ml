open DataStructures
open Test_models

module TM = DataStructures.Trees.Models

let leaf v = TM.BNode (v, TM.Empty, TM.Empty)

let node v left right = TM.BNode (v, left, right)

let wo_tree =
  node {wo_number="AB22R"; wo_class=Event}
    TM.Empty
    (leaf {wo_number="AB321"; wo_class=Network})

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