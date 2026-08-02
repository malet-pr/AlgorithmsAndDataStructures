open DataStructures

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

let expected_double =
  node 20
    (leaf 10)
    (node 40 TM.Empty (leaf 30))

let expected_string =
  node "10"
    (leaf "5")
    (node "20" TM.Empty (leaf "15"))


    