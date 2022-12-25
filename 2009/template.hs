import Data.List
import Data.Maybe

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode = (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x ys
  = fromJust (lookup x ys)

checkSat :: BDD -> Env -> Bool
checkSat (0, _) env
  = False
checkSat (1, _) env
  = True
checkSat (nodeId, nodes) env
  | boolValue = checkSat (trueNodeId, nodes) env
  | otherwise = checkSat (falseNodeId, nodes) env
    where
      (index, falseNodeId, trueNodeId) = lookUp nodeId nodes
      boolValue = lookUp index env

sat :: BDD -> [[(Index, Bool)]]
sat bdd = sat' bdd []
  where
    sat' :: BDD -> Env -> [[(Index, Bool)]]
    sat' (0, _) _ 
      = []
    sat' (1, _) env 
      = [env] 
    sat' (nodeId, nodes) env
      =  sat' (falseNodeId, nodes) ((index, False) : env)
      ++ sat' (trueNodeId , nodes) ((index, True)  : env)
        where
          (index, falseNodeId, trueNodeId) = lookUp nodeId nodes

------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim False))
  = Prim True
simplify (Not (Prim True))
  = Prim False
simplify (And (Prim False) (Prim False))
  = Prim False
simplify (And (Prim False) (Prim True))
  = Prim False
simplify (And (Prim True) (Prim False))
  = Prim False
simplify (And (Prim True) (Prim True))
  = Prim True
simplify (Or (Prim False) (Prim False))
  = Prim False
simplify (Or (Prim False) (Prim True))
  = Prim True
simplify (Or (Prim True) (Prim False))
  = Prim True
simplify (Or (Prim True) (Prim True))
  = Prim True
simplify exp
  = exp

-- data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp

restrict :: BExp -> Index -> Bool -> BExp
restrict (Prim x) _ _
  = Prim x
restrict (IdRef x) y z
  | x == y = Prim z
  | otherwise = IdRef x
restrict (Not exp) y z
  = simplify (Not (restrict exp y z))
restrict (And exp1 exp2) y z
  = simplify (And (restrict exp1 y z) (restrict exp2 y z))
restrict (Or exp1 exp2) y z
  = simplify (Or (restrict exp1 y z) (restrict exp2 y z))

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
-- type BDDNode = (NodeId, (Index, NodeId, NodeId))
-- type BDD = (NodeId, [BDDNode])
buildBDD :: BExp -> [Index] -> BDD
buildBDD e xs
  = buildBDD' e 2 xs
  where
    buildBDD' :: BExp -> NodeId -> [Index] -> BDD
    buildBDD' (Prim x) _ []
      | x = (1, [])
      | otherwise = (0, [])
    buildBDD' e i (x : xs)
      = (i, nodes1 ++ nodes2 ++ [node])
      where
        node = (i, (x, l, r))
        e1 = restrict e x False
        e2 = restrict e x True
        (l, nodes1) = buildBDD' e1 (2 * i) xs
        (r, nodes2) = buildBDD' e2 (2 * i + 1) xs

------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD 
  = undefined

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])

