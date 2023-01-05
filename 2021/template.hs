module Alloc where

import Data.Maybe
import Data.List

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count x ys
  = sum [1 | y <- ys, x == y]

degrees :: Eq a => Graph a -> [(a, Int)]
degrees ([], _)
  = []
degrees ((n : ns), es)
  = (n, (x + y)) : degrees (ns, es)
    where
      x = count n (map fst es) 
      y = count n (map snd es)
      
-- return the other item in the pair
-- other (1, 2) 1 -> 2
-- other (1, 2) 2 -> 1
other :: Eq a => (a, a) -> a -> a
other (x, y) z
  | x == z = y
  | otherwise = x

neighbours :: Eq a => a -> Graph a -> [a]
neighbours n (_, es)
  = [other e n | e@(n1, n2) <- es, n == n1 || n == n2]

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode n (ns, es)
  = (ns', es')
  where
    ns' = filter (/= n) ns
    es' = filter ((/= n) . snd) (filter ((/= n) . fst) es)
------------------------------------------------------
--
-- Part II
--
-- type Edge a = (a, a)
-- type Graph a = ([a], [Edge a])
-- type Colour = Int 
-- type Colouring a = [(a, Colour)]
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _ ([], _)
  = []
colourGraph x g
  = c : cs
    where
      (_, n) = minimum (map swap (degrees g))
      cs = colourGraph x (removeNode n g)
      is = [1..x] \\ map (`lookUp` cs) (neighbours n g)
      c = case is of
        [] -> (n, 0)
        (i : _) -> (n, i)

------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap 
  = undefined

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments 
  = undefined

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp 
  = undefined

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock 
  = undefined

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG 
  = undefined

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars 
  = undefined

buildCFG :: Function -> CFG
buildCFG 
  = undefined