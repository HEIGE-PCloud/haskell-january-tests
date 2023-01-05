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
  | x == z    = y
  | otherwise = x

neighbours :: Eq a => a -> Graph a -> [a]
neighbours n (_, es)
  = [other e n | e@(n1, n2) <- es, n == n1 || n == n2]

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode n (ns, es)
  = (ns', es')
  where
    ns' = filter (/= n) ns
    es' = filter (\(e1, e2) -> e1 /= n && e2 /= n) es
------------------------------------------------------
--
-- Part II
--
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
buildIdMap cs
  = ("return", "return") : (map build cs)
  where
    build :: (Id, Colour) -> (Id, Id)
    build (i, c)
      | c == 0    = (i, i)
      | otherwise = (i, 'R' : (show c))

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments ids idm
  = foldr build [] (zip ids (map (`lookUp` idm) ids))
    where
      build :: (Id, Id) -> [Statement] -> [Statement]
      build (i, i') ss
        | i == i'   = ss 
        | otherwise = (Assign i' (Var i)) : ss

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp exp@(Const _) _
  = exp
renameExp (Var id) idm
  = Var (lookUp id idm)
renameExp (Apply op e1 e2) idm
  = Apply op (renameExp e1 idm) (renameExp e2 idm)

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock bs idm
  = filter assign (map (`renameStatement` idm) bs)
  where
    assign :: Statement -> Bool
    assign (Assign id (Var id'))
      = id /= id'
    assign _
      = True

renameStatement :: Statement -> IdMap -> Statement
-- Pre: A precondition is that every variable referenced in 
-- the statement is in the idMap. 
renameStatement (Assign id exp) idm
  = Assign (lookUp id idm) (renameExp exp idm)
renameStatement (If exp b1 b2) idm
  = If (renameExp exp idm) (renameBlock b1 idm) (renameBlock b2 idm)
renameStatement (While exp b) idm
  = While (renameExp exp idm) (renameBlock b idm)

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG lvs
  = (ids, es)
  where
    ids = nub $ concat lvs
    es = nub $ concatMap build lvs
    build :: [Id] -> [Edge Id]
    build ids
      = [(i1, i2) | i1 <- ids, i2 <- ids, i1 < i2]

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