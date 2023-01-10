module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp x xys
  = fromJust (lookup x xys)

-- 3 marks
vars :: Formula -> [Id]
vars (Var id)
  = [id]
vars (Not f)
  = vars f
vars (And f1 f2)
  = sort (nub (vars f1 ++ vars f2))
vars (Or f1 f2)
  = sort (nub (vars f1 ++ vars f2))

-- 1 mark
idMap :: Formula -> IdMap
idMap f
  = zip (vars f) [1..]

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF f@(Var _)
  = f
toNNF (Not (And f1 f2))
  = Or (toNNF (Not f1)) (toNNF (Not f2))
toNNF (Not (Or f1 f2))
  = And (toNNF (Not f1)) (toNNF (Not f2))
toNNF (Not (Not f))
  = toNNF f
toNNF f@(Not (Var _))
  = f
toNNF (And f1 f2)
  = And (toNNF f1) (toNNF f2)
toNNF (Or f1 f2)
  = Or (toNNF f1) (toNNF f2)

-- 3 marks
toCNF :: Formula -> CNF
toCNF
  = toCNF' . toNNF
  where
    toCNF' :: Formula -> CNF
    toCNF' f@(Var _)
      = f
    toCNF' (Not f)
      = Not (toCNF' f)
    toCNF' (And f1 f2)
      = And (toCNF' f1) (toCNF' f2)
    toCNF' (Or f1 f2)
      = distribute (toCNF' f1) (toCNF' f2)

-- 4 marks
-- (!b & (!a | c))
-- (!b & (!a | c))
-- data Formula = Var Id
--              | Not Formula
--              | And Formula Formula
--              | Or  Formula Formula
-- type CNFRep = [[Int]]
-- [[1,2,3], [-1,4,5]]
flatten :: CNF -> CNFRep
flatten f
  = flatten' f (idMap f)
  where
    flatten' :: CNF -> IdMap -> CNFRep
    flatten' (Var id) idm
      = [[lookUp id idm]]
    flatten' (Not (Var id)) idm
      = [[-(lookUp id idm)]]
    flatten' (And f1 f2) idm
      = (flatten' f1 idm) ++ (flatten' f2 idm)
    flatten' (Or f1 f2) idm
      = [concat ((flatten' f1 idm) ++ (flatten' f2 idm))]

--------------------------------------------------------------------------
-- Part III

-- 5 marks
isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

propUnits :: CNFRep -> (CNFRep, [Int])
propUnits cnfrep
  | null units = (cnfrep, [])
  | otherwise = (cnfrep'', unit ++ us)
    where
      units = filter isSingleton cnfrep
      unit = head units
      cnfrep' = mapMaybe (prop unit) cnfrep
      (cnfrep'', us) = propUnits cnfrep'
      prop :: [Int] -> [Int] -> Maybe [Int]
      prop unit cnfrep
        | unit == cnfrep = Nothing
        | (head unit) `elem` cnfrep = Nothing
        | otherwise = Just (filter (/=(-(head unit))) cnfrep)

-- 4 marks
dp :: CNFRep -> [[Int]]
dp cnfrep
  | null cnfrep' = [units]
  | any null cnfrep' = []
  | otherwise = (map (++ units) (dp cnfrep1)) ++ (map (++ units) (dp cnfrep2))
    where
      (cnfrep', units) = propUnits cnfrep
      (xxs@(x : xs) : xxs') = cnfrep'
      cnfrep1 = [x] : xxs : xxs'
      cnfrep2 = [-x] : xxs : xxs'

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat
  = undefined

