module SC where

import Data.List
import Data.Maybe

import Types
import Examples

---------------------------------------------------------

prims :: [Id]
prims
  = ["+", "-", "*", "<=", "ite"]

lookUp :: Id -> [(Id, a)] -> a
lookUp v env
  = fromMaybe (error ("lookUp failed with search key " ++ v))
              (lookup v env)

---------------------------------------------------------
-- Part I
-- data Exp = Const Int | 
--            Var Id | 
--            Fun [Id] Exp |
--            App Exp [Exp] |
--            Let [Binding] Exp 
-- type Binding = (Id, Exp)
isFun :: Exp -> Bool
isFun (Fun _ _)
  = True
isFun _
  = False

splitDefs :: [Binding] -> ([Binding], [Binding])
splitDefs []
  = ([], [])
splitDefs (b@(_, e) : bs)
  | isFun e = (b : fs, vs)
  | otherwise = (fs, b : vs)
  where
    (fs, vs) = splitDefs bs

topLevelFunctions :: Exp -> Int
topLevelFunctions (Let bs _)
  = countFunctions bs
  where
    countFunctions :: [Binding] -> Int
    countFunctions []
      = 0
    countFunctions ((_, e) : bs)
      | isFun e = 1 + countFunctions bs
      | otherwise = countFunctions bs
topLevelFunctions _
  = 0

---------------------------------------------------------
-- Part II

unionAll :: Eq a => [[a]] -> [a]
unionAll
  = undefined

freeVars :: Exp -> [Id]
freeVars 
  = undefined

---------------------------------------------------------
-- Part III

-- Given...
lambdaLift :: Exp -> Exp
lambdaLift e
  = lift (modifyFunctions (buildFVMap e) e)

buildFVMap :: Exp -> [(Id, [Id])]
buildFVMap 
  = undefined

modifyFunctions :: [(Id, [Id])] -> Exp -> Exp
-- Pre: The mapping table contains a binding for every function
-- named in the expression.
modifyFunctions 
  = undefined

-- The default definition here is id.
-- If you implement the above two functions but not this one
-- then lambdaLift above will remove all the free variables
-- in functions; it just won't do any lifting.
lift :: Exp -> Exp
lift 
  = id

-- You may wish to use this...
lift' :: Exp -> (Exp, [Supercombinator])
lift' 
  = undefined


