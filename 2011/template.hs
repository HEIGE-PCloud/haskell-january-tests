import Data.Maybe

data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Show)

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr 
          deriving (Eq, Show)

showT :: Type -> String
showT TInt  
  = "Int"
showT TBool 
  = "Bool"
showT (TFun t t') 
  = "(" ++ showT t ++ " -> " ++ showT t' ++ ")"
showT (TVar a) 
  = a
showT TErr  
  = "Type error"

type TypeTable = [(String, Type)]

type TEnv 
  = TypeTable    -- i.e. [(String, Type)]

type Sub 
  = TypeTable    -- i.e. [(String, Type)]  

-- Built-in function types...
primTypes :: TypeTable
primTypes 
  = [("+", TFun TInt (TFun TInt TInt)),
     (">", TFun TInt (TFun TInt TBool)),
     ("==", TFun TInt (TFun TInt TBool)),
     ("not", TFun TBool TBool)]

------------------------------------------------------
-- PART I

-- Pre: The search item is in the table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x xys
  = fromJust (lookup x xys)

tryToLookUp :: Eq a => a -> b -> [(a, b)] -> b
tryToLookUp x z xys
  = case lookup x xys of
      Nothing -> z
      Just y -> y

-- Pre: The given value is in the table
reverseLookUp :: Eq b => b -> [(a, b)] -> [a]
reverseLookUp y []
  = []
reverseLookUp y ((x', y') : xys)
  | y == y' = x' : reverseLookUp y xys
  | otherwise    = reverseLookUp y xys


occurs :: String -> Type -> Bool
occurs _ TInt 
  = False
occurs _ TBool
  = False
occurs x (TFun t t')
  = (occurs x t) || (occurs x t')
occurs x (TVar y)
  = x == y
occurs _ TErr
  = False

------------------------------------------------------
-- PART II

-- Pre: There are no user-defined functions (constructor Fun)
-- Pre: All variables in the expression have a binding in the given 
--      type environment
inferType :: Expr -> TEnv -> Type
inferType (Number _) _
  = TInt
inferType (Boolean _) _
  = TBool
inferType (Id x) e
  = lookUp x e
inferType (Prim x) e
  = lookUp x primTypes
inferType (Cond exp0 exp1 exp2) env
  | t0 == TBool && t1 == t2 = t1
  | otherwise = TErr
    where
      t0 = inferType exp0 env
      t1 = inferType exp1 env
      t2 = inferType exp2 env
inferType (App exp0 exp1) env
  | t0 == TErr || t1 == TErr = TErr
  | otherwise = case t0 of
    (TFun t t') -> if t == t1 then t' else TErr
    otherwise -> TErr
  where
    t0 = inferType exp0 env
    t1 = inferType exp1 env
------------------------------------------------------
-- PART III

applySub :: Sub -> Type -> Type
applySub _ TInt
  = TInt
applySub _ TBool
  = TBool
applySub s (TFun t t')
  = TFun (applySub s t) (applySub s t')
applySub s (TVar v)
  = tryToLookUp v (TVar v) s
applySub _ TErr
  = TErr

unify :: Type -> Type -> Maybe Sub
unify t t'
  = unifyPairs [(t, t')] []

pmap :: (a -> b) -> [(a, a)] -> [(b, b)]
pmap f [] = []
pmap f ((x, x') : xs) = (f x, f x') : pmap f xs

unifyPairs :: [(Type, Type)] -> Sub -> Maybe Sub
unifyPairs [] s
  = Just s
unifyPairs ((TInt, TInt) : ts) s
  = unifyPairs ts s
unifyPairs ((TBool, TBool) : ts) s
  = unifyPairs ts s
unifyPairs ((TVar v, t@(TVar v')) : ts) s
  | v == v' = unifyPairs ts s
unifyPairs ((TVar v, t) : ts) s
  | occurs v t = Nothing
  | otherwise = unifyPairs (pmap (applySub [b]) ts) s'
  where
    b = (v, t)
    s' = b : s
unifyPairs ((t, TVar v) : ts) s
  = unifyPairs ((TVar v, t) : ts) s
unifyPairs ((TFun t1 t2, TFun t1' t2') : ts) s
  = unifyPairs ((t1, t1') : (t2, t2') : ts) s
unifyPairs _ _
  = Nothing

------------------------------------------------------
-- PART IV

updateTEnv :: TEnv -> Sub -> TEnv
updateTEnv tenv tsub
  = map modify tenv
  where
    modify (v, t) = (v, applySub tsub t)

combine :: Sub -> Sub -> Sub
combine sNew sOld
  = sNew ++ updateTEnv sOld sNew

-- In combineSubs [s1, s2,..., sn], s1 should be the *most recent* substitution
-- and will be applied *last*
combineSubs :: [Sub] -> Sub
combineSubs 
  = foldr1 combine

names :: [String]
names = ['a' : (show x) | x <- [1..]]

inferPolyType :: Expr -> Type
inferPolyType exp
  = t
  where
    (_, t, _) = inferPolyType' exp [] names

-- You may optionally wish to use one of the following helper function declarations
-- as suggested in the specification. 

inferPolyType' :: Expr -> TEnv -> [String] -> (Sub, Type, [String])
inferPolyType' (Number _) e ns
  = ([], TInt, ns)

inferPolyType' (Boolean _) e ns
  = ([], TBool, ns)

inferPolyType' (Id x) e nns@(n : ns)
  = case lookup x e of
    Just t -> ([], t, nns)
    Nothing -> ([(x ,TVar n)], TVar n, ns)

inferPolyType' (Prim x) e ns
  = ([], lookUp x primTypes, ns)

inferPolyType' (Fun x e) env nns@(n : ns)
  | te == TErr = ([], TErr, nns)
  | otherwise = (sub, TFun tx te, ns') 
    where
      env' = (x, TVar n) : env
      (sub, te, ns') = inferPolyType' e env' ns
      tx = applySub sub (TVar n)

inferPolyType' (App f e) env ns
  = case sub of 
      Nothing -> ([], TErr, ns)
      Just s3 -> (combineSubs [s3, s2, s1], applySub s3 v, ns'')
    where
      (s1, tf, ns') = inferPolyType' f env ns
      env2 = updateTEnv env s1
      (s2, te, (n : ns'')) = inferPolyType' e env2 ns'
      v = TVar n
      sub = unify tf (TFun te v)

inferPolyType' (Cond e1 e2 e3) env ns
  = case sub1 of
      Nothing -> ([], TErr, ns)
      Just s4 -> case sub2 of
        Nothing -> ([], TErr, ns)
        Just s5 -> (combineSubs [s5, s4, s3, s2, s1], applySub s5 t2', ns3)
    where
      (s1, t1, ns1) = inferPolyType' e1 env ns
      env2 = s1 ++ (updateTEnv env s1)
      (s2, t2, ns2) = inferPolyType' e2 env2 ns1
      env3 = s2 ++ (updateTEnv env2 s2)
      (s3, t3, ns3) = inferPolyType' e3 env3 ns2
      t1' = applySub (combineSubs [s3, s2, s1]) t1
      t2' = applySub (combineSubs [s3, s2]) t2
      t3' = applySub s3 t3
      sub1 = unify t1' TBool
      sub2 = unify t2' t3'

------------------------------------------------------
-- Monomorphic type inference test cases from Table 1...

env :: TEnv
env = [("x",TInt),("y",TInt),("b",TBool),("c",TBool)]

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 :: Expr
type1, type2, type3, type4, type5, type6, type7, type8 :: Type

ex1 = Number 9
type1 = TInt

ex2 = Boolean False
type2 = TBool

ex3 = Prim "not"
type3 =  TFun TBool TBool

ex4 = App (Prim "not") (Boolean True)
type4 = TBool

ex5 = App (Prim ">") (Number 0)
type5 = TFun TInt TBool

ex6 = App (App (Prim "+") (Boolean True)) (Number 5)
type6 = TErr

ex7 = Cond (Boolean True) (Boolean False) (Id "c")
type7 = TBool

ex8 = Cond (App (Prim "==") (Number 4)) (Id "b") (Id "c")
type8 = TErr

------------------------------------------------------
-- Unification test cases from Table 2...

u1a, u1b, u2a, u2b, u3a, u3b, u4a, u4b, u5a, u5b, u6a, u6b :: Type
sub1, sub2, sub3, sub4, sub5, sub6 :: Maybe Sub

u1a = TFun (TVar "a") TInt
u1b = TVar "b"
sub1 = Just [("b",TFun (TVar "a") TInt)]

u2a = TFun TBool TBool
u2b = TFun TBool TBool
sub2 = Just []

u3a = TFun (TVar "a") TInt
u3b = TFun TBool TInt
sub3 = Just [("a",TBool)]

u4a = TBool
u4b = TFun TInt TBool
sub4 = Nothing

u5a = TFun (TVar "a") TInt
u5b = TFun TBool (TVar "b")
sub5 = Just [("b",TInt),("a",TBool)]

u6a = TFun (TVar "a") (TVar "a")
u6b = TVar "a"
sub6 = Nothing

------------------------------------------------------
-- Polymorphic type inference test cases from Table 3...

ex9, ex10, ex11, ex12, ex13, ex14 :: Expr
type9, type10, type11, type12, type13, type14 :: Type

ex9 = Fun "x" (Boolean True)
type9 = TFun (TVar "a1") TBool

ex10 = Fun "x" (Id "x")
type10 = TFun (TVar "a1") (TVar "a1")

ex11 = Fun "x" (App (Prim "not") (Id "x"))
type11 = TFun TBool TBool

ex12 = Fun "x" (Fun "y" (App (Id "y") (Id "x")))
type12 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TVar "a3")) (TVar "a3"))

ex13 = Fun "x" (Fun "y" (App (App (Id "y") (Id "x")) (Number 7)))
type13 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TFun TInt (TVar "a4"))) 
              (TVar "a4"))

ex14 = Fun "x" (Fun "y" (App (Id "x") (Prim "+"))) 
type14 = TFun (TFun (TFun TInt (TFun TInt TInt)) (TVar "a3")) 
              (TFun (TVar "a2") (TVar "a3"))

ex15 = Cond (Id "x") (App (Prim "not") (Id "x")) (Id "x")
type15 = TBool

ex16 = Cond (Id "x") (Id "x") (App (Prim "not") (Id "x"))
type16 = TBool

ex17 = Fun "x" (Cond (Id "x") (App (Prim "not") (Id "x")) (Id "x"))
type17 = TFun TBool TBool

ex18 = Cond (Id "x") (App (App (Prim "+") (Id "x")) (Number 5)) 
      (App (App (Prim "+") (Id "x")) (Number 3))
type18 = TErr

ex19 = Cond (Id "x") (Id "x") (Id "x")
type19 = TVar "a1"

ex20 = Cond (Boolean True) (App (App (Prim "+") (Id "x")) (Number 5)) (Id "x")
type20 = TInt

ex21 = Cond (Boolean True) (Id "x") (App (App (Prim "+") (Id "x")) (Number 5))
type21 = TInt

ex22 = Cond (Id "x") (App (Prim "not") (Id "x")) (App (App (Prim "+") (Id "x"))
       (Number 5))
type22 = TErr

ex23 = Cond (Id "y") (Id "x") (App (App (Prim "+") (Id "x")) (Number 5))
type23 = TInt

ex24 = Cond (App (Fun "x" (App (Prim "not") (Id "x"))) (Id "y")) (Id "y") (Id "y") 
type24 = TBool
