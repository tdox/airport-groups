{-# LANGUAGE GADTs #-}

import Data.Map (Map)
import Data.Set (difference, fromList, intersection, union)
import qualified Data.Set as S

import Data.Text

-- import Data.List (nub)
{-


s1 = {"a", "b", "c"}
s2 = {"c", "d, "e"}
s3 = {}

s3 = union s1 s2                    -- s1 U s3 = {"a", "b", "c", "d", "e"}
s4 = intersection s1 s2             -- {"c"}

s5 = union s1 {"b", "c", "x"}       -- {"a", "b", "c", "x"}
s6 = difference s1 s2 = s1 - s2     -- {"a", "b"}
s7 = s1 suchThat pred(s1)

b1 = element s1 "a"  -- true



http://stackoverflow.com/questions/16970431/implementing-a-language-interpreter-in-haskell

https://github.com/budabudimir/imp_interpreter

--------------------

Airports

everyAirport



a1 := FAA:SFO
a2 := ICAO:KSFO

as1 := {FAA:SFO, ICAO:KSFO} -- Set Airport
as2 := {FAA:MDW, ICAO:TEB}

as3 := as1 intersect as2
as4 := as1 \/ as2 -- disjunction, ||, or, union
as5 := as1 /\ as2 -- conjuction, &&,  and, intersection

loc1 := (34.345, -72.22) -- (lat, lon) in decimal degrees

dist1 := distance loc1 loc2 -- distance
region1 := circle loc1 radius : 

b1 = airportIsInRegion a1 region1


isInCA := isInState(CA)
pred2 := isInCountry(USA)
pred3 := isNear(FAA:SFO, 100)

isInOR := isInState(OR)

usAps := everyAirport suchThat (isInCountry "US")
      := suchThat(isInCountry("USA"), everyAirport))
      := everyAirport | isInCountry("USA")

isInCAorOR := isInState(CA) predOr isInState(OR)
           := isInState(CA) || isInState(OR)

           isNorthOfAirport(FAA:SFO)
           isSouthOfAirport, isEastOfAirport, isWestOfAirport

isNorthOfLatitude(34.5)
isSouthOfLatitude(34)
isWestOfLongitude(23)
isEastOfLongitude(45)


pred1 := isInState(CA) && isNorthOf(FAA:SFO)

as3 := usAps suchThat isInCAorOr


-}

-- type Elem = Int

type SetVar = Text
-- type BoolVar = Text

data Pred a = Pred (a -> Bool)

instance Show (Pred a) where
  show (Pred _) = "pred"

type  Set a = S.Set a


type SetVars a = Map SetVar (Set a)

data SetExpr a = Elems (Set a)
               | Var SetVar
               | Union (SetExpr a) (SetExpr a)
               | Intersection (SetExpr a) (SetExpr a)
               | Difference (SetExpr a) (SetExpr a)
--             | SuchThat (SetExpr a) Pred
               | SuchThat (SetExpr a) (PredExpr a)
               deriving (Show)

{-
data BoolExpr = BoolLit Bool
              | And BoolExpr BoolExpr
              | Or  BoolExpr BoolExpr
              | Not BoolExpr
              | IsMemberOf Elem SetExpr
-}

type PredVar = Text
type PredVars a = Map PredVar (Pred a)

data PredExpr a = PLit (Pred a)
                | PVar PredVar
                | PAnd (PredExpr a) (PredExpr a)
                | POr (PredExpr a) (PredExpr a)
                | PNot (PredExpr a)
                deriving Show
  


data Stmt a = AssignSet SetVar (SetExpr a)
--          | AssignBool BoolVar BoolExpr
            | AssignPred PredVar (PredExpr a)
            | PrintSet SetVar
            | PrintPred PredVar
            | Seq [Stmt a]


type Program = Stmt


{-
data Stmt where
  AssignSet :: SetVar -> SetExpr a -> Stmt
  AssignPred :: PredVar -> PredExpr a -> Stmt
  Seq :: [Stmt] -> Stmt

-}  


evalSetExpr :: Ord a => SetExpr a -> Set a
evalSetExpr (Elems xs) = xs
--evalSetExpr (Var v) = 
evalSetExpr (Union xs ys) = union (evalSetExpr xs) (evalSetExpr ys)

evalSetExpr (Intersection xs ys) =
  intersection (evalSetExpr xs) (evalSetExpr ys)

evalSetExpr (Difference xs ys) = difference (evalSetExpr xs) (evalSetExpr ys)

{-
evalSetExpr (SuchThat xs (Pred p)) =
  S.filter (\x -> p x) (evalSetExpr xs)
-}

evalSetExpr (SuchThat xs pe) = -- evalSetExpr xs (evalPredExpr pe)
  S.filter (\x -> (evalPred (evalPredExpr pe)) x) (evalSetExpr xs)


-----

{-
evalBoolExpr :: BoolExpr -> Bool
evalBoolExpr (BoolLit b) = b
evalBoolExpr (And x y) = evalBoolExpr x && evalBoolExpr y
evalBoolExpr (Or x y) = evalBoolExpr x || evalBoolExpr y
evalBoolExp (Not x) = not $ evalBoolExpr x
-}

evalIsMemberOf e (Elems xs) = S.member e xs

evalIsMemberOf e (Union xs ys) =
  S.member e (evalSetExpr xs) || S.member e (evalSetExpr ys)

evalIsMemberOf e (Intersection xs ys) =
  S.member e (evalSetExpr xs) && S.member e (evalSetExpr ys)

evalIsMemberOf e (Difference xs ys) =
  S.member e (evalSetExpr xs) && not (S.member e (evalSetExpr ys))

-- evalIsMemberOf e s@(SuchThat _ _) = S.member e $ evalSetExpr s
-- uses following default

evalIsMemberOf e s = S.member e $ evalSetExpr s

evalPred :: (Pred a) -> a -> Bool
evalPred (Pred f) e = f e 

evalPredExpr :: (PredExpr a) -> (Pred a)
evalPredExpr (PLit pred) = pred

evalPredExpr (PAnd  p q) =
  Pred (\e -> ((evalPred (evalPredExpr p) e) && (evalPred (evalPredExpr q) e)))

evalPredExpr (POr p q) =
  Pred (\e -> ((evalPred (evalPredExpr p) e) || (evalPred (evalPredExpr q) e)))

evalPredExpr (PNot p) =
  Pred (\e -> (not (evalPred (evalPredExpr p) e)))


{-
pe101 :: PredExpr
pe101 = undefined

p101 :: Pred
p101 = evalPredExpr pe101

b101 :: Bool
b101 = (evalPred p101) 5
-}

--------------------------------------------------------------------------------

s1 = Elems $ fromList [1, 2, 3]
ok1 = evalSetExpr s1 == fromList [1, 2, 3]

s2 = Elems $ fromList [2, 3, 4]
ok2 = evalSetExpr s2 == fromList [2, 3, 4]

s3 = Union s1 s2
ok3 = evalSetExpr s3 == fromList [1, 2, 3, 4]

p1 = Pred (>= 3)

s4 = SuchThat s2 (PLit p1)
ok4 = evalSetExpr s4 == fromList [3, 4]

b1 = evalIsMemberOf 2 s1
ok5 = b1

b2 = evalIsMemberOf 9 s1
ok6 = not b2

b3 = evalIsMemberOf 1 (Union s1 s2)
ok7 = b3

b4 = evalIsMemberOf 9 (Union s1 s2)
ok8 = not b4

b5 = evalIsMemberOf 2 (Intersection s1 s2)
ok9 = b5

b6 = evalIsMemberOf 1 (Intersection s1 s2)
ok10 =not b6

b7 = evalIsMemberOf 1 (Difference s1 s2)
ok11 = b7

b8 = evalIsMemberOf 2 (Difference s1 s2)
ok12 = not b8

b9 = evalIsMemberOf 10 (Difference s1 s2)
ok13 = not b9

b10 = evalIsMemberOf 1 $ SuchThat s1 $ PLit $ Pred (> 2)
ok14 = not b10

b11 = evalIsMemberOf 3 $ SuchThat s1 $ PLit $ Pred (> 2)
ok15 = b11

b12 = evalIsMemberOf 9 $ SuchThat s1 $ PLit $ Pred (> 2)
ok16 = not b12

b13 = evalPred p1 0
ok17 = not b13

b14 = evalPred p1 5
ok18 = b14

p2 = Pred (< 5)

p3 :: Pred Int
p3 = evalPredExpr (PAnd (PLit p1) (PLit p2))
b15 = evalPred p3 0
ok19 = not b15

b16 = evalPred p3 4
ok20 = b16

b17 = evalPred p3 5
ok21 = not b17

p4 = evalPredExpr (POr (PLit p1) (PLit p2))

b18 = evalPred p4 0
ok22 = b18

b19 = evalPred p4 4
ok23 = b19

b20 = evalPred p4 5
ok24 = b20

p5 = evalPredExpr (PNot (PLit (Pred (>3))))

b21 = evalPred p5 0
ok25 = b21

b22 = evalPred p5 5
ok26 = not b22


oks = [ok1, ok2, ok3, ok4, ok5, ok6, ok7, ok8, ok9, ok10, ok11, ok12, ok13
      , ok14, ok15, ok16, ok17, ok18, ok19, ok20, ok21, ok22, ok23, ok24, ok25
      , ok26]
      
ok = and oks


--------------------------------------------------------------------------------

{-
pe101 :: PredExpr
pe101 = undefined

p101 :: Pred
p101 = evalPredExpr pe101

b101 :: Bool
b101 = (evalPred p101) 5
-}
