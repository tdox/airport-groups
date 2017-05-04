{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables #-}


import Data.Monoid ((<>))
import Data.Map ((!), Map, empty)
import qualified Data.Map as M
import Data.Set (difference, fromList, intersection, union)
import qualified Data.Set as S

import Data.Text (Text, pack, unpack)

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

type Var = Text
-- type BoolVar = Text

data Pred a = Pred (a -> Bool)

instance Show (Pred a) where
  show (Pred _) = "pred"

type  Set a = S.Set a


-- type SetVars a = Map Var (Set a)

data SetExpr a = Elems (Set a)
               | SVar Var
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

-- type PredVar = Text
-- type PredVars a = Map Var (Pred a)

data PredExpr a = PLit (Pred a)
                | PVar Var
                | PAnd (PredExpr a) (PredExpr a)
                | POr (PredExpr a) (PredExpr a)
                | PNot (PredExpr a)
                deriving Show

type Store a = Map Var (Val a)

data Expr a = SetExpr a
            | PredExpr a
            deriving Show

data Val a = SetVal (Set a)
           | PredVal (Pred a)
           -- deriving (Show)

instance Show a => (Show (Val a)) where
  show (SetVal s) = show (S.toList s)
  show (PredVal p) = show p

{-           
data Store a = Store { setVars  :: SetVars a
                     , predVars :: PredVars a
                     } deriving Show
-}

data Stmt a = AssignSet Var (SetExpr a)
--          | AssignBool BoolVar BoolExpr
            | AssignPred Var (PredExpr a)
            | Print Var
           -- | PrintPred PredVar
            | Seq [Stmt a]
            deriving Show

type Output = Text

type Program = Stmt


{-
data Stmt where
  AssignSet :: SetVar -> SetExpr a -> Stmt
  AssignPred :: PredVar -> PredExpr a -> Stmt
  Seq :: [Stmt] -> Stmt

-}  

type Err = Text

evalSetExpr :: Ord a => Store a -> SetExpr a -> Either Err (Set a)
evalSetExpr _ (Elems xs) = Right xs

evalSetExpr st (SVar v) = case M.lookup v st of
  Nothing -> Left $ v <> " not found"
  Just (SetVal s) -> Right s
  Just (PredVal _) -> Left $ v <> " is a set variable"

evalSetExpr st (Union xs ys) =
  union <$> (evalSetExpr st xs) <*> (evalSetExpr st ys)

evalSetExpr st (Intersection xs ys) =
  intersection <$> (evalSetExpr st xs) <*> (evalSetExpr st ys)

evalSetExpr st (Difference xs ys) =
  difference <$> (evalSetExpr st xs) <*> (evalSetExpr st ys)

{-
evalSetExpr (SuchThat xs (Pred p)) =
  S.filter (\x -> p x) (evalSetExpr xs)
-}

evalSetExpr st (SuchThat xs pe) =
  case evalSetExpr st xs of
    Left err -> Left err
    Right s -> Right $ S.filter (\x -> (evalPred (evalPredExpr pe)) x) s
                       

-----

{-
evalBoolExpr :: BoolExpr -> Bool
evalBoolExpr (BoolLit b) = b
evalBoolExpr (And x y) = evalBoolExpr x && evalBoolExpr y
evalBoolExpr (Or x y) = evalBoolExpr x || evalBoolExpr y
evalBoolExp (Not x) = not $ evalBoolExpr x
-}


evalIsMemberOf :: Ord a => Store a -> a -> SetExpr a -> Either Err Bool

evalIsMemberOf _ e (Elems xs) = Right $ S.member e xs

evalIsMemberOf st e (Union xs ys) =
  (||) <$> (S.member e <$> (evalSetExpr st xs))
       <*> (S.member e <$> (evalSetExpr st ys))

evalIsMemberOf st e (Intersection xs ys) =
  (&&) <$> (S.member e <$> (evalSetExpr st xs))
       <*> (S.member e <$> (evalSetExpr st ys))



evalIsMemberOf st e (Difference xs ys) =
  (&&) <$> (S.member e <$> (evalSetExpr st xs))
       <*> (not <$> (S.member e <$> (evalSetExpr st ys)))

  
--  S.member e (evalSetExpr st xs) && not (S.member e (evalSetExpr st ys))

-- evalIsMemberOf e s@(SuchThat _ _) = S.member e $ evalSetExpr s
-- uses following default

evalIsMemberOf st e s = S.member e <$> evalSetExpr st s

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


exec :: forall a. (Show a, Ord a) =>
        Stmt a -> (Output, Store a) -> Either Err (Output, Store a)

exec (AssignSet var setExpr) (out, st) = eOutStore

  where
    eSet :: Either Err (Set a)
    eSet = evalSetExpr st setExpr

    eOutStore :: Either Err (Output, Store a)
    eOutStore = case eSet of
      Left err -> Left err
      Right set -> Right (out, M.insert var (SetVal set) st)


exec (AssignPred var predExpr) (out, st) =
  Right $ (out, M.insert var (PredVal (evalPredExpr predExpr)) st)

exec (Print var) (out0, st) = rslt
  where
    mVal = M.lookup var st

    rslt = case mVal of
      Nothing -> Left (var <> " undefined")
      Just val -> Right $ (var <> " = " <> pack (show val) <> out0, st)

exec (Seq []) outSt = Right outSt

exec (Seq (s:ss)) os0@(out0, st0) = x
  where
    eOutStore = exec s os0

    x = case eOutStore of
      Left err -> Left err
      Right os1 -> exec (Seq ss) os1




{-
pe101 :: PredExpr
pe101 = undefined

p101 :: Pred
p101 = evalPredExpr pe101

b101 :: Bool
b101 = (evalPred p101) 5
-}

--------------------------------------------------------------------------------

set1 = fromList [1, 2, 3] :: Set Int
set2 = fromList [2, 3, 4] :: Set Int

st = M.fromList [("s1", SetVal set1), ("s2", SetVal set2)] :: Store Int

s1 = Elems set1
ok1 = evalSetExpr st s1 == Right (fromList [1, 2, 3])

s2 = Elems $ fromList [2, 3, 4]
ok2 = evalSetExpr st s2 == Right (fromList [2, 3, 4])

s3 = Union s1 s2
ok3 = evalSetExpr st s3 == Right (fromList [1, 2, 3, 4])

p1 = Pred (>= 3)

s4 = SuchThat s2 (PLit p1)
ok4 = evalSetExpr st s4 == Right (fromList [3, 4])

Right b1 = evalIsMemberOf st 2 s1
ok5 = b1

Right b2 = evalIsMemberOf st 9 s1
ok6 = not b2

Right b3 = evalIsMemberOf st 1 (Union s1 s2)
ok7 = b3

Right b4 = evalIsMemberOf st 9 (Union s1 s2)
ok8 = not b4

Right b5 = evalIsMemberOf st 2 (Intersection s1 s2)
ok9 = b5

Right b6 = evalIsMemberOf st 1 (Intersection s1 s2)
ok10 = not b6

Right b7 = evalIsMemberOf st 1 (Difference s1 s2)
ok11 = b7

Right b8 = evalIsMemberOf st 2 (Difference s1 s2)
ok12 = not b8

Right b9 = evalIsMemberOf st 10 (Difference s1 s2)
ok13 = not b9

Right b10 = evalIsMemberOf st 1 $ SuchThat s1 $ PLit $ Pred (> 2)
ok14 = not b10

Right b11 = evalIsMemberOf st 3 $ SuchThat s1 $ PLit $ Pred (> 2)
ok15 = b11

Right b12 = evalIsMemberOf st 9 $ SuchThat s1 $ PLit $ Pred (> 2)
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

s5 = evalSetExpr st (SVar "s1") :: Either Err (Set Int)
ok27 = s5 == evalSetExpr st s1

st0 :: Store Int
st0 = empty

out0 :: Output
out0 = ""

  
stmt1 = AssignSet "s3" s3
Right os1 = exec stmt1 (out0, st0) :: Either Err (Output, Store Int)

stmt2 = Print "s3"
Right os2 = exec stmt2 os1

stmt3 = Print "s999"
Left os3 = exec stmt3 os2


oks = [ok1, ok2, ok3, ok4, ok5, ok6, ok7, ok8, ok9, ok10, ok11, ok12, ok13
      , ok14, ok15, ok16, ok17, ok18, ok19, ok20, ok21, ok22, ok23, ok24, ok25
      , ok26, ok27]
      
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
