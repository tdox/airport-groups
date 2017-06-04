{-# LANGUAGE DeriveFunctor, GADTs, OverloadedStrings, ScopedTypeVariables #-}

module Groups where

-- base
import Control.Applicative (liftA2)
import Control.Monad (liftM2)
import Data.Monoid ((<>))

-- containers
import Data.Map ((!), Map, empty)
import qualified Data.Map as M
import Data.Set (difference, fromList, intersection, union)
import qualified Data.Set as S

-- text
import Data.Text (Text, pack, unpack)

-- trace
import Debug.Trace

{-


s1 = ["a", "b", "c"]
s2 = ["c", "d, "e"]
s3 = []

s3 = s1 + s2                         -- ["a", "b", "c", "d", "e"]
s4 = s1 ^ s2                         -- ["c"]

s5 = s1 + ["b", "c", "x"]           -- ["a", "b", "c", "x"]
s6 = s1 - s2                        -- ["a", "b"]
s7 = s1 | pred(s1)

b1 = element s1 "a"  -- true



http://stackoverflow.com/questions/16970431/implementing-a-language-interpreter-in-haskell

https://github.com/budabudimir/imp_interpreter

--------------------

Airports

everyAirport



a1 := FAA:SFO;
a2 := ICAO:KSFO;

as1 := {FAA:SFO, ICAO:KSFO}; -- Set Airport
as2 := {FAA:MDW, ICAO:TEB};

as3 := as1 + as2
as4 := as1 + as2 -- union
as5 := as1 ^ as2 -- intersection

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
isBetweenLongitudes(22, 26)


pred1 := isInState(CA) && isNorthOf(FAA:SFO)

as3 := usAps suchThat isInCAorOr


-}


type Var = Text

data Pred a = Pred (a -> Bool)

instance Show (Pred a) where
  show (Pred _) = "pred"

type  Set a = S.Set a


data SetExpr a = Elems (Set a)
               | SVar Var
               | SParens (SetExpr a)
               | Union (SetExpr a) (SetExpr a)
               | Intersection (SetExpr a) (SetExpr a)
               | Difference (SetExpr a) (SetExpr a)
               | SuchThat (SetExpr a) (PredExpr a)
               deriving (Show)

{-
data BoolExpr = BoolLit Bool
              | And BoolExpr BoolExpr
              | Or  BoolExpr BoolExpr
              | Not BoolExpr
              | IsMemberOf Elem SetExpr
-}

data PredExpr a = PLit (Pred a)
                | PVar Var
                | PParens (PredExpr a)
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

instance Show a => (Show (Val a)) where
  show (SetVal s) = swapForBraces $ show (S.toList s)
  show (PredVal p) = show p

data AssignSetStmt a = AssignSetStmt Var (SetExpr a)

data Stmt a = AssignSet Var (SetExpr a)
--          | AssignBool BoolVar BoolExpr
            | AssignPred Var (PredExpr a)
            | Print Var
            | ElemOf a (SetExpr a)
            | SatisfiesPred a (PredExpr a)
            deriving Show

type Output = [Text]
type Program a = [Stmt a]
type Err = Text

evalSetExpr :: forall a. Ord a => Store a -> SetExpr a -> Either Err (Set a)
evalSetExpr _ (Elems xs) = Right xs

evalSetExpr st (SVar v) = case M.lookup v st of
  Nothing -> Left $ v <> " not found"
  Just (SetVal s) -> Right s
  Just (PredVal _) -> Left $ v <> " is a predicate variable"

evalSetExpr st (SParens xs) = evalSetExpr st xs

evalSetExpr st (Union xs ys) =
  union <$> (evalSetExpr st xs) <*> (evalSetExpr st ys)

evalSetExpr st (Intersection xs ys) =
  intersection <$> (evalSetExpr st xs) <*> (evalSetExpr st ys)

evalSetExpr st (Difference xs ys) =
  difference <$> (evalSetExpr st xs) <*> (evalSetExpr st ys)


evalSetExpr st (SuchThat xs pe) =
  case evalSetExpr st xs of
    Left err -> Left err
    Right s -> do
      let ePred :: Either Err (Pred a) = evalPredExpr st pe
      case ePred of
        Left err -> Left err
        Right pred -> Right $ S.filter (\x -> evalPred pred x) s

-----

{-
evalBoolExpr :: BoolExpr -> Bool
evalBoolExpr (BoolLit b) = b
evalBoolExpr (And x y) = evalBoolExpr x && evalBoolExpr y
evalBoolExpr (Or x y) = evalBoolExpr x || evalBoolExpr y
evalBoolExp (Not x) = not $ evalBoolExpr x
-}


evalIsElementOf :: Ord a => Store a -> a -> SetExpr a -> Either Err Bool

evalIsElementOf _ e (Elems xs) = Right $ S.member e xs

evalIsElementOf st e (Union xs ys) =
  (||) <$> (S.member e <$> (evalSetExpr st xs))
       <*> (S.member e <$> (evalSetExpr st ys))

evalIsElementOf st e (Intersection xs ys) =
  (&&) <$> (S.member e <$> (evalSetExpr st xs))
       <*> (S.member e <$> (evalSetExpr st ys))

evalIsElementOf st e (Difference xs ys) =
  (&&) <$> (S.member e <$> (evalSetExpr st xs))
       <*> (not <$> (S.member e <$> (evalSetExpr st ys)))

evalIsElementOf st e s = S.member e <$> evalSetExpr st s


evalPred :: (Pred a) -> a -> Bool
evalPred (Pred f) e = f e


{-

evalPredExprOld :: Store a -> (PredExpr a) -> (Pred a)

evalPredExprOld st (PLit pred) = pred


evalPredExprOld st (PVar v) = case M.lookup v st of
  Nothing -> error "evalPredExprOld"
  Just (SetVal _) -> error "evalPredExprOld2"
  Just (PredVal p) -> p

evalPredExprOld st (PAnd  p q) =
   Pred (\x -> (&&)  (evalPred (evalPredExprOld st p) x)
                     (evalPred (evalPredExprOld st q) x))

evalPredExprOld st (POr p q) =
  Pred (\e -> ((evalPred (evalPredExprOld st p) e)
               || (evalPred (evalPredExprOld st q) e)))

evalPredExprOld st (PNot p) =
  Pred (\e -> (not (evalPred (evalPredExprOld st p) e)))
-}



evalPredExpr :: Store a -> (PredExpr a) -> Either Err (Pred a)

evalPredExpr st (PLit pred) = Right pred

evalPredExpr st (PVar v) = case M.lookup v st of
  Nothing -> Left $ v <> " not found"
  Just (SetVal _) -> Left $ v <> " is a set variable"
  Just (PredVal p) -> Right p

evalPredExpr st (PParens p) = evalPredExpr st p

evalPredExpr st (PAnd  p q) = do
  predP :: Pred a <- evalPredExpr st p
  predQ :: Pred a <- evalPredExpr st q
  let r :: a -> Bool = \x -> evalPred predP x && evalPred predQ x
  return $ Pred r

evalPredExpr st (POr  p q) = do
  predP <- evalPredExpr st p
  predQ <- evalPredExpr st q
  return $ Pred (\x -> evalPred predP x || evalPred predQ x)

evalPredExpr st (PNot p) = do
  predP <- evalPredExpr st p
  return $ Pred (\x -> not (evalPred predP x))



-- evalSatisfiesPred :: Store a -> START HERE
   

execStmt :: forall a. (Show a, Ord a) =>
        (Output, Store a) -> Stmt a -> Either Err (Output, Store a)

execStmt (out, st) (AssignSet var setExpr) = {- traceShowId -} eOutStore

  where
    eSet :: Either Err (Set a)
    eSet = evalSetExpr st setExpr

    eOutStore :: Either Err (Output, Store a)
    eOutStore = case eSet of
      Left err -> Left err
      Right set -> Right (out, M.insert var (SetVal set) st)

execStmt  (out, st) (AssignPred var predExpr) = do
  case evalPredExpr st predExpr of
    Left err -> Left err
    Right predExp -> Right (out, M.insert var (PredVal predExp) st)

execStmt  (out0, st) (Print var) = rslt
  where
    mVal = M.lookup var st

    rslt = case mVal of
      Nothing -> Left (var <> ": undefined variable")
      Just val -> Right $ ((var <> " = " <> pack (show val)) : out0, st)


execStmt  (out, st) (ElemOf x setExpr) = eOutStore
  where
    eBool :: Either Err Bool
    eBool = evalIsElementOf st x setExpr

    eOutStore :: Either Err (Output, Store a)
    eOutStore = case eBool of
      Left err -> Left err
      Right b -> Right (pack (show b) : out, st)


-- execStmt (out, st) (SatisfiesPred x predExpr) = eOutStore


execProgram outSt [] = Right outSt

execProgram  os0@(out0, st0) (s:ss) =
  case eOutStore of
      Left err -> Left err
      Right os1 -> execProgram os1 ss
      
  where
    eOutStore = execStmt  os0 s


swapForBraces :: String -> String
swapForBraces str = map swapBrace str

swapBrace :: Char -> Char
swapBrace '[' = '{'
swapBrace ']' = '}'
swapBrace c = c

--------------------------------------------------------------------------------
