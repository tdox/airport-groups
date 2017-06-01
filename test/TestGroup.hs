-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module TestGroup where


-- containers
import Data.Map (empty)
import qualified Data.Map as M
import Data.Set (fromList)

-- text
-- import Data.Text (Text, pack, unpack)

-- trace
-- import Debug.Trace

import Groups


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

Right b1 = evalIsElementOf st 2 s1
ok5 = b1

Right b2 = evalIsElementOf st 9 s1
ok6 = not b2

Right b3 = evalIsElementOf st 1 (Union s1 s2)
ok7 = b3

Right b4 = evalIsElementOf st 9 (Union s1 s2)
ok8 = not b4

Right b5 = evalIsElementOf st 2 (Intersection s1 s2)
ok9 = b5

Right b6 = evalIsElementOf st 1 (Intersection s1 s2)
ok10 = not b6

Right b7 = evalIsElementOf st 1 (Difference s1 s2)
ok11 = b7

Right b8 = evalIsElementOf st 2 (Difference s1 s2)
ok12 = not b8

Right b9 = evalIsElementOf st 10 (Difference s1 s2)
ok13 = not b9

Right b10 = evalIsElementOf st 1 $ SuchThat s1 $ PLit $ Pred (> 2)
ok14 = not b10

Right b11 = evalIsElementOf st 3 $ SuchThat s1 $ PLit $ Pred (> 2)
ok15 = b11

Right b12 = evalIsElementOf st 9 $ SuchThat s1 $ PLit $ Pred (> 2)
ok16 = not b12

b13 = evalPred p1 0
ok17 = not b13

b14 = evalPred p1 5
ok18 = b14

p2 = Pred (< 5)

p3 :: Pred Int
Right p3 = evalPredExpr st (PAnd (PLit p1) (PLit p2))
b15 = evalPred p3 0
ok19 = not b15

b16 = evalPred p3 4
ok20 = b16

b17 = evalPred p3 5
ok21 = not b17

Right p4 = evalPredExpr st (POr (PLit p1) (PLit p2))

b18 = evalPred p4 0
ok22 = b18

b19 = evalPred p4 4
ok23 = b19

b20 = evalPred p4 5
ok24 = b20

Right p5 = evalPredExpr st (PNot (PLit (Pred (>3))))

b21 = evalPred p5 0
ok25 = b21

b22 = evalPred p5 5
ok26 = not b22

s5 = evalSetExpr st (SVar "s1") :: Either Err (Set Int)
ok27 = s5 == evalSetExpr st s1

st0 :: Store Int
st0 = empty

out0 :: Output
out0 = []

  
stmt1 = AssignSet "s3" s3
Right os1 = execStmt  (out0, st0) stmt1 :: Either Err (Output, Store Int)


stmt2 = Print "s3"
Right os2 = execStmt  os1 stmt2
ok28 = fst os2 == ["s3 = {1,2,3,4}"]

stmt3 = Print "s999"
Left os3 = execStmt  os2 stmt3
ok29 = os3 == "s999: undefined variable"

stmt4 = AssignSet "s4" s4
Right os4 = execStmt  os2 stmt4
stmt4a = Print "s4"
Right os4a = execStmt  os4 stmt4a
ok30 = fst os4a == ["s4 = {3,4}","s3 = {1,2,3,4}"]


stmt5 = AssignPred "p1" (PAnd (PLit p1) (PLit p2))
Right os5 = execStmt  os4a stmt5

prog1 = [stmt1, stmt2, stmt4, stmt4a, stmt5]
Right os6 = execProgram (out0, st0) prog1
ok31 = fst os6 == fst os4a


oks = [ok1, ok2, ok3, ok4, ok5, ok6, ok7, ok8, ok9, ok10, ok11, ok12, ok13
      , ok14, ok15, ok16, ok17, ok18, ok19, ok20, ok21, ok22, ok23, ok24, ok25
      , ok26, ok27, ok28, ok29, ok30, ok31]
      
ok = and oks


--------------------------------------------------------------------------------

