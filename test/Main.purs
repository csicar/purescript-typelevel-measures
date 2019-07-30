module Test.Main where

import Data.Type.Units

import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit)
import Prim.RowList (kind RowList, Cons, Nil)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))


two :: IProxy (Pos (Succ (Succ Z)))
two = undefined

three :: IProxy (Pos (Succ (Succ (Succ Z))))
three = undefined

minusTwo :: IProxy (Neg (Succ (Succ Z)))
minusTwo = undefined

-- Test Add

t1 :: IProxy (Pos (Succ (Succ (Succ Z))))
t1 = one `plus` two


t1' :: IProxy (Pos (Succ (Succ (Succ Z))))
t1' = two `plus` one

t2 :: IProxy (Neg (Succ (Succ (Succ Z))))
t2 = minusTwo `plus` minusOne

t2' :: IProxy (Neg (Succ (Succ (Succ Z))))
t2' = minusTwo `plus` minusOne

t3 :: IProxy (Neg (Succ Z))
t3 = minusTwo `plus` one

t3' :: IProxy (Neg (Succ Z))
t3' = one `plus` minusTwo

t4 :: IProxy (Neg (Succ (Succ (Succ (Succ Z)))))
t4 = minusTwo `plus` minusTwo

t5 :: IProxy (Neg (Succ (Succ Z)))
t5 = t4 `plus` two

t5' :: IProxy (Neg (Succ (Succ Z)))
t5' = two `plus` t4

t6 :: IProxy (Neg (Succ (Succ Z)))
t6 = two `plus` (minusTwo `plus` minusTwo)

t6' :: IProxy (Neg (Succ (Succ Z)))
t6' = (minusTwo `plus` minusTwo) `plus` two

t7 :: IProxy (Pos Z)
t7 = minusOne `plus` one

t7' :: IProxy (Pos Z)
t7' = one `plus` minusOne

--Test Insert


t11 :: RLProxy Nil -> SProxy "meter" -> MeasureExp MeterT P1 -> RLProxy (Cons "meter" (MeasureExp MeterT P1) Nil)
t11 = insert

t12 :: 
  RLProxy (Cons "meter" (MeasureExp MeterT P1) Nil) 
  -> SProxy "meter"
  -> MeasureExp MeterT P1
  -> RLProxy (Cons "meter" (MeasureExp MeterT P2) Nil)
t12 = insert

t13 :: RLProxy (Cons "meter" (MeasureExp MeterT P1) Nil) -> SProxy "meter" -> (MeasureExp MeterT P3) -> RLProxy (Cons "meter" (MeasureExp MeterT P4) Nil)
t13 = insert

t14 :: RLProxy (Cons "meter" (MeasureExp MeterT P2) Nil) -> SProxy "meter" -> (MeasureExp MeterT P3) -> RLProxy (Cons "meter" (MeasureExp MeterT P5) Nil)
t14 = insert

t15 :: RLProxy (Cons "meter" (MeasureExp MeterT P2) (Cons "sec" (MeasureExp MeterT P1) Nil)) -> SProxy "meter" -> (MeasureExp MeterT P3) -> RLProxy (Cons "meter" (MeasureExp MeterT P5) (Cons "sec" (MeasureExp MeterT P1) Nil))
t15 = insert

t16 :: RLProxy (Cons "meter" (MeasureExp MeterT P1) (Cons "sec" (MeasureExp MeterT P2) Nil)) -> SProxy "sec" -> (MeasureExp MeterT P2) -> RLProxy (Cons "meter" (MeasureExp MeterT P1) (Cons "sec" (MeasureExp MeterT P4) Nil))
t16 = insert

t17 :: RLProxy (Cons "meter" (MeasureExp MeterT P1) (Cons "sec" (MeasureExp MeterT P2) Nil)) -> SProxy "sec" -> (MeasureExp MeterT N3) -> RLProxy (Cons "meter" (MeasureExp MeterT P1) (Cons "sec" (MeasureExp MeterT N1) Nil))
t17 = insert

-- Test Add RowLists

t21 :: RLProxy (Cons "meter" (MeasureExp MeterT P1) Nil) -> RLProxy (Cons "sec" (MeasureExp SecT P1) Nil) -> RLProxy (Cons "sec" (MeasureExp SecT P1) (Cons "meter" (MeasureExp MeterT P1) Nil))
t21 = addRowLists

t22 :: RLProxy (Cons "meter" (MeasureExp MeterT P2) Nil) -> RLProxy (Cons "sec" (MeasureExp SecT P1) Nil) -> RLProxy (Cons "sec" (MeasureExp SecT P1) (Cons "meter" (MeasureExp MeterT P2) Nil))
t22 = addRowLists

t23 :: RLProxy (Cons "meter" (MeasureExp MeterT P2) Nil) -> RLProxy (Cons "sec" (MeasureExp SecT P0) Nil) -> RLProxy (Cons "sec" (MeasureExp SecT P0) (Cons "meter" (MeasureExp MeterT P2) Nil))
t23 = addRowLists

t24 :: RLProxy (Cons "meter" (MeasureExp MeterT P2) Nil) -> RLProxy (Cons "meter" (MeasureExp MeterT P2) Nil) -> RLProxy (Cons "meter" (MeasureExp MeterT P4) Nil)
t24 = addRowLists

t25 :: RLProxy (Cons "meter" (MeasureExp MeterT P2) Nil) -> RLProxy (Cons "meter" (MeasureExp MeterT N3) Nil) -> RLProxy (Cons "meter" (MeasureExp MeterT N1) Nil)
t25 = addRowLists

-- Test Add Rows

t31 :: RProxy (meter :: (MeasureExp MeterT P1)) -> RProxy (sec :: (MeasureExp SecT P1)) -> RProxy (sec :: (MeasureExp SecT P1), meter :: (MeasureExp MeterT P1))
t31 = undefined

t32 :: RProxy (meter :: (MeasureExp MeterT P1)) -> RProxy (meter :: (MeasureExp MeterT P1)) -> RProxy (sec :: (MeasureExp SecT P1), meter :: (MeasureExp MeterT P1))
t32 = undefined


m :: Measured Int (meter :: MeasureExp MeterT P1)
m = Measured 1

m2 :: Measured Int (meter :: MeasureExp MeterT N2, sec :: MeasureExp SecT P1)
m2 = Measured 2


t :: Measured Int
  ( meter :: MeasureExp MeterT (Neg (Succ Z))
  , sec :: MeasureExp SecT (Pos (Succ Z))
  )
t = m `mult` m2

main :: Effect Unit
main = do
  log "You should add some tests."