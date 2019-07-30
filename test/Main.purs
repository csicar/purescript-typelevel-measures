module Test.Main where

import Data.Type.Units
import Data.Type.Units.SI

import Data.Symbol (SProxy(..))
import Data.Type.Numbers (IProxy, N1, N2, N3, Neg, P0, P1, P2, P3, P4, P5, Pos, Succ, Z, minusOne, one, plus, undefined)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show)
import Prim.RowList (kind RowList, Cons, Nil)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Type.Row (type (+))



two :: IProxy (Pos (Succ (Succ Z)))
two = undefined

three :: IProxy (Pos (Succ (Succ (Succ Z))))
three = undefined

minusTwo :: IProxy (Neg (Succ (Succ Z)))
minusTwo = undefined

minusZero :: IProxy (Neg Z)
minusZero = undefined

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

t8 :: IProxy P1
t8 = one `plus` minusZero

--Test Insert


-- t11 :: RLProxy Nil -> SProxy "meter" -> MeasureExp MeterT P1 -> RLProxy (Cons "meter" (MeasureExp MeterT P1) Nil)
-- t11 = insert

-- t12 :: 
--   RLProxy (Cons "meter" (MeasureExp MeterT P1) Nil) 
--   -> SProxy "meter"
--   -> MeasureExp MeterT P1
--   -> RLProxy (Cons "meter" (MeasureExp MeterT P2) Nil)
-- t12 = insert

-- t13 :: RLProxy (Cons "meter" (MeasureExp MeterT P1) Nil) -> SProxy "meter" -> (MeasureExp MeterT P3) -> RLProxy (Cons "meter" (MeasureExp MeterT P4) Nil)
-- t13 = insert

-- t14 :: RLProxy (Cons "meter" (MeasureExp MeterT P2) Nil) -> SProxy "meter" -> (MeasureExp MeterT P3) -> RLProxy (Cons "meter" (MeasureExp MeterT P5) Nil)
-- t14 = insert

-- t15 :: RLProxy (Cons "meter" (MeasureExp MeterT P2) (Cons "sec" (MeasureExp MeterT P1) Nil)) -> SProxy "meter" -> (MeasureExp MeterT P3) -> RLProxy (Cons "meter" (MeasureExp MeterT P5) (Cons "sec" (MeasureExp MeterT P1) Nil))
-- t15 = insert

-- t16 :: RLProxy (Cons "meter" (MeasureExp MeterT P1) (Cons "sec" (MeasureExp SecT P2) Nil)) -> SProxy "sec" -> (MeasureExp SecT P2) -> RLProxy (Cons "meter" (MeasureExp MeterT P1) (Cons "sec" (MeasureExp SecT P4) Nil))
-- t16 = insert

-- -- t17 :: RLProxy (Cons "meter" (MeasureExp MeterT P1) (Cons "sec" (MeasureExp MeterT P2) Nil)) -> SProxy "sec" -> (MeasureExp SecT N3) -> RLProxy (Cons "meter" (MeasureExp MeterT P1) (Cons "sec" (MeasureExp MeterT N1) Nil))
-- -- t17 = insert

-- -- t18 :: RLProxy (Cons "meter" (MeasureExp SecT P1) Nil) -> SProxy "sec" -> MeasureExp SecT

-- -- Test Add RowLists

-- t21 :: RLProxy (Cons "meter" (MeasureExp MeterT P1) Nil) -> RLProxy (Cons "sec" (MeasureExp SecT P1) Nil) -> RLProxy (Cons "sec" (MeasureExp SecT P1) (Cons "meter" (MeasureExp MeterT P1) Nil))
-- t21 = addRowLists

-- t22 :: RLProxy (Cons "meter" (MeasureExp MeterT P2) Nil) -> RLProxy (Cons "sec" (MeasureExp SecT P1) Nil) -> RLProxy (Cons "sec" (MeasureExp SecT P1) (Cons "meter" (MeasureExp MeterT P2) Nil))
-- t22 = addRowLists

-- t23 :: RLProxy (Cons "meter" (MeasureExp MeterT P2) Nil) -> RLProxy (Cons "sec" (MeasureExp SecT P0) Nil) -> RLProxy (Cons "sec" (MeasureExp SecT P0) (Cons "meter" (MeasureExp MeterT P2) Nil))
-- t23 = addRowLists

-- t24 :: RLProxy (Cons "meter" (MeasureExp MeterT P2) Nil) -> RLProxy (Cons "meter" (MeasureExp MeterT P2) Nil) -> RLProxy (Cons "meter" (MeasureExp MeterT P4) Nil)
-- t24 = addRowLists

-- t25 :: RLProxy (Cons "meter" (MeasureExp MeterT P2) Nil) -> RLProxy (Cons "meter" (MeasureExp MeterT N3) Nil) -> RLProxy (Cons "meter" (MeasureExp MeterT N1) Nil)
-- t25 = addRowLists

-- t26 :: RLProxy (Cons "meter" (MeasureExp MeterT P1) Nil) -> RLProxy (Cons "sec" (MeasureExp SecT P1) Nil) -> RLProxy (Cons "sec" (MeasureExp SecT P1) (Cons "meter" (MeasureExp MeterT P1) Nil))
-- t26 = addRowLists

-- -- t27 :: RLProxy (Cons "sec" (MeasureExp MeterT P1) Nil) -> RLProxy (Cons "meter" (MeasureExp SecT P1) Nil) -> RLProxy (Cons "sec" (MeasureExp SecT P1) (Cons "meter" (MeasureExp MeterT P1) Nil))
-- -- t27 = addRowLists

-- Test Add Rows

t31 :: RProxy () -> RProxy () -> RProxy ()
t31 = addRows

t32 :: RProxy (meter :: MeasureExp MeterT P1) -> RProxy () -> RProxy (meter :: MeasureExp MeterT P1)
t32 = addRows

t33 :: RProxy (meter :: MeasureExp MeterT P1) -> RProxy (meter :: MeasureExp MeterT P2) -> RProxy (meter :: MeasureExp MeterT P3)
t33 = addRows

t34 :: RProxy (sec :: MeasureExp SecT P1) -> RProxy (meter :: MeasureExp MeterT P1) -> RProxy (meter :: MeasureExp MeterT P1, sec :: MeasureExp SecT P1)
t34 = addRows

t35 :: RProxy (meter :: MeasureExp MeterT P1) -> RProxy (sec :: MeasureExp SecT P1) -> RProxy (meter :: MeasureExp MeterT P1, sec :: MeasureExp SecT P1)
t35 = addRows

t36 :: RProxy (meter :: MeasureExp MeterT P1) -> RProxy (sec :: MeasureExp SecT P2) -> RProxy (meter :: MeasureExp MeterT P1, sec :: MeasureExp SecT P2)
t36 = addRows

t37 :: RProxy (meter :: (MeasureExp MeterT P1)) -> RProxy (sec :: (MeasureExp SecT P1)) -> RProxy (sec :: (MeasureExp SecT P1), meter :: (MeasureExp MeterT P1))
t37 = addRows

t38 :: RProxy (meter :: (MeasureExp MeterT P1)) -> RProxy (meter :: (MeasureExp MeterT P1)) -> RProxy (meter :: (MeasureExp MeterT P2))
t38 = addRows

t39 :: RProxy (meter :: (MeasureExp MeterT P1)) -> RProxy (sec :: (MeasureExp SecT P1)) -> RProxy (meter :: (MeasureExp MeterT P1), sec :: (MeasureExp SecT P1))
t39 = addRows

t40 :: RProxy (sec :: (MeasureExp SecT P1)) -> RProxy (meter :: (MeasureExp MeterT P1)) -> RProxy (meter :: (MeasureExp MeterT P1), sec :: (MeasureExp SecT P1))
t40 = addRows

unitLess :: Int : ()
unitLess = Measured 1

m :: Int : Sec P2 * Meter P1 ()
m = Measured 1

t0 :: Measured Int
  ( meter :: MeasureExp MeterT (Pos (Succ Z))
  , sec :: MeasureExp SecT (Pos (Succ (Succ Z)))
  )
t0 = m ** unitLess

m2 :: Int : Sec N1 * Meter N2 ()
m2 = Measured 2


m3 :: Int : Sec P1 ()
m3 = Measured 2

m4 :: Int : Sec P2 ()
m4 = Measured 4

m5 :: Int : Meter P1 * Sec P1 ()
m5 = Measured 5

tt1 :: Measured Int
  ( sec :: MeasureExp SecT (Pos (Succ (Succ (Succ Z))))
  )
tt1 = m3 ** m4


t :: Measured Int
                                       ( meter :: MeasureExp MeterT (Neg (Succ Z))
                                       , sec :: MeasureExp SecT (Pos (Succ Z))
                                       )
t = m ** m2


tt2 :: Measured Int
  ( meter :: MeasureExp MeterT (Pos (Succ Z))
  , sec :: MeasureExp SecT (Pos (Succ (Succ (Succ Z))))
  )
tt2 = m4 ** m5



distance :: Int : Meter P1 ()
distance = (const 10) ** meter

speed :: Int : Meter P1 * Sec P1 ()
speed = meter ** sec


main :: Effect Unit
main = do
  log (show t)
  log (show (Measured 12 :: Int : Meter P5 * Sec N2 ()))
  log "Done"