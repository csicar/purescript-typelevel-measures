module Test.Main where

import Data.Type.Units
import Data.Type.Units.SI

import Data.Symbol (SProxy(..))
import Data.Type.Numbers (IProxy, N1, N2, N3, Neg, P0, P1, P2, P3, P4, P5, P8, Pos, Succ, Z, P10, minusOne, one, parseInt, plus, prod, undefined)
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

t9 :: IProxy P4
t9 = (one `plus` one) `prod` (one `plus` one)

t10 :: IProxy P8
t10 = t9 `prod` (one `plus` one)

t11 :: IProxy (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
t11 = t10 `prod` t10

t12 :: IProxy (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
t12 = parseInt (undefined :: SProxy "64")

t13 :: IProxy P10
t13 = parseInt (undefined :: SProxy "10")

t14 :: IProxy N2
t14 = minusOne `prod` two

t14' :: IProxy N2
t14' = two `prod` minusOne

t15 :: IProxy P4
t15 = minusTwo `prod` minusTwo


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

s :: Int : Sec N1()
s = Measured 1

speed :: Int : Meter P1 * Sec N1 ()
speed = meter // sec


main :: Effect Unit
main = do
  log (show t)
  log (show (Measured 12 :: Int : Meter P5 * Sec N2 ()))
  log "Done"