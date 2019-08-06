module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Type.Data.Peano.Int (N1, N2, N3, Neg, P1, P2, P3, P5, Pos)
import Type.Data.Peano.Nat (kind Nat, Succ, Z)
import Type.Data.Row (RProxy)
import Type.Data.Units (class ShowMeasure, type (*), type (:), MeasureExp, Measured, addRows, liftV, (**), (//), kind Measure)
import Type.Data.Units.SI (AmpereT, Joule, Kg, KgT, Meter, Meter', MeterT, Newton, Ohm, Sec, SecT, Volt, Ampere, ampere, joule, meter, ohm, sec, volt)
import Unsafe.Coerce (unsafeCoerce)




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
unitLess = liftV 1

m :: Int : Sec P2 * Meter P1 ()
m = liftV 1 ** sec ** sec ** meter

t0 :: Measured Int
  ( meter :: MeasureExp MeterT (Pos (Succ Z))
  , sec :: MeasureExp SecT (Pos (Succ (Succ Z)))
  )
t0 = m ** unitLess

m2 :: Int : Sec N1 * Meter N2 ()
m2 = liftV 2 // sec // meter // meter


m3 :: Int : Sec P1 ()
m3 = liftV 2 ** sec

m4 :: Int : Sec P2 ()
m4 = liftV 4 ** sec ** sec

m5 :: Int : Meter P1 * Sec P1 ()
m5 = liftV 5 ** meter ** sec

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
distance = liftV 10 ** meter

s :: Int : Sec N1 ()
s = liftV 1 // sec

speed :: Int : Meter P1 * Sec N1 ()
speed = meter // sec

avgSpeed :: Int : Meter P1 () -> Int : Sec P1 () -> Int : Meter P1 * Sec N1 ()
avgSpeed a b = a // b


speedOver10m :: Int : Meter P1 * Sec N1 ()
speedOver10m = avgSpeed distance (liftV 5 ** sec)

energyInBarOfChocolate :: Int : Joule ()
energyInBarOfChocolate = liftV 2_300_000 ** joule

forceOver5Meter :: Int : Newton ()
forceOver5Meter = energyInBarOfChocolate // (liftV 5 ** meter)

typeInferenceTest :: Measured Int
  ( kg :: MeasureExp KgT (Pos (Succ Z))
  , meter :: MeasureExp MeterT (Pos (Succ (Succ Z)))
  , sec :: MeasureExp SecT (Neg (Succ (Succ (Succ Z))))
  )
typeInferenceTest = forceOver5Meter ** liftV 5 ** meter // sec

voltage :: Number -> Number : Volt ()
voltage v = liftV v ** volt

resistance :: Number -> Number : Ohm ()
resistance r = liftV r ** ohm

amperage :: Number -> Number : Ampere P1 ()
amperage a = liftV a ** ampere

isSame :: Boolean
isSame = (voltage 6.0 // resistance 2.0) == amperage 3.0


-- Test custom Measure
foreign import data MyUnitOfMeasureT :: Measure

type MyUnitOfMeasure exp r = (myUnit :: MeasureExp MyUnitOfMeasureT exp | r)
type MyUnitOfMeasure' r = MyUnitOfMeasure P1 r

myUnit :: ∀a. Semiring a => a : MyUnitOfMeasure P1 ()
myUnit = unsafeCoerce $ (liftV one :: a : ())

instance showMeter :: ShowMeasure MyUnitOfMeasureT where
  showMeasure _ = "μU" 

valOfMyUnit :: Int : MyUnitOfMeasure' * Meter' ()
valOfMyUnit = liftV 2 ** myUnit ** meter

main :: Effect Unit
main = do
  logShow valOfMyUnit
  logShow unitLess
  logShow ((unsafeCoerce $ liftV 12) :: Int : Meter P5 * Sec N2 ())
  logShow (forceOver5Meter)
  logShow energyInBarOfChocolate
  logShow isSame
  log "Done"