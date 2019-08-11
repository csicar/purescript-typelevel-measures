module Test.Main where

import Prelude
import Data.Foldable (class Foldable, foldl)
import Effect (Effect)
import Effect.Console (log, logShow)
import Type.Data.Int (class SumInt,Pos)
import Type.Data.Row (RProxy)
import Type.Data.Units (class ShowMeasure, type (*), type (:), MeasureExp, Measured, addRows, liftV, (**), (//), (++), kind Measure)
import Type.Data.Units.SI (AmpereT, Joule, Kg, KgT, Meter, Meter', MeterT, Newton, Ohm, Sec, SecT, Volt, Ampere, ampere, joule, meter, ohm, sec, volt)
import Unsafe.Coerce (unsafeCoerce)

-- Test Add Rows
t31 :: RProxy () -> RProxy () -> RProxy ()
t31 = addRows

t32 :: RProxy ( meter :: MeasureExp MeterT "1" ) -> RProxy () -> RProxy ( meter :: MeasureExp MeterT "1" )
t32 = addRows

t33 :: RProxy ( meter :: MeasureExp MeterT "1" ) -> RProxy ( meter :: MeasureExp MeterT "2" ) -> RProxy ( meter :: MeasureExp MeterT "3" )
t33 = addRows

t34 :: RProxy ( sec :: MeasureExp SecT "1" ) -> RProxy ( meter :: MeasureExp MeterT "1" ) -> RProxy ( meter :: MeasureExp MeterT "1", sec :: MeasureExp SecT "1" )
t34 = addRows

t35 :: RProxy ( meter :: MeasureExp MeterT "1" ) -> RProxy ( sec :: MeasureExp SecT "1" ) -> RProxy ( meter :: MeasureExp MeterT "1", sec :: MeasureExp SecT "1" )
t35 = addRows

t36 :: RProxy ( meter :: MeasureExp MeterT "1" ) -> RProxy ( sec :: MeasureExp SecT "2" ) -> RProxy ( meter :: MeasureExp MeterT "1", sec :: MeasureExp SecT "2" )
t36 = addRows

t37 :: RProxy ( meter :: (MeasureExp MeterT "1") ) -> RProxy ( sec :: (MeasureExp SecT "1") ) -> RProxy ( sec :: (MeasureExp SecT "1"), meter :: (MeasureExp MeterT "1") )
t37 = addRows

t38 :: RProxy ( meter :: (MeasureExp MeterT "1") ) -> RProxy ( meter :: (MeasureExp MeterT "1") ) -> RProxy ( meter :: (MeasureExp MeterT "2") )
t38 = addRows

t39 :: RProxy ( meter :: (MeasureExp MeterT "1") ) -> RProxy ( sec :: (MeasureExp SecT "1") ) -> RProxy ( meter :: (MeasureExp MeterT "1"), sec :: (MeasureExp SecT "1") )
t39 = addRows

t40 :: RProxy ( sec :: (MeasureExp SecT "1") ) -> RProxy ( meter :: (MeasureExp MeterT "1") ) -> RProxy ( meter :: (MeasureExp MeterT "1"), sec :: (MeasureExp SecT "1") )
t40 = addRows

unitLess :: Int : ()
unitLess = liftV 1

m :: Int : Sec "2" * Meter "1" ()
m = liftV 1 ** sec ** sec ** meter

t0 :: Measured Int
  ( meter :: MeasureExp MeterT "1"
  , sec :: MeasureExp SecT "2"
  )
t0 = m ** unitLess

m2 :: Int : Sec "-1" * Meter "-2" ()
m2 = liftV 2 // sec // meter // meter

m3 :: Int : Sec "1" ()
m3 = liftV 2 ** sec

m4 :: Int : Sec "2" ()
m4 = liftV 4 ** sec ** sec

m5 :: Int : Meter "1" * Sec "1" ()
m5 = liftV 5 ** meter ** sec


tt1 :: Measured Int
  ( sec :: MeasureExp SecT "3"
  )
tt1 = m3 ** m4


t :: Measured Int
  ( meter :: MeasureExp MeterT "-1"
  , sec :: MeasureExp SecT "1"
  )
t = m ** m2


tt2 :: Measured Int
  ( meter :: MeasureExp MeterT "1"
  , sec :: MeasureExp SecT "3"
  )
tt2 = m4 ** m5

distance :: Int : Meter "1" ()
distance = liftV 10 ** meter

s :: Int : Sec "-1" ()
s = liftV 1 // sec

speed :: Int : Meter "1" * Sec "-1" ()
speed = meter // sec

avgSpeed :: Int : Meter "1" () -> Int : Sec "1" () -> Int : Meter "1" * Sec "-1" ()
avgSpeed a b = a // b

speedOver10m :: Int : Meter "1" * Sec "-1" ()
speedOver10m = avgSpeed distance (liftV 5 ** sec)

energyInBarOfChocolate :: Int : Joule ()
energyInBarOfChocolate = liftV 2_300_000 ** joule

forceOver5Meter :: Int : Newton ()
forceOver5Meter = energyInBarOfChocolate // (liftV 5 ** meter)


typeInferenceTest :: Measured Int
  ( kg :: MeasureExp KgT "1"
  , meter :: MeasureExp MeterT "2"
  , sec :: MeasureExp SecT "-3"
  )
typeInferenceTest = forceOver5Meter ** liftV 5 ** meter // sec

-- voltage :: Number -> Number : Volt ()
voltage :: Number
  -> Measured Number
     ( ampere :: MeasureExp AmpereT "-1"
     , kg :: MeasureExp KgT "1"
     , meter :: MeasureExp MeterT "2"
     , sec :: MeasureExp SecT "-3"
     )
voltage v = liftV (v :: Number) ** volt

-- resistance :: Number -> Number : Ohm ()
resistance :: Number
  -> Measured Number
     ( ampere :: MeasureExp AmpereT "-2"
     , kg :: MeasureExp KgT "1"
     , meter :: MeasureExp MeterT "2"
     , sec :: MeasureExp SecT "-3"
     )
resistance r = liftV (r :: Number) ** ohm

amperage :: Number -> Number : Ampere "1" ()
amperage a = liftV a ** ampere

isSame :: Boolean
isSame = (voltage 6.0 // resistance 2.0) == amperage 3.0

sum :: forall f v m. Foldable f => Semiring v => Measured v m -> f (Measured v m) -> Measured v m
sum as = foldl (++) as

invertR :: Array (Number : Ohm ()) -> Array _
invertR = map (\r -> liftV 1.0 // r)

sumOfInverted :: Array
  (Measured Number
     ( ampere :: MeasureExp AmpereT "-2"
     , kg :: MeasureExp KgT "1"
     , meter :: MeasureExp MeterT "2"
     , sec :: MeasureExp SecT "-3"
     )
  )
  -> Measured Number
      ( ampere :: MeasureExp AmpereT "2"
      , kg :: MeasureExp KgT "-1"
      , meter :: MeasureExp MeterT "-2"
      , sec :: MeasureExp SecT "3"
      )
sumOfInverted rs = sum (liftV 0.0 // ohm) $ invertR rs

sumOfInverted' ::
  Array (Number : Ohm ()) -> Number : Ampere "2" * Kg "-1" * Meter "-2" * Sec "3" ()
sumOfInverted' rs =
  rs
    # map (\r -> liftV 1.0 // r)
    # foldl (++) (liftV 0.0 // ohm)

-- Test custom Measure
foreign import data MyUnitOfMeasureT :: Measure

type MyUnitOfMeasure exp r
  = ( myUnit :: MeasureExp MyUnitOfMeasureT exp | r )

type MyUnitOfMeasure' r
  = MyUnitOfMeasure "1" r

myUnit :: ∀ a. Semiring a => a : MyUnitOfMeasure "1" ()
myUnit = unsafeCoerce $ (liftV one :: a : ())

instance showMeter :: ShowMeasure MyUnitOfMeasureT where
  showMeasure _ = "μU"

valOfMyUnit :: Int : MyUnitOfMeasure' * Meter' ()
valOfMyUnit = liftV 2 ** myUnit ** meter

main :: Effect Unit
main = do
  logShow valOfMyUnit
  logShow unitLess
  logShow ((unsafeCoerce $ liftV 12) :: Int : Meter "5" * Sec "-2" ())
  logShow (forceOver5Meter)
  logShow energyInBarOfChocolate
  logShow isSame
  logShow $ sumOfInverted' [ resistance 2.0, resistance 2.0 ]
  log "Done"
