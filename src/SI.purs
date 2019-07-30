module Data.Type.Units.SI where


import Data.Type.Units
import Prelude

import Data.Type.Numbers (P1)


foreign import data MeterT :: Measure
type Meter exp r = (meter :: MeasureExp MeterT exp | r)

meter :: ∀a. Semiring a => a : Meter P1 ()
meter = Measured one

instance showMeter :: ShowMeasure MeterT where
  showMeasure _ = "m"


foreign import data SecT :: Measure
type Sec exp r = (sec :: MeasureExp SecT exp | r)

sec :: ∀a. Semiring a => a : Sec P1 ()
sec = Measured one

instance showSec :: ShowMeasure SecT where
   showMeasure _ = "s"