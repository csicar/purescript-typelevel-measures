module Data.Type.Units.SI where


import Data.Type.Units


foreign import data MeterT :: Measure
type Meter exp r = (meter :: MeasureExp MeterT exp | r)

instance showMeter :: ShowMeasure MeterT where
  showMeasure _ = "m"


foreign import data SecT :: Measure
type Sec exp r = (sec :: MeasureExp SecT exp | r)

instance showSec :: ShowMeasure SecT where
   showMeasure _ = "s"