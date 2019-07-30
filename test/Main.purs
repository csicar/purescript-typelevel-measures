module Test.Main where

import Data.Type.Units (type (*), type (/), type (:), Measured(..), mul, valOf)
import Data.Type.Units.SI (Meter, Sec, Newton, Kg, meter)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit)

main :: Effect Unit
main = do
  log "You should add some tests."



m1 :: Int : Kg * Meter * () / ()
m1 = valOf 11

m2 :: Int : () / Sec*Sec*()
m2 = valOf 34

x :: (Newton Int () ())
x = m1 `mul` m2