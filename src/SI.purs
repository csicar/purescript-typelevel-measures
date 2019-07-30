module Data.Type.Units.SI where
  
import Data.Type.Units
import Prelude

data MeterT 

instance showM :: Show MeterT 
   where show _ = "m"

type Meter r = (meter :: MeterT | r)

meter :: ∀t. Semiring t => t : (Meter * ()) / ()
meter = oneM

data KgT

instance showKg :: Show KgT
   where show _ = "kg"

kg :: ∀t. Semiring t => t : Kg*()/()
kg = oneM

type Kg r = (kg :: KgT | r)

data SecT

instance showSec :: Show SecT
   where show _ = "s"

type Sec r = (sec :: SecT | r)

sec :: ∀t. Semiring t => t : Sec*()/()
sec = oneM

data AmpereT

instance showAmpere :: Show AmpereT where
  show _ = "A"

type Ampere r = (ampere :: AmpereT | r)

ampere :: ∀t. Semiring t => t : Ampere*()/()
ampere = oneM

data MoleT

instance showMole :: Show MoleT where
  show _ = "mol"

type Mole r = (mole :: MoleT | r)

mole :: ∀t. Semiring t => t : Mole*()/()
mole = oneM

data KelvinT

instance showKelvin :: Show KelvinT where
  show _ = "K"

type Kelvin r = (kelvin :: KelvinT | r)

kelvin :: ∀t. Semiring t => t : Kelvin*()/()
kelvin = oneM

type Newton  v a b = v : Kg * Meter * a / Sec * Sec * b




