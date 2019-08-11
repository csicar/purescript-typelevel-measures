module Type.Data.Units.SI where

import Prelude
import Type.Data.Units (class ShowMeasure, type (*), type (:), MeasureExp, liftV, kind Measure)
import Unsafe.Coerce (unsafeCoerce)

-- Meter
foreign import data MeterT :: Measure

type Meter exp r
  = ( meter :: MeasureExp MeterT exp | r )

-- | Meter with default exponent 1
-- |
-- | `Sec "2" * Meter' * Kg () ≡ Sec "2" * Meter "1" * Kg ()`
type Meter' r
  = Meter "1" r

meter :: ∀ a. Semiring a => a : Meter "1" ()
meter = unsafeCoerce $ (liftV one :: a : ())

instance showMeter :: ShowMeasure MeterT where
  showMeasure _ = "m"

-- Kilogram
foreign import data KgT :: Measure

type Kg exp r
  = ( kg :: MeasureExp KgT exp | r )

type Kg' r
  = Kg "1" r

kg :: ∀ a. Semiring a => a : Kg "1" ()
kg = unsafeCoerce $ (liftV one :: a : ())

instance showkg :: ShowMeasure KgT where
  showMeasure _ = "kg"

-- Second
foreign import data SecT :: Measure

type Sec exp r
  = ( sec :: MeasureExp SecT exp | r )

type Sec' r
  = Sec "1" r

sec :: ∀ a. Semiring a => a : Sec "1" ()
sec = unsafeCoerce $ (liftV one :: a : ())

instance showSec :: ShowMeasure SecT where
  showMeasure _ = "s"

-- Ampere
foreign import data AmpereT :: Measure

type Ampere exp r
  = ( ampere :: MeasureExp AmpereT exp | r )

ampere :: ∀ a. Semiring a => a : Ampere "1" ()
ampere = unsafeCoerce $ (liftV one :: a : ())

instance showAmpere :: ShowMeasure AmpereT where
  showMeasure _ = "A"

-- Mole
foreign import data MoleT :: Measure

type Mole exp r
  = ( mole :: MeasureExp MoleT exp | r )

type Mole' r
  = Mole "1" r

mole :: ∀ a. Semiring a => a : Mole "1" ()
mole = unsafeCoerce $ (liftV one :: a : ())

instance showMole :: ShowMeasure MoleT where
  showMeasure _ = "mol"

-- Kelvin
foreign import data KelvinT :: Measure

type Kelvin exp r
  = ( kelvin :: MeasureExp KelvinT exp | r )

type Kelvin' r
  = Kelvin "1" r

kelvin :: ∀ a. Semiring a => a : Kelvin "1" ()
kelvin = unsafeCoerce $ (liftV one :: a : ())

instance showKelvin :: ShowMeasure KelvinT where
  showMeasure _ = "K"

-- Candela
foreign import data CandelaT :: Measure

type Candela exp r
  = ( candela :: MeasureExp CandelaT exp | r )

type Candela' r
  = Candela "1" r

candela :: ∀ a. Semiring a => a : Candela "1" ()
candela = unsafeCoerce $ (liftV one :: a : ())

instance showCandela :: ShowMeasure CandelaT where
  showMeasure _ = "cd"

-- Derived
type Newton r
  = Kg "1" * Meter "1" * Sec "-2" * r

newton :: ∀ a. Semiring a => a : Newton ()
newton = unsafeCoerce $ (liftV one :: a : ())

type Hertz r
  = Sec "-1" * r

hertz :: ∀ a. Semiring a => a : Hertz ()
hertz = unsafeCoerce $ (liftV one :: a : ())

type Pascal r
  = Kg "1" * Meter "-1" * Sec "-2" * r

pascal :: ∀ a. Semiring a => a : Pascal ()
pascal = unsafeCoerce $ (liftV one :: a : ())

type Joule r
  = Kg "1" * Meter "2" * Sec "-2" * r

joule :: ∀ a. Semiring a => a : Joule ()
joule = unsafeCoerce $ (liftV one :: a : ())

type Watt r
  = Kg "1" * Meter "2" * Sec "-3" * r

watt :: ∀ a. Semiring a => a : Watt ()
watt = unsafeCoerce $ (liftV one :: a : ())

type Coulomb r
  = Ampere "1" * Sec "1" * r

coulomb :: ∀ a. Semiring a => a : Coulomb ()
coulomb = unsafeCoerce $ (liftV one :: a : ())

type Volt r
  = Kg "1" * Meter "2" * Ampere "-1" * Sec "-3" * r

volt :: ∀ a. Semiring a => a : Volt ()
volt = unsafeCoerce $ (liftV one :: a : ())

type Farad r
  = Ampere "2" * Sec "-4" * Kg "-2" * Meter "-2" * r

farad :: ∀ a. Semiring a => a : Farad ()
farad = unsafeCoerce $ (liftV one :: a : ())

type Ohm r
  = Kg "1" * Meter "2" * Sec "-3" * Ampere "-2" * r

ohm :: ∀ a. Semiring a => a : Ohm ()
ohm = unsafeCoerce $ (liftV one :: a : ())
