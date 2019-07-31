module Type.Data.Units.SI where

import Prelude
import Type.Data.Peano.Int (N1, N2, N3, P1, P2, N4)
import Type.Data.Units (class ShowMeasure, type (*), type (:), MeasureExp, liftV, kind Measure)
import Unsafe.Coerce (unsafeCoerce)

-- Meter
foreign import data MeterT :: Measure

type Meter exp r
  = ( meter :: MeasureExp MeterT exp | r )

-- | Meter with default exponent 1
-- |
-- | `Sec P2 * Meter' * Kg () ≡ Sec P2 * Meter P1 * Kg ()`
type Meter' r
  = Meter P1 r

meter :: ∀ a. Semiring a => a : Meter P1 ()
meter = unsafeCoerce $ (liftV one :: a : ())

instance showMeter :: ShowMeasure MeterT where
  showMeasure _ = "m"

-- Kilogram
foreign import data KgT :: Measure

type Kg exp r
  = ( kg :: MeasureExp KgT exp | r )

type Kg' r
  = Kg P1 r

kg :: ∀ a. Semiring a => a : Kg P1 ()
kg = unsafeCoerce $ (liftV one :: a : ())

instance showkg :: ShowMeasure KgT where
  showMeasure _ = "kg"

-- Second
foreign import data SecT :: Measure

type Sec exp r
  = ( sec :: MeasureExp SecT exp | r )

type Sec' r
  = Sec P1 r

sec :: ∀ a. Semiring a => a : Sec P1 ()
sec = unsafeCoerce $ (liftV one :: a : ())

instance showSec :: ShowMeasure SecT where
  showMeasure _ = "s"

-- Ampere
foreign import data AmpereT :: Measure

type Ampere exp r
  = ( ampere :: MeasureExp AmpereT exp | r )

ampere :: ∀ a. Semiring a => a : Ampere P1 ()
ampere = unsafeCoerce $ (liftV one :: a : ())

instance showAmpere :: ShowMeasure AmpereT where
  showMeasure _ = "A"

-- Mole
foreign import data MoleT :: Measure

type Mole exp r
  = ( mole :: MeasureExp MoleT exp | r )

type Mole' r
  = Mole P1 r

mole :: ∀ a. Semiring a => a : Mole P1 ()
mole = unsafeCoerce $ (liftV one :: a : ())

instance showMole :: ShowMeasure MoleT where
  showMeasure _ = "mol"

-- Kelvin
foreign import data KelvinT :: Measure

type Kelvin exp r
  = ( kelvin :: MeasureExp KelvinT exp | r )

type Kelvin' r
  = Kelvin P1 r

kelvin :: ∀ a. Semiring a => a : Kelvin P1 ()
kelvin = unsafeCoerce $ (liftV one :: a : ())

instance showKelvin :: ShowMeasure KelvinT where
  showMeasure _ = "K"

-- Candela
foreign import data CandelaT :: Measure

type Candela exp r
  = ( candela :: MeasureExp CandelaT exp | r )

type Candela' r
  = Candela P1 r

candela :: ∀ a. Semiring a => a : Candela P1 ()
candela = unsafeCoerce $ (liftV one :: a : ())

instance showCandela :: ShowMeasure CandelaT where
  showMeasure _ = "cd"

-- Derived
type Newton r
  = Kg P1 * Meter P1 * Sec N2 * r

newton :: ∀ a. Semiring a => a : Newton ()
newton = unsafeCoerce $ (liftV one :: a : ())

type Hertz r
  = Sec N1 * r

hertz :: ∀ a. Semiring a => a : Hertz ()
hertz = unsafeCoerce $ (liftV one :: a : ())

type Pascal r
  = Kg P1 * Meter N1 * Sec N2 * r

pascal :: ∀ a. Semiring a => a : Pascal ()
pascal = unsafeCoerce $ (liftV one :: a : ())

type Joule r
  = Kg P1 * Meter P2 * Sec N2 * r

joule :: ∀ a. Semiring a => a : Joule ()
joule = unsafeCoerce $ (liftV one :: a : ())

type Watt r
  = Kg P1 * Meter P2 * Sec N3 * r

watt :: ∀ a. Semiring a => a : Watt ()
watt = unsafeCoerce $ (liftV one :: a : ())

type Coulomb r
  = Ampere P1 * Sec P1 * r

coulomb :: ∀ a. Semiring a => a : Coulomb ()
coulomb = unsafeCoerce $ (liftV one :: a : ())

type Volt r
  = Kg P1 * Meter P2 * Ampere N1 * Sec N3 * r

volt :: ∀ a. Semiring a => a : Volt ()
volt = unsafeCoerce $ (liftV one :: a : ())

type Farad r
  = Ampere P2 * Sec N4 * Kg N2 * Meter N2 * r

farad :: ∀ a. Semiring a => a : Farad ()
farad = unsafeCoerce $ (liftV one :: a : ())

type Ohm r
  = Kg P1 * Meter P2 * Sec N3 * Ampere N2 * r

ohm :: ∀ a. Semiring a => a : Ohm ()
ohm = unsafeCoerce $ (liftV one :: a : ())
