module Type.Data.Units (Measured, kind Measure, MProxy, MeasureExp(..), class Combine, combine, class AddRows, addRows, class InverseRow, class InverseRowList, mult, (**), divide, (//), liftV, add, (++), sub, (-*), type (*), type (:), class ShowMeasure, showMeasure, ShowRow) where

import Prelude

import Data.Array (index)
import Data.Maybe (Maybe(..))
import Prelude as Prelude
import Prim.RowList (kind RowList, Cons, Nil)
import Type.Data.Boolean (class If)
import Type.Data.Int
import Type.Prelude (class ListToRow, class RowToList, class Union, RLProxy, RProxy, kind Boolean, True, False)
import Type.Row (RowApply)
import Unsafe.Coerce (unsafeCoerce)
undefined :: ∀ a. a
undefined = unsafeCoerce unit

foreign import kind Measure

-- | Proxy from kind Measure to kind Type
data MProxy (m :: Measure)

-- | Base Measure with an Exponent. Represents `m^exp`
data MeasureExp (m :: Measure) (exp :: Symbol)

-- | Represents a value with a List of Measures with Exponents `value [m² * s³]`
-- |
-- | Types are constructed using the type aliases provided by the Base Units: 
-- |
-- | `Measured Int (Meter N3 ())` which represents Int with Unit `[1/m³]``
-- |
-- | The infix operator `:` allows for simpler syntax: `Int : Meter N3 ()`
data Measured v (u :: #Type)
  = Measured v

instance eqMeasured :: (Eq v) => Eq (Measured v u) where
  eq (Measured a) (Measured b) = eq a b

instance ordMeasured :: (Ord v) => Ord (Measured v u) where
  compare (Measured a) (Measured b) = compare a b

-- | Combines two sequential same symbols by adding their values
-- |
-- | Example: `Combine (m², m¹, s¹, s¹, kg) (m³, s², kg)`
class Combine (original :: RowList) (combined :: RowList) | original -> combined

instance combineSame ∷ (SumInt exp1 exp2 exp3, IsZeroInt exp3 isZero, If isZero (RLProxy tail') (RLProxy (Cons sym (MeasureExp m exp3) tail')) (RLProxy result), Combine tail tail') => Combine (Cons sym (MeasureExp m exp1) (Cons sym (MeasureExp m exp2) tail)) result
else instance combineDifferent :: (Combine rest rest') => Combine (Cons sym1 (MeasureExp m1 exp1) rest) (Cons sym1 (MeasureExp m1 exp1) rest')

instance combineNil :: Combine Nil Nil

-- | Valuelevel combine
combine :: ∀ a b. (Combine a b) => RLProxy a -> RLProxy b
combine = undefined

-- | Adds two rows, adding entries with the same type
class AddRows (a :: #Type) (b :: #Type) (sum :: #Type) | a b -> sum

instance addRowsCombine :: (Union a b sum, RowToList sum rsum, Combine rsum resultList, ListToRow resultList result) => AddRows a b result

addRows :: ∀ a b sum. (AddRows a b sum) => RProxy a -> RProxy b -> RProxy sum
addRows a b = undefined

class InverseRowList (original :: RowList) (inverted :: RowList) | original -> inverted, inverted -> original

instance inverseNil ∷ InverseRowList Nil Nil

instance inverseCons :: (InverseRowList tail inverseTail, Inverse ty inverseTy) => InverseRowList (Cons sym (MeasureExp m ty) tail) (Cons sym (MeasureExp m inverseTy) inverseTail)

-- | Invert a Row: `InserseRow (Meter P1 * Sec N3 ()) (Meter N1 * Sec N3 ())`
class InverseRow (original :: #Type) (inverted :: #Type) | inverted -> original

instance inverseRowList ∷ (RowToList original originalList, InverseRowList originalList resultList, ListToRow resultList result) => InverseRow original result

-- Methods
-- | multily values with measures
mult :: ∀ a b c v. AddRows a b c => Semiring v => Measured v a -> Measured v b -> Measured v c
mult (Measured a) (Measured b) = Measured (a `Prelude.mul` b)

infixl 7 mult as **

-- | divide values with measures
divide :: ∀ a b c v ib. InverseRow b ib => AddRows a ib c => EuclideanRing v => Measured v a -> Measured v b -> Measured v c
divide (Measured a) (Measured b) = Measured (a / b)

infixl 7 divide as //

-- | lifts a Value to a unitless Measured
liftV :: ∀ a. Semiring a => a -> a : ()
liftV = Measured

-- | add values with measures
add :: ∀ m v. Semiring v => Measured v m -> Measured v m -> Measured v m
add (Measured a) (Measured b) = Measured (a `Prelude.add` b)

infixl 6 add as ++

-- | subtract values with measures
sub :: ∀ v m. Ring v => Measured v m -> Measured v m -> Measured v m
sub (Measured a) (Measured b) = Measured (a `Prelude.sub` b)

infixl 6 sub as -*

zeroV :: ∀ v. Semiring v => Measured v ()
zeroV = liftV Prelude.zero

oneV :: ∀ v. Semiring v => Measured v ()
oneV = liftV Prelude.one

-- Syntactic Sugar

-- | "Multiply" Measures: `(Meter P1 * Sec N2 * ()) ≡ (Meter P1 (Sec N2 ()))`
-- |
-- | Note: The last `*` can be omitted `Sec N2 * () ≡ Sec N2 ()`
infixr 4 type RowApply as *

-- | Add Unit to a value: `(Int : Meter P1 ()) ≡ Measured Int (Meter P1 ())`
infix 3 type Measured as :

-- Show
class ShowMeasure (m :: Measure) where
  showMeasure :: MProxy m -> String

unicodeExponents :: Array String
unicodeExponents = [ "⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹" ]

negExp :: String
negExp = "⁻"

constructExp :: Int -> String
constructExp x
  | x >= 0 = constructPosExp x

constructExp x = negExp <> (constructPosExp (-x))

constructPosExp :: Int -> String
constructPosExp x = case (index unicodeExponents x) of
  Nothing -> constructPosExp (x `div` 10) <> constructPosExp (x `mod` 10)
  Just exp -> exp

data ShowRow (r :: RowList)

instance showRowNil :: Show (ShowRow Nil) where
  show _ = ""

instance showRowCons ::
  (ShowMeasure ty, Show (ShowRow tail), IsInt exp) =>
  Show (ShowRow (Cons sym (MeasureExp ty exp) tail)) where
  show _ = showMeasure (undefined :: MProxy ty) <> (constructExp $ reflectInt (undefined :: IProxy exp)) <> (show (undefined :: ShowRow tail))

instance showMeasured :: (Show v, RowToList r rl, Show (ShowRow rl)) => Show (Measured v r) where
  show (Measured v) = show v <> appendix
    where
    showUnit = show (undefined :: ShowRow rl)

    appendix = if showUnit == "" then "" else "·" <> showUnit
