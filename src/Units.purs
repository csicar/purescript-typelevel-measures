module Data.Type.Units where

import Data.Type.Numbers
import Type.Prelude

import Prelude (class EuclideanRing, class Semiring, class Show, show, ($), (<>), (/))
import Prelude as Prelude
import Prim.Row as Row
import Prim.RowList (kind RowList, Cons, Nil)
import Type.Data.RowList (RLProxy(..))
import Type.Row (RowApply)


foreign import undefined :: ∀a. a


data Measured v (u :: # Type) = Measured v

foreign import kind Measure
data MProxy (m :: Measure)
data MeasureExp (m :: Measure) (exp :: Int)



-- combines two sequential same symbols by adding their values
class Combine (original :: RowList) (combined :: RowList) | original -> combined


instance combineSame ∷ (Sum exp1 exp2 exp3, Combine tail tail') => Combine (Cons sym (MeasureExp m exp1) (Cons sym (MeasureExp m exp2) tail)) (Cons sym (MeasureExp m exp3) tail')
else
instance combineDifferent :: (Combine rest rest') => Combine (Cons sym1 (MeasureExp m1 exp1) rest) (Cons sym1 (MeasureExp m1 exp1) rest')

instance combineNil :: Combine Nil Nil


combine :: ∀a b. (Combine a b) => RLProxy a -> RLProxy b
combine = undefined



class AddRows (a :: # Type) (b :: # Type) (sum :: #Type) | a b -> sum

instance addRowsCombine :: (Union a b sum, RowToList sum rsum, Combine rsum resultList, ListToRow resultList result) => AddRows a b result

addRows :: ∀a b sum. (AddRows a b sum) => RProxy a -> RProxy b -> RProxy sum
addRows a b = undefined


class InverseRowList (original :: RowList) (inverted :: RowList) | original -> inverted, inverted -> original

instance inverseNil ∷ InverseRowList Nil Nil

instance inverseCons :: (InverseRowList tail inverseTail, Inverse ty inverseTy) => InverseRowList (Cons sym (MeasureExp m ty) tail) (Cons sym (MeasureExp m inverseTy) inverseTail)


class InverseRow (original :: # Type) (inverted :: # Type) | inverted -> original

instance inverseRowList ∷ (RowToList original originalList, InverseRowList originalList resultList, ListToRow resultList result) =>  InverseRow original result

-- Methods

mult :: ∀a b c v. AddRows a b c => Semiring v => Measured v a -> Measured v b -> Measured v c
mult (Measured a) (Measured b) = Measured (a `Prelude.mul` b)

infixl 7 mult as **

divide :: ∀a b c v ib. InverseRow b ib => AddRows a ib c => EuclideanRing v => Measured v a -> Measured v b -> Measured v c
divide (Measured a) (Measured b) = Measured (a / b)

infixl 7 divide as //

const :: ∀a. Semiring a => a -> a : ()
const = Measured

-- Syntactic Sugar

infixr 4 type RowApply as *

infix 3 type Measured as :

-- Show
class ShowMeasure (m :: Measure) where
  showMeasure :: MProxy m -> String

data ShowRow (r :: RowList)

instance showRowNil :: Show (ShowRow Nil) where
  show _ = ""

instance showRowCons :: (ShowMeasure ty, Show (ShowRow tail), IntToValue exp) 
  => Show (ShowRow (Cons sym (MeasureExp ty exp) tail)) where
  show _ = showMeasure (undefined :: MProxy ty) <> (constructExp $ intToValue (undefined :: IProxy exp)) <> (show (undefined :: ShowRow tail))

instance showMeasured :: (Show v, RowToList r rl, Show (ShowRow rl)) => Show (Measured v r) where
   show (Measured v) = show v <> "·" <> show (undefined :: ShowRow rl)