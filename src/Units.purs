module Data.Type.Units where

import Data.Type.Numbers
import Prelude
import Type.Prelude

import Prim.Row as Row
import Prim.RowList (kind RowList, Cons, Nil)
import Type.Data.RowList (RLProxy(..))
import Type.Row (RowApply)


foreign import undefined :: ∀a. a


data Measured v (u :: # Type) = Measured v

foreign import kind Measure
data MProxy (m :: Measure)
data MeasureExp (m :: Measure) (exp :: Int)



class InsertAddRowList (list :: RowList) (sym :: Symbol) ty (result :: RowList) | list sym ty -> result

instance insertAddSame ∷ (Sum exp exp2 sum) => InsertAddRowList (Cons sym (MeasureExp ty exp) tail) sym (MeasureExp ty2 exp2) (Cons sym (MeasureExp ty sum) tail)
else
instance insertAddDifferenct ∷ (InsertAddRowList tail sym2 ty2 tail') => InsertAddRowList (Cons sym1 ty1 tail) sym2 ty2 (Cons sym1 ty1 tail')

instance insertNew :: InsertAddRowList Nil sym (MeasureExp m exp) (Cons sym (MeasureExp m exp) Nil)

insert :: ∀a b c ty. (InsertAddRowList a b ty c) => RLProxy a -> SProxy b -> ty -> RLProxy c
insert a b = undefined


class AddRowLists (a :: RowList) (b :: RowList) (sum :: RowList) | a b -> sum

instance addRowListCons :: (InsertAddRowList other sym ty other', AddRowLists tail other' result) => AddRowLists (Cons sym ty tail) other result

instance addRowListNil :: AddRowLists Nil other other

addRowLists :: ∀a b sum. (AddRowLists a b sum) => RLProxy a -> RLProxy b -> RLProxy sum
addRowLists a b = undefined

class AddRows (a :: # Type) (b :: # Type) (sum :: #Type) | a b -> sum

instance rowToRowList :: (AddRowLists ra rb rc, RowToList a ra, RowToList b rb, RowToList c rc, ListToRow ra a, ListToRow rb b, ListToRow rc c) => AddRows a b c

addRows :: ∀a b sum. (AddRows a b sum) => RProxy a -> RProxy b -> RProxy sum
addRows a b = undefined


mult :: ∀a b c v. AddRows a b c => Semiring v => Measured v a -> Measured v b -> Measured v c
mult (Measured a) (Measured b) = Measured (a*b)

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