module Data.Type.Units where

import Prelude
import Type.Prelude

import Prim.RowList (kind RowList, Cons, Nil)
import Type.Data.RowList (RLProxy(..))


foreign import undefined :: ∀a. a

-- Nats and Ints
foreign import kind Nat
foreign import data Z :: Nat
foreign import data Succ :: Nat -> Nat

data NProxy (n :: Nat)

foreign import kind Int
foreign import data Pos :: Nat -> Int
foreign import data Neg :: Nat -> Int

data IProxy (i :: Int)

type P0 = Pos Z
type P1 = Pos (Succ Z)
type P2 = Pos (Succ (Succ Z))
type P3 = Pos (Succ (Succ (Succ Z)))
type P4 = Pos (Succ (Succ (Succ (Succ Z))))
type P5 = Pos (Succ (Succ (Succ (Succ (Succ Z)))))

type N0 = Neg Z
type N1 = Neg (Succ Z)
type N2 = Neg (Succ (Succ Z))
type N3 = Neg (Succ (Succ (Succ Z)))

class NatToValue (a :: Nat) where
  natToValue :: NProxy a -> Int

instance toIntZ ∷ NatToValue Z where
  natToValue _ = 0

instance toIntSucc ∷ NatToValue a => NatToValue (Succ a) where
  natToValue _ = 1 + (natToValue (undefined :: NProxy a))

class IntToValue (a :: Int) where
  intToValue :: IProxy a -> Int

instance toIntPos :: NatToValue a => IntToValue (Pos a) where
  intToValue _ = natToValue (undefined :: NProxy a)

instance toIntNeg ∷ NatToValue a => IntToValue (Neg a) where
  intToValue _ = - natToValue (undefined :: NProxy a)

instance showZ ∷ Show (NProxy Z) where
  show a = show $ natToValue a


instance showSucc ∷ (NatToValue a) => Show (NProxy (Succ a)) where
  show a = show $ natToValue a

instance showPos ∷ (NatToValue a) => Show (IProxy (Pos a)) where
   show a = show $ intToValue a

instance showNeg ∷ (NatToValue a) => Show (IProxy (Neg a)) where
  show a = show $ intToValue a




-- Addition

class Sum (a :: Int) (b :: Int) (c :: Int) | a b -> c


-- (+ S a) + (+ b) = (+ S (a + b))
instance addPos :: (Sum (Pos a) (Pos b) (Pos c')) =>  Sum (Pos (Succ a)) (Pos b) (Pos (Succ c'))

-- (Pos (Succ a)) + (Neg (Succ b)) = a+b
instance addPosNegSucc ∷ (Sum (Pos a) (Neg b) c) => Sum (Pos (Succ a)) (Neg (Succ b)) c

instance addNegPosSucc :: (Sum (Neg a) (Pos b) c) => Sum (Neg (Succ a)) (Pos (Succ b)) c

instance addNegSucc ∷ (Sum (Neg a) (Neg b) (Neg c')) => Sum (Neg (Succ a)) (Neg b) (Neg (Succ c'))

instance addPosZ :: Sum (Pos Z) (Pos b) (Pos b)
instance addNegZ :: Sum (Neg Z) (Neg b) (Neg b)
instance addNegPosZ :: Sum (Neg (Succ a)) (Pos Z) (Neg (Succ a))
instance addPosNegZ :: Sum (Pos Z) (Neg (Succ b)) (Neg (Succ b))
instance addZZ :: Sum (Neg Z) (Pos Z) (Pos Z)
instance addZZ' :: Sum (Pos Z) (Neg Z) (Pos Z)

plus :: ∀a b c. Sum a b c => IProxy a -> IProxy b -> IProxy c
plus _ _ = (undefined :: IProxy c)

zero :: IProxy P0
zero = undefined

one :: IProxy P1
one = undefined

minusOne :: IProxy N1
minusOne = undefined

data Measured v (u :: # Type) = Measured v

foreign import kind Measure
data MProxy (m :: Measure)
data MeasureExp (m :: Measure) (exp :: Int)

foreign import data MeterT :: Measure
type Meter r = (meter :: MeasureExp MeterT P1 | r)
foreign import data SecT :: Measure
type Sec r = (sec :: MeasureExp SecT P1 | r)

class InsertAddRowList (list :: RowList) (sym :: Symbol) ty (result :: RowList) | list sym ty -> result

-- instance insertAddSame ∷ (Sum ty ty2 sum) => InsertAddRowList (Cons sym ty tail) sym ty2 (Cons sym sum tail)
instance insertAddSame ∷ (Sum exp exp2 sum) => InsertAddRowList (Cons sym (MeasureExp ty exp) tail) sym (MeasureExp ty2 exp2) (Cons sym (MeasureExp ty sum) tail)
else
-- instance insertAddDifferenct ∷ (InsertAddRowList tail sym2 ty2 tail') => InsertAddRowList (Cons sym1 ty1 tail) sym2 ty2 (Cons sym1 ty1 tail')
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
