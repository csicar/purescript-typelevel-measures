module Test where

import Prelude
import Type.Prelude

import Data.Type.Units (undefined)
import Prim.RowList (kind RowList, Cons, Nil)
import Type.Data.RowList (RLProxy(..))


-- class InsertOrderedRowList (list :: RowList) (sym :: Symbol) ty (result :: RowList) | list sym ty -> result

-- instance orderedNil ∷ InsertOrderedRowList Nil sym ty (Cons sym ty Nil)

-- instance same ∷ InsertOrderedRowList (Cons sym ty tail) sym ty (Cons sym ty tail)
-- else
-- instance different :: (InsertOrderedRowList tail sym ty inserted) => InsertOrderedRowList (Cons sym' ty' tail) sym ty (Cons sym' ty' inserted)



-- foreign import kind Measure

-- foreign import data Meter :: Measure
-- foreign import data Sec :: Measure


-- Nats and Ints
data Z
data Succ a

data Pos a
data Neg a

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

class ToInt a where
  toInt :: a -> Int

instance toIntZ ∷ ToInt Z where
  toInt _ = 0

instance toIntSucc ∷ ToInt a => ToInt (Succ a) where
  toInt _ = 1 + (toInt (undefined :: a))

instance toIntPos :: ToInt a => ToInt (Pos a) where
  toInt _ = toInt (undefined :: a)

instance toIntNeg ∷ ToInt a => ToInt (Neg a) where
  toInt _ = - toInt (undefined :: a)

instance showZ ∷ Show Z where
  show a = show $ toInt a

instance showSucc ∷ (ToInt a) => Show (Succ a) where
  show a = show $ toInt a

instance showPso ∷ (ToInt a) => Show (Pos a) where
   show a = show $ toInt a

instance showNeg ∷ (ToInt a) => Show (Neg a) where
  show a = show $ toInt a




-- Addition

class Sum a b c | a b -> c


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

plus :: ∀a b c. Sum a b c => a -> b -> c
plus _ _ = (undefined :: c)

zero :: Pos Z
zero = undefined

one :: Pos (Succ Z)
one = undefined

two :: Pos (Succ (Succ Z))
two = undefined

three :: Pos (Succ (Succ (Succ Z)))
three = undefined

minusOne :: Neg (Succ Z)
minusOne = undefined

minusTwo :: Neg (Succ (Succ Z))
minusTwo = undefined

--Tests

t1 :: Pos (Succ (Succ (Succ Z)))
t1 = one `plus` two


t1' :: Pos (Succ (Succ (Succ Z)))
t1' = two `plus` one

t2 :: Neg (Succ (Succ (Succ Z)))
t2 = minusTwo `plus` minusOne

t2' :: Neg (Succ (Succ (Succ Z)))
t2' = minusTwo `plus` minusOne

t3 :: Neg (Succ Z)
t3 = minusTwo `plus` one

t3' :: Neg (Succ Z)
t3' = one `plus` minusTwo

t4 :: Neg (Succ (Succ (Succ (Succ Z))))
t4 = minusTwo `plus` minusTwo

t5 :: Neg (Succ (Succ Z))
t5 = t4 `plus` two

t5' :: Neg (Succ (Succ Z))
t5' = two `plus` t4

t6 :: Neg (Succ (Succ Z))
t6 = two `plus` (minusTwo `plus` minusTwo)

t6' :: Neg (Succ (Succ Z))
t6' = (minusTwo `plus` minusTwo) `plus` two

t7 :: Pos Z
t7 = minusOne `plus` one

t7' :: Pos Z
t7' = one `plus` minusOne


data Measured v (u :: # Type) = Measured v

type Meter r = (meter :: Pos (Succ Z) | r)
type Sec r = (sec :: Pos (Succ Z) | r)

class InsertAddRowList (list :: RowList) (sym :: Symbol) ty (result :: RowList) | list sym ty -> result

instance insertAddSame ∷ (Sum ty ty2 sum) => InsertAddRowList (Cons sym ty tail) sym ty2 (Cons sym sum tail)
else
instance insertAddDifferenct ∷ (InsertAddRowList tail sym2 ty2 tail') => InsertAddRowList (Cons sym1 ty1 tail) sym2 ty2 (Cons sym1 ty1 tail')

instance insertNew :: InsertAddRowList Nil sym ty (Cons sym ty Nil)

insert :: ∀a b c ty. (InsertAddRowList a b ty c) => RLProxy a -> SProxy b -> ty -> RLProxy c
insert a b = undefined

t11 :: RLProxy Nil -> SProxy "meter" -> P1 -> RLProxy (Cons "meter" P1 Nil)
t11 = insert

t12 :: RLProxy (Cons "meter" P1 Nil) -> SProxy "meter" -> P1 -> RLProxy (Cons "meter" P2 Nil)
t12 = insert

t13 :: RLProxy (Cons "meter" P1 Nil) -> SProxy "meter" -> P3 -> RLProxy (Cons "meter" P4 Nil)
t13 = insert

t14 :: RLProxy (Cons "meter" P2 Nil) -> SProxy "meter" -> P3 -> RLProxy (Cons "meter" P5 Nil)
t14 = insert

t15 :: RLProxy (Cons "meter" P2 (Cons "sec" P1 Nil)) -> SProxy "meter" -> P3 -> RLProxy (Cons "meter" P5 (Cons "sec" P1 Nil))
t15 = insert

t16 :: RLProxy (Cons "meter" P1 (Cons "sec" P2 Nil)) -> SProxy "sec" -> P2 -> RLProxy (Cons "meter" P1 (Cons "sec" P4 Nil))
t16 = insert

t17 :: RLProxy (Cons "meter" P1 (Cons "sec" P2 Nil)) -> SProxy "sec" -> N3 -> RLProxy (Cons "meter" P1 (Cons "sec" N1 Nil))
t17 = insert



class AddRowLists (a :: RowList) (b :: RowList) (sum :: RowList) | a b -> sum

instance addRowListCons :: (InsertAddRowList other sym ty other', AddRowLists tail other' result) => AddRowLists (Cons sym ty tail) other result

instance addRowListNil :: AddRowLists Nil other other

addRowLists :: ∀a b sum. (AddRowLists a b sum) => RLProxy a -> RLProxy b -> RLProxy sum
addRowLists a b = undefined

t21 :: RLProxy (Cons "meter" P1 Nil) -> RLProxy (Cons "sec" P1 Nil) -> RLProxy (Cons "sec" P1 (Cons "meter" P1 Nil))
t21 = addRowLists

t22 :: RLProxy (Cons "meter" P2 Nil) -> RLProxy (Cons "sec" P1 Nil) -> RLProxy (Cons "sec" P1 (Cons "meter" P2 Nil))
t22 = addRowLists

t23 :: RLProxy (Cons "meter" P2 Nil) -> RLProxy (Cons "sec" P0 Nil) -> RLProxy (Cons "sec" P0 (Cons "meter" P2 Nil))
t23 = addRowLists

t24 :: RLProxy (Cons "meter" P2 Nil) -> RLProxy (Cons "meter" P2 Nil) -> RLProxy (Cons "meter" P4 Nil)
t24 = addRowLists

t25 :: RLProxy (Cons "meter" P2 Nil) -> RLProxy (Cons "meter" N3 Nil) -> RLProxy (Cons "meter" N1 Nil)
t25 = addRowLists

class AddRows (a :: # Type) (b :: # Type) (sum :: #Type) | a b -> sum

instance rowToRowList :: (AddRowLists ra rb rc, RowToList a ra, RowToList b rb, RowToList c rc, ListToRow ra a, ListToRow rb b, ListToRow rc c) => AddRows a b c

addRows :: ∀a b sum. (AddRows a b sum) => RProxy a -> RProxy b -> RProxy sum
addRows a b = undefined

t31 :: RProxy (meter :: P1) -> RProxy (sec :: P1) -> RProxy (sec :: P1, meter :: P1)
t31 = undefined

t32 :: RProxy (meter :: P1) -> RProxy (meter :: P1) -> RProxy (sec :: P1, meter :: P1)
t32 = undefined

mult :: ∀a b c v. AddRows a b c => Semiring v => Measured v a -> Measured v b -> Measured v c
mult (Measured a) (Measured b) = Measured (a*b)

m :: Measured Int (meter :: P1)
m = Measured 1

m2 :: Measured Int (meter :: N2, sec :: P1)
m2 = Measured 2

t :: Measured Int
  ( meter :: N1
  , sec :: P1
  )
t = m `mult` m2