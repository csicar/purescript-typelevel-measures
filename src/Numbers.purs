module Data.Type.Numbers where

import Prelude
import Type.Prelude

import Data.Array (index)
import Data.Maybe (Maybe(..))
import Prim.Row as Row
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

unicodeExponents :: Array String
unicodeExponents = ["⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹"]

negExp :: String
negExp = "⁻"

constructExp :: Int -> String
constructExp x | x >= 0 = constructPosExp x
constructExp x = negExp <> (constructPosExp (-x))

constructPosExp :: Int -> String
constructPosExp x = case (index unicodeExponents x) of
   Nothing -> constructPosExp (x `div` 10) <> constructPosExp (x `mod` 10)
   Just exp -> exp

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
instance minusZ :: Sum (Pos a) (Neg Z) (Pos a)
instance minusZ' :: Sum (Neg Z) (Pos a) (Pos a)

plus :: ∀a b c. Sum a b c => IProxy a -> IProxy b -> IProxy c
plus _ _ = (undefined :: IProxy c)

zero :: IProxy P0
zero = undefined

one :: IProxy P1
one = undefined

minusOne :: IProxy N1
minusOne = undefined