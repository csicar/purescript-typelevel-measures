module Data.Type.Numbers where

import Prelude
import Type.Prelude

import Data.Array (index)
import Data.Maybe (Maybe(..))
import Prim.Row as Row
import Prim.RowList (kind RowList, Cons, Nil)
import Prim.Symbol as Symbol
import Type.Data.Boolean (class If)
import Type.Data.RowList (RLProxy(..))
import Type.Data.Symbol (class Equals)

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

type P0  = Pos Z
type P1  = Pos (Succ Z)
type P2  = Pos (Succ (Succ Z))
type P3  = Pos (Succ (Succ (Succ Z)))
type P4  = Pos (Succ (Succ (Succ (Succ Z))))
type P5  = Pos (Succ (Succ (Succ (Succ (Succ Z)))))
type P6  = Pos (Succ (Succ (Succ (Succ (Succ (Succ Z))))))
type P7  = Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))
type P8  = Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))
type P9  = Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))
type P10 = Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))))

type N0 = Neg Z
type N1 = Neg (Succ Z)
type N2 = Neg (Succ (Succ Z))
type N3 = Neg (Succ (Succ (Succ Z)))
type N4 = Neg (Succ (Succ (Succ (Succ Z))))

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

-- Inverse

class Inverse (a :: Int) (b :: Int) | a -> b, b -> a

instance inversePosZ :: Inverse (Pos Z) (Pos Z)
else
instance inversePosSucc :: Inverse (Pos a) (Neg a)
else
instance inverseNegZ ∷ Inverse (Neg Z) (Pos Z) 
else
instance inverseNegSucc :: Inverse (Neg a) (Pos a)

plus :: ∀a b c. Sum a b c => IProxy a -> IProxy b -> IProxy c
plus _ _ = (undefined :: IProxy c)

zero :: IProxy P0
zero = undefined

one :: IProxy P1
one = undefined

minusOne :: IProxy N1
minusOne = undefined


-- Product
class Product (a :: Int) (b :: Int) (c :: Int) | a b -> c

instance productPosNeg :: Product (Pos a) (Pos b) (Pos c) => Product (Neg a) (Pos b) (Neg c)

instance productNegNeg :: Product (Pos a) (Pos b) (Pos c) => Product (Neg a) (Neg b) (Pos c)

instance productZ :: Product (Pos Z) a (Pos Z)
else
instance product1 :: Product (Pos (Succ Z)) a a
else
instance productNegPos :: Product (Pos a) (Pos b) (Pos c) => Product (Pos a) (Neg b) (Neg c)

else
-- (1 + a) * b = b + (a * b)
instance productSucc :: (Product (Pos a) b ab, Sum ab b result) => Product (Pos (Succ a)) b result

prod :: ∀a b c. Product a b c => IProxy a -> IProxy b -> IProxy c
prod _ _ = (undefined :: IProxy c)

-- Parsing
class ParseNumber (sym :: Symbol) (nat :: Nat) | nat -> sym, sym -> nat


-- a * 10 = -1
instance parseLit0 :: ParseNumber "0" Z
else
instance parseLit1 :: ParseNumber "1" (Succ Z)
else
instance parseLit2 :: ParseNumber "2" (Succ (Succ Z))
else
instance parseLit3 :: ParseNumber "3" (Succ (Succ (Succ Z)))
else
instance parseLit4 :: ParseNumber "4" (Succ (Succ (Succ (Succ Z))))
else
instance parseLit5 :: ParseNumber "5" (Succ (Succ (Succ (Succ (Succ Z)))))
else
instance parseLit6 :: ParseNumber "6" (Succ (Succ (Succ (Succ (Succ (Succ Z))))))
else
instance parseLit7 :: ParseNumber "7" (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))
else
instance parseLit8 :: ParseNumber "8" (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))
else
instance parseLit9 :: ParseNumber "9" (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))
else
instance parseCons :: (ParseNumber head msd, Symbol.Cons head tail sym, Product (Pos msd) P10 high, ParseNumber tail lower, Sum high (Pos lower) (Pos res)) => ParseNumber sym res

parseNat :: ∀a sym. ParseNumber sym a => SProxy sym -> NProxy a
parseNat _ = undefined

class ParseInt (sym :: Symbol) (int :: Int) | int -> sym, sym -> int

instance parseMinus :: 
   ( Equals "-" head isMinus
   , If isMinus (IProxy (Neg natValue)) (IProxy (Pos natValue)) (IProxy int)
   , If isMinus (SProxy tail) (SProxy sym) (SProxy numberSymbol)
   , Symbol.Cons head tail sym
   , ParseNumber numberSymbol natValue
   ) => ParseInt sym int


parseInt :: ∀a sym. ParseInt sym a => SProxy sym -> IProxy a
parseInt _ = undefined
