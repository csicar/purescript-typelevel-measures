module Data.Type.Units where

import Effect (Effect)
import Effect.Console (log)
import Prelude (class Apply, class Semiring, class Show, Unit, one, otherwise, show, zero, (*), (<>))
import Prelude as P
import Prim.Row as Row
import Prim.RowList (kind RowList, Cons, Nil)
import Type.Prelude (class IsSymbol, class Union, class RowToList)
import Type.Row (RowApply, class Cons)
import Type.RowList (class RowListAppend)

main :: Effect Unit
main = do
  log "Hello sailor!"


class Fraction (c :: # Type ) (c'  :: # Type) (s :: # Type) (s' :: #Type) | c c' -> s s'

class FractionRl (c :: RowList) (c' :: RowList) (s :: RowList) (s' :: RowList) | c c' -> s s'

instance rowListToRow :: 
  ( FractionRl rc rc' rs rs'
  , RowToList c rc
  , RowToList c' rc'
  , RowToList s rs
  , RowToList s' rs'
  ) => Fraction c c' s s' 


instance fractionBoth :: (FractionRl above below above' below') 
  => FractionRl (Cons sym ty above) (Cons sym ty below) above' below'
else
instance fraction1 :: (FractionRl above below above' below')
  => FractionRl (Cons s1 t1 (Cons s1 t2 above)) (Cons s1 t2 below) (Cons s1 t1 above) below
else
instance fractionDoNothing :: FractionRl c c' c c'

type Apply t (f :: Type -> Type) = f t
infix 3 type  Apply as :
infixr 6 type RowApply as *

class Mul u (a :: # Type) (a' :: # Type) (b :: # Type) (b' :: #Type) (c :: # Type) (c' :: # Type) where
  mul :: Measured u a a' -> Measured u b b' -> Measured u c c'

instance nilMul :: (Semiring v, Union a b c, Union a' b' c', Fraction c c' s s') => Mul v a a' b b' s s' where
  mul (Measured a) (Measured b) = Measured (a*b)

mult = mul

oneM :: ∀v above below. Semiring v => Measured v above below
oneM = valOf one

-- | value of type v with unit above / below
data Measured v (above :: # Type) (below :: # Type) = Measured v

type PosMeasured v (u :: # Type) = Measured v u

type Divide (a :: # Type) (x :: # Type) v = Measured v a x
infixr 5 type Divide as /

data Measure (t :: RowList)


foreign import undefined :: ∀a. a

class ShowRow (t :: RowList) where
  showRow :: Measure t -> String

instance showNil :: ShowRow Nil where
  showRow _ = ""

instance showCons ∷ (Show ty, ShowRow rest) => ShowRow (Cons sym ty rest) where
  showRow t = (show (undefined :: ty)) <> str
    where
      str = case showRow (undefined :: Measure rest) of
        "" -> ""
        rest -> "*" <> rest


instance showMeasured :: (Show v, RowToList u u', RowToList d d', ShowRow u', ShowRow d') => Show (Measured v u d) where
  show (Measured v) = show v <> " " <> showRow (undefined :: Measure u') <> "/" <> showRow (undefined :: Measure d')

valOf :: ∀v (u :: # Type) (d :: # Type). v -> Measured v u d
valOf u = Measured u
