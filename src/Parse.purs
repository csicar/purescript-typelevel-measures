module Data.Type.Units.Parse where

import Data.Type.Units


class SymbolToMeasure (sym :: Symbol) (m :: Measure)

class SymbolToMeasureExp (sym :: Symbol) m