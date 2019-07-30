Purescript Typelevel Units
==========================


Library for typelevel units of measure

### Features:

- Automatic Simplification of Type-Terms
- Clean Type-Syntax
- Inferrence of the Unit of a Term

### Example:

```purescript
import Data.Type.Units
import Data.Type.Units.SI
import Data.Type.Numbers

distance :: Int : Meter P1 ()
distance = liftV 10 ** meter

> distance -- prints:
10·m¹

avgSpeed :: Int : Meter P1 () -> Int : Sec P1 () -> Int : Meter P1 * Sec N1 ()
avgSpeed a b = a // b

speedOver10m :: Int : Meter P1 * Sec N1 ()
speedOver10m = avgSpeed distance (liftV 5 ** sec)

> speedOver10m  -- prints:
2·m¹s⁻¹

energyInBarOfChocolate :: Int : Joule ()
energyInBarOfChocolate = 2_300_000 ** joule

> > energyInBarOfChocolate -- prints:
2300000·kg¹m²s⁻²

forceOver5Meter :: Int : Newton ()
forceOver5Meter = energyInBarOfChocolate // (liftV 5 ** meter)

> forceOver5Meter -- prints:
460000·kg¹m¹s⁻²

> :t forceOver5Meter ** liftV 5 ** meter // sec -- what type does it have?
Int : Kg P1 * Meter P2 * Sec N3 * () -- without Type aliases:
Measured Int                                           
  ( kg :: MeasureExp KgT (Pos (Succ Z))                
  , meter :: MeasureExp MeterT (Pos (Succ (Succ Z)))   
  , sec :: MeasureExp SecT (Neg (Succ (Succ (Succ Z))))
  )


```