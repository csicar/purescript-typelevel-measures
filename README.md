Purescript Typelevel Units
==========================


Library for typelevel units of measure

### Features:

- Automatic simplification of type-terms
- Inference of types of units of measure
- Extensible to new base units

### Example:

```purescript
import Data.Type.Units
import Data.Type.Units.SI
import Data.Type.Numbers

distance :: Int : Meter' ()
distance = liftV 10 ** meter

> distance -- prints:
10·m¹

avgSpeed :: Int : Meter' () -> Int : Sec' () -> Int : Meter' * Sec' * ()
avgSpeed a b = a // b

speedOver10m :: Int : Meter' * Sec N1 ()
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

### Build

```bash
$ psc-package install
$ pulp --psc-package build
$ pulp --psc-package test
```

### Now it works

The Units are encoded as a Row of BaseUnits (called `kind Measure`) with its exponent (`kind Int`).

When multiplying two Units, the Rows are first sorted (using `ListToRow`) and then combined similar to this:

```haskell

combine (meter :: MeasureExp MeterT exp, meter :: MeasureExp MeterT exp2 | tail) = (meter :: MeasureExp MeterT (exp + exp2) | combine tail)
combine () = ()
```