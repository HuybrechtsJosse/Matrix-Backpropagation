{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module AD.NCReverseMode where

------ IMPORTS ------
import AD.ForwardMode (Semiring(..), Norm(..), Expr(..), eval)
import Data.Array.IO (Ix)
import Numeric.LinearAlgebra (Transposable (..))
import Prelude hiding ((++), (**))

reprB :: Semiring d => d -> (d -> d -> d)
reprB n = \l r -> l `times` n `times` r

absB :: Semiring d => (d -> d -> d) -> d
absB f = f one one

ddotplus :: Semiring d => (d -> d -> d) -> (d -> d -> d) -> (d -> d -> d)
f `ddotplus` g = \l r -> f l r `plus` g l r

actL :: Semiring d => d -> (d -> d -> d) -> (d -> d -> d)
x `actL` f = \l r -> f (x `times` l) r

actPointwise :: Num d => d -> (d -> d -> d) -> (d -> d -> d)
x `actPointwise` f = \l r -> f (x * l) r

actR :: Semiring d => (d -> d -> d) -> d -> (d -> d -> d)
f `actR` x = \l r -> f l (r `times` x)

{-|
Datatype for dual numbers for Non-Cummutative Reverse AD
-}
data NCDualR d = NCD {fstNCD :: d , sndNCD :: d -> d -> d}

instance (Num d, Semiring d, Transposable d d) => Num (NCDualR d) where
    (+)                = plus
    (*)                = times
    fromInteger i      = NCD (fromInteger i) (const $ const zero)
    negate (NCD f df)  = NCD (negate f) (\l r -> df (negate l) r)

instance (Fractional d, Semiring d, Transposable d d) => Fractional (NCDualR d) where
    fromRational r            = NCD (fromRational r) (const $ const zero)
    (/) (NCD f df) (NCD g dg) = NCD (f / g) (((1/g) `actPointwise` df) `ddotplus` (negate (f/(g*g)) `actPointwise` dg))

instance (Floating d, Semiring d, Transposable d d) => Floating (NCDualR d) where
    exp (NCD f df) = NCD (exp f) (exp f `actPointwise` df)
    log (NCD f df) = NCD (log f) ((1/f) `actPointwise` df)

instance (Norm d, Semiring d, Transposable d d, Num d) => Norm (NCDualR d) where
    norm (NCD f df) = NCD (norm f) (1 `actL` df)

instance Transposable d d => Transposable (NCDualR d) (NCDualR d) where
    tr  (NCD f df) = NCD (tr  f) (\l r -> df (tr r) (tr l))

instance (Semiring d, Transposable d d) => Semiring (NCDualR d) where
    zero = NCD zero (const $ const zero)
    one  = NCD one (const $ const zero)
    plus (NCD f df) (NCD g dg) = NCD (f `plus` g) (df `ddotplus` dg)
    times (NCD f df) (NCD g dg) = NCD (f `times` g) ((df `actR` tr g) `ddotplus` (tr f `actL` dg))


{-|
@reverseADNC env x e@ is a function to perform non commutative reverse AD to compute the derivative of @e@ to @x@ with values given in @env@.
 -}
reverseADNC :: (Eq v, Semiring d, Floating d, Transposable d d, Norm d) => (v -> d) -> v -> Expr v -> NCDualR d
reverseADNC env x e = let gen y = NCD (env y) (if x == y then reprB one else const $ const zero)
                  in eval gen e