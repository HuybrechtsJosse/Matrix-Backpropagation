{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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
scalar `actL` f = \l r -> f (scalar `times` l) r

actLPointwise :: Num d => d -> (d -> d -> d) -> (d -> d -> d)
scalar `actLPointwise` f = \l r -> f (scalar * l) r

actR :: Semiring d => (d -> d -> d) -> d -> (d -> d -> d)
f `actR` scalar = \l r -> f l (r `times` scalar)

actRPointwise :: Num d => (d -> d -> d) -> d -> (d -> d -> d)
f `actRPointwise` scalar = \l r -> f l (r * scalar)

{-|
Datatype for Non-Cummutative Reverse AD
-}
data NCDualR d = NCD {fstNCD :: d , sndNCD :: d -> d -> d}

instance (Num d, Semiring d, Transposable d d) => Num (NCDualR d) where
    (+)                = plus
    (*)                = times
    fromInteger i      = NCD (fromInteger i) (const $ const zero)
    negate (NCD f df)  = NCD (negate f) (negate one `actL` df)
instance (Fractional d, Semiring d, Transposable d d) => Fractional (NCDualR d) where
    fromRational r            = NCD (fromRational r) (const $ const zero)
    (/) (NCD f df) (NCD g dg) = NCD (f / g) (((1/g) `actLPointwise` df) `ddotplus` ((-f/(g*g)) `actLPointwise` dg))
instance (Floating d, Semiring d, Transposable d d) => Floating (NCDualR d) where
    exp (NCD f df) = NCD (exp f) (exp f `actLPointwise` df)
    log (NCD f df) = NCD (log f) ((1/f) `actLPointwise` df)

instance (Norm d, Semiring d, Transposable d d, Num d) => Norm (NCDualR d) where
    norm x = 1 `times` x
instance Transposable d d => Transposable (NCDualR d) (NCDualR d) where
    tr  (NCD f df) = NCD (tr  f) (\l r -> df (tr l) (tr r))

instance (Semiring d, Transposable d d, Num d) => Semiring (NCDualR d) where
    zero = NCD zero (const $ const zero)
    one  = NCD one (const $ const zero)
    plus (NCD f df) (NCD g dg) = NCD (f `plus` g) (df `ddotplus` dg)
    times (NCD f df) (NCD g dg) = NCD (f `times` g) ((df `actR` tr g) `ddotplus` (tr f `actL` dg))


{-|
@reverseADNC env x e@ is a function to perform non commutative reverse AD to compute the derivative of @e@ to @x@ with values given in @env@.
 -}
reverseADNC :: (Eq v, Semiring d, Floating d, Transposable d d, Norm d) => (v -> d) -> v -> Expr v -> NCDualR d
reverseADNC env x e = let gen y = NCD (env y) (if x == y then reprB one else reprB zero)
                  in eval gen e