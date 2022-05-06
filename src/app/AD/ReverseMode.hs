{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module AD.ReverseMode where

------ IMPORTS ------
import AD.ForwardMode (Semiring(..), Norm(..), Expr(..), eval, SDual (SP), act')
import Control.Monad (forM_)
import Data.Array.IO (Ix, IOArray, readArray, writeArray, newArray, getAssocs)
import Data.Map (Map, empty, toList, fromAscList, unionWith, singleton)
import Numeric.LinearAlgebra (Transposable(..))
import Prelude hiding ((++), (**))


------ DATA TYPES AND FUNCTIONS ------

-- Reverse mode AD
data DualR d = DR {fstDR :: d , sndDR :: d -> d}

instance Num d => Num (DualR d)
instance Fractional d => Fractional (DualR d)
instance Floating d => Floating (DualR d)

instance Norm d => Norm (DualR d)
instance Transposable d d => Transposable (DualR d) (DualR d)

instance Semiring d => Semiring (DualR d) where
    zero                         = DR zero (const zero)
    one                          = DR one (const zero)
    (DR f df) `plus` (DR g dg)   = DR (f `plus` g) (df `dotplus3` dg)
    (DR f df) `times` (DR g dg)  = DR (f `times` g) ((g `act2` df) `dotplus3` (f `act2` dg))

reprL :: Semiring d => d -> (d -> d)
reprL = \ n m -> m `times` n

absL :: Semiring d => (d -> d) -> d
absL = \ f -> f one

act2 :: Semiring d => d -> (d -> d) -> (d -> d)
m `act2` f = \ n -> f (n `times` m)

dotplus3 :: Semiring d => (d -> d) -> (d -> d) -> (d -> d)
f1 `dotplus3` f2 = \ n -> f1 n `plus` f2 n

reverseAD :: (Eq v, Semiring d, Floating d, Transposable d d, Norm d) => (v -> d) -> v -> Expr v -> DualR d
reverseAD env x e = let gen y = DR (env y) (if x == y then id else const zero)
                  in eval gen e


reprLM :: (Semiring d, Ord v) => Map v d -> (d -> Map v d)
reprLM = \m n -> n `act'` m

absLM :: (Semiring d) => (d -> Map v d) -> Map v d
absLM = \f -> f one

act2' :: (Ord v, Semiring d) => d -> (d -> Map v d) -> (d -> Map v d)
m `act2'` f = \n -> f (n `times` m)

dotplusM :: (Ord v, Semiring d) => (d -> Map v d) -> (d -> Map v d) -> (d -> Map v d)
f1 `dotplusM` f2 = \n -> unionWith plus (f1 n) (f2 n)


{- 
Data type for CDual
Intsances: Num, Fractional, Floating, Norm, Transposable, Semiring
-}
data CDual v d = C {fstC :: d, sndC :: d -> Map v d}

instance (Num d, Ord v, Semiring d, Transposable d d) => Num (CDual v d) where
    (+)              = plus
    (*)              = times
    abs (C f df)     = C (abs f) (\n -> df (abs n))
    signum (C f df)  = C (signum f) (\n -> df (signum n))
    fromInteger i    = C (fromInteger i) (const empty)
    negate (C f df)  = C (negate f) (\n -> df (negate n))

instance (Fractional d, Ord v, Semiring d, Transposable d d) => Fractional (CDual v d) where
    (/) (C f df) (C g dg) = C (f / g)  (\n -> unionWith (-) ((\m -> df (m / g)) n) ((\m -> dg (m `times` f / g / g)) n))

instance (Floating d, Ord v, Semiring d, Transposable d d) => Floating (CDual v d) where
    exp (C f df)  = C (exp f) (\n -> df (exp f `times` n))
    log (C f df)  = C (log f) (\n -> df (n / f))

instance (Norm d, Ord v, Semiring d) => Norm (CDual v d) where
    norm (C f df) = C (norm f) (\n -> fmap norm (df n))

instance Transposable d d => Transposable (CDual v d) (CDual v d) where
    tr   (C f df) = C (tr   f) (\n -> fmap tr   (df n))
    tr'  (C f df) = C (tr'  f) (\n -> fmap tr'  (df n))

instance (Ord v, Semiring d, Transposable d d) => Semiring (CDual v d) where
    zero                       = C zero (const empty)
    one                        = C one  (const empty)
    (C f df) `plus` (C g dg)   = C (f `plus` g) (df `dotplusM` dg)
    (C f df) `times` (C g dg)  = C (f `times` g) ((g `act2'` df) `dotplusM` (f `act2'` dg))

reprDual :: (Ord v, Semiring d) => SDual v d -> CDual v d
reprDual (SP f df) = C f (reprLM df)

absDual :: (Ord v, Semiring d) => CDual v d ->  SDual v d
absDual (C f df) = SP f (absLM df)

reverseADSparse :: (Ord v, Semiring d, Floating d, Transposable d d, Norm d) => (v -> d) -> Expr v -> CDual v d
reverseADSparse env e = let genRev x = C (env x) (singleton x)
                  in eval genRev e

{- 
Data type for CDual'
Intsances: Num, Fractional, Floating, Norm, Transposable, Semiring
-}
data CDual' v d = C' {fstC' :: d, sndC' :: d -> Map v d -> Map v d}

instance (Num d, Ord v, Semiring d) => Num (CDual' v d) where
    (+)               = plus
    (*)               = times
    abs (C' f df)     = C' (abs f) (\n -> df (abs n))
    signum (C' f df)  = C' (signum f) (\n -> df (signum n))
    fromInteger i     = C' (fromInteger i) (const (const empty))
    negate (C' f df)  = C' (negate f) (\n -> df (negate n))

instance (Fractional d, Ord v, Semiring d) => Fractional (CDual' v d)where
    fromRational r           = C' (fromRational r) (const (const empty))
    (/) (C' f df) (C' g dg)  = C' (f / g)  (\n -> dg (- f * n / g / g) . df (n * g / g / g))

instance (Floating d, Ord v, Semiring d) => Floating (CDual' v d)where
    exp (C' f df) = C' (exp f) (\n -> df (exp f * n))
    log (C' f df) = C' (log f) (\n -> df (n / f))

instance Norm d => Norm (CDual' v d) where
    norm (C' f df) = C' (norm f) (\n -> df (norm n))

instance Transposable d d => Transposable (CDual' v d) (CDual' v d) where
    tr   (C' f df) = C' (tr  f) (\n -> df (tr   n))
    tr'  (C' f df) = C' (tr' f) (\n -> df (tr'  n))

instance (Ord v, Semiring d) => Semiring (CDual' v d) where
    zero                         = C' zero (const id)
    one                          = C' one (const id)
    (C' f df) `plus` (C' g dg)   = C' (f `plus` g) (\n -> dg n . df n)
    (C' f df) `times` (C' g dg)  = C' (f `times` g) (\n -> dg (n `times` f) . df (n `times` g))

-- function to transform CDual to CDual'
reprSC :: (Ord v, Semiring d) => CDual v d -> CDual' v d
reprSC (C f df) = C' f (\n m -> unionWith plus m (df n))

-- function to transform CDual' to CDual
absSC :: (Ord v, Semiring d) => CDual' v d -> CDual v d
absSC (C' f df) = C f (\n -> df n empty)

insertWith :: Ord v => (d -> d -> d) -> v -> d -> Map v d -> Map v d
insertWith f x n m = unionWith f m (singleton x n)

-- | Function for optimised reverse AD with cayley representation
reverseADCayley :: (Ord v, Semiring d, Floating d, Norm d, Transposable d d) => (v -> d) -> Expr v -> CDual' v d
reverseADCayley env e = let genCayley x = C' (env x) (\n m -> insertWith plus x n m)
                        in eval genCayley e

{-| 
Data type for CDualIO
Intsances: Num, Fractional, Floating, Norm, Transposable, Semiring
-}
data CDualIO d = M {fstM :: d, sndM :: d -> IO ()}

instance (Num d, Semiring d) => Num (CDualIO d) where
    (+)              = plus
    (*)              = times
    abs (M f df)     = M (abs f) (\n -> df (abs n))
    signum (M f df)  = M (signum f) (\n -> df (signum n))
    fromInteger i    = M (fromInteger i) (\n -> return ())
    negate (M f df)  = M (negate f) (\n -> df (negate n))

instance (Fractional d, Semiring d) => Fractional (CDualIO d) where
    fromRational r         = M (fromRational r) (\n -> return ())
    (/) (M f df) (M g dg)  = M (f / g) (\n -> df (- n / g) >> dg (n * f / g / g))

instance (Floating d, Semiring d) => Floating (CDualIO d) where
    exp (M f df)  = M (exp f) (\n -> df (exp f * n))
    log (M f df)  = M (log f) (\n -> df (n / f))

instance Norm d => Norm (CDualIO d) where
    norm (M f df) = M (norm f) (\n -> df (norm n))

instance Transposable d d => Transposable (CDualIO d) (CDualIO d) where
    tr   (M f df)  = M (tr   f) (\n -> df (tr   n))
    tr'  (M f df)  = M (tr'  f) (\n -> df (tr'  n))

instance Semiring d => Semiring (CDualIO d) where
    zero                       = M zero (\n -> return ())
    one                        = M one (\n -> return ())
    (M f df) `plus` (M g dg)   = M (f `plus` g) (\n -> df n >> dg n)
    (M f df) `times` (M g dg)  = M (f `times` g) (\n -> df (n `times` g) >> dg (n `times` f))

-- | Function to transform CDual' to CDualIO
reprIO :: (Semiring d, Ix v, Ord v) => IOArray v d -> CDual' v d -> CDualIO d
reprIO arr (C' f df ) = let df' = \n -> do let m = df n empty
                                           forM_ (toList m) (\(v, a) ->
                                            do b <- readArray arr v
                                               writeArray arr v (b  `plus` a))
                        in M f df'

-- | Function to transform CDualIO to IO(SDual).
absIO :: (Semiring d, Ix v, Ord v) => (v, v) -> (IOArray v d -> CDualIO d) -> IO (SDual v d)
absIO rng d = do arr <- newArray rng zero
                 let (M f df) = d arr
                 df one
                 l <- getAssocs arr
                 let m = fromAscList l
                 return (SP f m)

-- | Function to perform optimised reverse AD with mutable arrays.
reverseADIO :: (Semiring d, Ix v, Floating d, Norm d, Transposable d d) => (v, v) -> (v -> d) -> Expr v -> IO (SDual v d)
reverseADIO rng env e = absIO rng (\arr ->
    let genIO x = M (env x) (\a -> do b <- readArray arr x
                                      writeArray arr x (b `plus` a))
    in eval genIO e)