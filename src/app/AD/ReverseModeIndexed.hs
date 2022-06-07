{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module AD.ReverseModeIndexed where

------ IMPORTS ------
import AD.ForwardMode (Norm(..), Expr(..), eval, SDual (SP))
import Data.Array.IO (Ix, IOArray, readArray, writeArray, newArray, getAssocs)
import IndexedSemiring (IndexedSemiring(..), Indexed(..), evalIndexed, IndexedExpr)
import Prelude hiding ((++), (**))
import Numeric.LinearAlgebra (Transposable (..))
import Data.Map (Map, unionWith, singleton, empty, toList, fromAscList)
import Control.Monad (forM_)



reprBI :: IndexedSemiring d => d -> (d -> d -> d)
reprBI n = \l r -> l `times` n `times` r

absBI :: IndexedSemiring d => (d -> d -> d) -> d -> d
absBI f one = f one one

ddotplus :: IndexedSemiring d => (d -> d -> d) -> (d -> d -> d) -> (d -> d -> d)
f `ddotplus` g = \l r -> f l r `plus` g l r

actL :: IndexedSemiring d => d -> (d -> d -> d) -> (d -> d -> d)
x `actL` f = \l r -> f (x `times` l) r

actLPointwise :: Num d => d -> (d -> d -> d) -> (d -> d -> d)
x `actLPointwise` f = \l r -> f (x * l) r

actR :: IndexedSemiring d => (d -> d -> d) -> d -> (d -> d -> d)
f `actR` x = \l r -> f l (r `times` x)

actRPointwise :: Num d => (d -> d -> d) -> d -> (d -> d -> d)
f `actRPointwise` x = \l r -> f l (r * x)

{-|
Datatype for dual numbers for Indexed Reverse AD
-}
data IndexedDualR d = ID {fstID :: d , sndID :: d -> d -> d}

instance (Num d, Indexed d, IndexedSemiring d, Transposable d d) => Num (IndexedDualR d) where
    (+)               = plus
    (*)               = times
    negate (ID f df)  = ID (negate f) (\l r -> df (negate l) r)
instance (Fractional d, IndexedSemiring d, Transposable d d, Indexed d) => Fractional (IndexedDualR d) where
    (/) (ID f df) (ID g dg) = ID (f / g) (\l r -> df (l / g) r + dg (negate f / g / g * l) r)
instance (Floating d, IndexedSemiring d, Transposable d d, Indexed d) => Floating (IndexedDualR d) where
    exp (ID f df) = ID (exp f) (\l r -> df (exp f * l) r)
    log (ID f df) = ID (log f) (\l r -> df (l/f) r)

instance (Norm d, IndexedSemiring d, Transposable d d, Num d, Indexed d) => Norm (IndexedDualR d) where
    norm (ID f df) = ID (norm f) (oneNorm f `actL` df)

instance Transposable d d => Transposable (IndexedDualR d) (IndexedDualR d) where
    tr  (ID f df) = ID (tr  f) (\l r -> df (tr r) (tr l))

instance (IndexedSemiring d, Transposable d d, Num d) => IndexedSemiring (IndexedDualR d) where
    zero r c = ID (zero r c) (const $ const (zero r c))
    one r = ID (one r) (const $ const (zero r r))
    plus (ID f df) (ID g dg) = ID (f `plus` g) (df `ddotplus` dg)
    times (ID f df) (ID g dg) = ID (f `times` g) ((df `actR` tr g) `ddotplus` (tr f `actL` dg))

instance (IndexedSemiring d, Indexed d) => Indexed (IndexedDualR d) where
    rows (ID f _) = rows f
    cols (ID f _) = cols f
    fromInt i (ID f df) = ID (fromInt i f) (\l r -> df (zero (rows l) (cols l)) (zero (rows r) (cols r)))
{-|
@reverseADI env x e@ is a function to perform indexed reverse AD to compute the derivative of @e@ to @x@ with values given in @env@.
 -}
reverseADI :: (Eq v, IndexedSemiring d, Floating d, Transposable d d, Norm d, Indexed d) => (v -> d) -> v -> IndexedExpr v -> IndexedDualR d
reverseADI env x e = let gen y = ID (env y) (if x == y then reprBI (one 1) else const $ const (zero (rows $ env x) (cols $ env x)))
                  in evalIndexed gen e

{- 
Data type for IndexedCDual, dual number for indexed reverse AD with sparse maps
-}
data IndexedCDual v d = IC {fstC :: d, sndC :: d -> d -> Map v d}

instance (Num d, Ord v, IndexedSemiring d, Transposable d d) => Num (IndexedCDual v d) where
    (+)              = plus
    (*)              = times
    negate (IC f df)  = IC (negate f) (\l r -> df (negate l) r)

instance (Fractional d, Ord v, IndexedSemiring d, Transposable d d) => Fractional (IndexedCDual v d) where
    (/) (IC f df) (IC g dg) = IC (f / g)  (\l r -> unionWith (+) (df (l / g) r) (dg (negate f / g / g * l) r))

instance (Floating d, Ord v, IndexedSemiring d, Transposable d d) => Floating (IndexedCDual v d) where
    exp (IC f df)  = IC (exp f) (\l r -> df (exp f * l) r)
    log (IC f df)  = IC (log f) (\l r -> df (l / f) r)

instance (Norm d, Ord v, IndexedSemiring d) => Norm (IndexedCDual v d) where
    norm (IC f df) = IC (norm f) (\l r -> df (oneNorm f `times` l) r)

instance Transposable d d => Transposable (IndexedCDual v d) (IndexedCDual v d) where
    tr   (IC f df) = IC (tr  f) (\l r -> df (tr r) (tr l))

instance (Ord v, IndexedSemiring d, Transposable d d) => IndexedSemiring (IndexedCDual v d) where
    zero r c                     = IC (zero r c) (const $ const empty)
    one  r                       = IC (one r)  (const $ const empty)
    (IC f df) `plus` (IC g dg)   = IC (f `plus` g) (df `dotplusM` dg)
    (IC f df) `times` (IC g dg)  = IC (f `times` g) ((df `actR2'` tr g) `dotplusM` (tr f `actL2'` dg))

instance Indexed d => Indexed (IndexedCDual v d) where
    rows (IC f _) = rows f
    cols (IC f _) = cols f
    fromInt i (IC f _) = IC (fromInt i f) (const $ const empty)

dotplusM :: (Ord v, IndexedSemiring d) => (d -> d -> Map v d) -> (d -> d -> Map v d) -> (d -> d -> Map v d)
f1 `dotplusM` f2 = \l r -> unionWith plus (f1 l r) (f2 l r)

act' :: (Ord v, IndexedSemiring d) => d -> d -> Map v d -> Map v d
act' l r dg = fmap (\n -> l `times` n `times` r) dg

actL2' :: (Ord v, IndexedSemiring d) => d -> (d -> d -> Map v d) -> (d -> d -> Map v d)
m `actL2'` f = \l r -> f (m `times` l) r

actR2' :: (Ord v, IndexedSemiring d) => (d -> d -> Map v d) -> d -> (d -> d -> Map v d)
f `actR2'` m = \l r -> f l (r `times` m)

reprDualI :: (Ord v, IndexedSemiring d) => SDual v d -> IndexedCDual v d
reprDualI (SP f df) = IC f (reprBMI df)

absDualI :: (Ord v, IndexedSemiring d) => IndexedCDual v d -> SDual v d
absDualI (IC f df) = SP f (absBMI df (one 1))

reprBMI :: (Ord v, IndexedSemiring d) => Map v d -> (d -> d -> Map v d)
reprBMI m = \l r -> act' l r m

absBMI :: (d -> d -> Map v d) -> d -> Map v d
absBMI f one = f one one

-- | indexed reverse AD with sparse maps
reverseADSparseI :: (Ord v, IndexedSemiring d, Floating d, Transposable d d, Norm d, Indexed d) => (v -> d) -> IndexedExpr v -> IndexedCDual v d
reverseADSparseI env e = let genRev x = IC (env x) (\l r -> singleton x (l `times` r))
                  in evalIndexed genRev e

{- |
Data type for IndexedCDual', dual number for indexed reverse AD with cayley representation
-}
data IndexedCDual' v d = IC' {fstC' :: d, sndC' :: d -> d -> Map v d -> Map v d}

instance (Num d, Indexed d, IndexedSemiring d, Transposable d d) => Num (IndexedCDual' v d) where
    (+)               = plus
    (*)               = times
    negate (IC' f df)  = IC' (negate f) (\l r -> df (negate l) r)
instance (Fractional d, IndexedSemiring d, Transposable d d, Indexed d) => Fractional (IndexedCDual' v d) where
    (/) (IC' f df) (IC' g dg) = IC' (f / g) (\ l r -> dg (negate f/(g*g)*l) r . df (l/g) r)
instance (Floating d, IndexedSemiring d, Transposable d d, Indexed d) => Floating (IndexedCDual' v d) where
    exp (IC' f df) = IC' (exp f) (\ l r -> df (exp f * l) r)
    log (IC' f df) = IC' (log f) (\l r -> df (l/f) r)

instance (Norm d, IndexedSemiring d, Transposable d d, Num d, Indexed d) => Norm (IndexedCDual' v d) where
    norm (IC' f df) = IC' (norm f) (\l r -> df (oneNorm f `times` l) r)

instance Transposable d d => Transposable (IndexedCDual' v d) (IndexedCDual' v d) where
    tr  (IC' f df) = IC' (tr  f) (\l r -> df (tr r) (tr l))

instance (IndexedSemiring d, Transposable d d, Num d) => IndexedSemiring (IndexedCDual' v d) where
    zero r c = IC' (zero r c) (const $ const id)
    one r = IC' (one r) (const $ const id)
    plus (IC' f df) (IC' g dg) = IC' (f `plus` g) (\l r -> dg l r . df l r)
    times (IC' f df) (IC' g dg) = IC' (f `times` g) (\l r -> dg (tr f `times` l) r . df l (r `times` tr g))

instance Indexed d => Indexed (IndexedCDual' v d) where
    rows (IC' f _) = rows f
    cols (IC' f _) = cols f
    fromInt i (IC' f _) = IC' (fromInt i f) (const $ const id)

-- | function to transform CDual to CDual'
reprSCI :: (Ord v, IndexedSemiring d) => IndexedCDual v d -> IndexedCDual' v d
reprSCI (IC f df) = IC' f (\l r m -> unionWith plus m (df l r))

-- | function to transform CDual' to CDual
absSCI :: (Ord v, IndexedSemiring d) => IndexedCDual' v d -> IndexedCDual v d
absSCI (IC' f df) = IC f (\l r -> df l r empty)

insertWith :: (Ord v, IndexedSemiring d) => (d -> d -> d) -> v -> d -> d -> Map v d -> Map v d
insertWith f x l r m = unionWith f m (singleton x (l `times` r))

-- | function for indexed reverse AD with cayley representation
reverseADCayleyI :: (Ord v, IndexedSemiring d, Floating d, Norm d, Transposable d d, Indexed d) => (v -> d) -> IndexedExpr v -> IndexedCDual' v d
reverseADCayleyI env e = let genCayley x = IC' (env x) (\l r m -> insertWith plus x l r m)
                        in evalIndexed genCayley e

{- |
Data type for CDualIOI, dual number for indexed reverse AD with mutable arrays
-}
data IndexedCDualIO d = IM {fstMI :: d, sndMI :: d -> d -> IO ()}

instance (Num d, IndexedSemiring d, Transposable d d) => Num (IndexedCDualIO d) where
    (+)               = plus
    (*)               = times
    negate (IM f df)  = IM (negate f) (\l r -> df (negate l) r)

instance (Fractional d, IndexedSemiring d, Transposable d d) => Fractional (IndexedCDualIO d) where
    fromRational r           = IM (fromRational r) (\l r -> return ())
    (/) (IM f df) (IM g dg)  = IM (f / g) (\l r -> df (l / g) r >> dg (negate l * f / g / g) r)

instance (Floating d, IndexedSemiring d, Transposable d d) => Floating (IndexedCDualIO d) where
    exp (IM f df)  = IM (exp f) (\l r -> df (exp f * l) r)
    log (IM f df)  = IM (log f) (\l r -> df (l / f) r)

instance (Norm d, IndexedSemiring d) => Norm (IndexedCDualIO d) where
    norm (IM f df) = IM (norm f) (\l r -> df (oneNorm f `times` l) r)

instance Transposable d d => Transposable (IndexedCDualIO d) (IndexedCDualIO d) where
    tr (IM f df) = IM (tr f) (\l r -> df (tr r) (tr l))

instance (IndexedSemiring d, Transposable d d) => IndexedSemiring (IndexedCDualIO d) where
    zero r c                     = IM (zero r c) (\l r -> return ())
    one r                        = IM (one r) (\l r -> return ())
    (IM f df) `plus` (IM g dg)   = IM (f `plus` g) (\l r -> df l r >> dg l r)
    (IM f df) `times` (IM g dg)  = IM (f `times` g) (\l r -> df l (r `times` tr g) >> dg (tr f `times` l) r)

instance Indexed d => Indexed (IndexedCDualIO d) where
    rows (IM f _) = rows f
    cols (IM f _) = cols f
    fromInt i (IM f _) = IM (fromInt i f) (\l r -> return ())

reprIOIndexed :: (IndexedSemiring d, Ix v, Ord v) => IOArray v d -> IndexedCDual' v d -> IndexedCDualIO d
reprIOIndexed arr (IC' f df ) = let df' = \l r -> do
                                            let m = df l r empty
                                            forM_ (toList m) (\(v, a) ->
                                                do
                                                    b <- readArray arr v
                                                    writeArray arr v (b  `plus` a))
                                in IM f df'


absIOIndexed :: (IndexedSemiring d, Ix v, Ord v, Show v, Show d) => (v, v) -> (IOArray v d -> IndexedCDualIO d) -> IO (Map v d)
absIOIndexed rng d = do
                    arr <- newArray rng (zero 1 1)
                    let (IM f df) = d arr
                    df (one 1) (one 1)
                    l <- getAssocs arr
                    let m = fromAscList l
                    return m

-- | function for reverse AD with mutable arrays
reverseADIOIndexed :: (IndexedSemiring d, Ix v, Floating d, Norm d, Transposable d d, Show v, Show d, Indexed d) => (v, v) -> (v -> d) -> IndexedExpr v -> IO (Map v d)
reverseADIOIndexed rng env e = absIOIndexed rng (\arr ->
    let genIO x = IM (env x) (\l r -> do
                                        b <- readArray arr x
                                        writeArray arr x (b `plus` (l `times` r )))
    in evalIndexed genIO e)