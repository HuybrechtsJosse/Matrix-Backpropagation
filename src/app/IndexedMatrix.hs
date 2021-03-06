{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveGeneric         #-}

module IndexedMatrix where

------ IMPORTS ------
import AD.ForwardMode (Norm(..))
import GHC.Generics (Generic)
import IndexedSemiring (Indexed(..), IndexedSemiring(..))
import Numeric.LinearAlgebra (Transposable (tr))
import Numeric.LinearAlgebra.Data (Matrix, Konst (konst), ident, (><), rows, cols)

data IndexedMatrix = IM {nRows :: Int, nCols :: Int, matrix :: Matrix Double}
    deriving (Show, Eq, Generic)

instance Num IndexedMatrix where
    (+) = plus
    (*) (IM r1 c1 m1) (IM r2 c2 m2) = IM r1 c1 (m1*m2)
    negate (IM r c m) = IM r c (negate m)

instance Fractional IndexedMatrix where
    (/) (IM r1 c1 m1) (IM r2 c2 m2) = IM r1 c1 (m1/m2)

instance Floating IndexedMatrix where
    exp (IM r c m) = IM r c (exp m)
    log (IM r c m) = IM r c (log m)

instance Transposable IndexedMatrix IndexedMatrix where
    tr (IM r c m) = IM c r (tr m)

instance Norm IndexedMatrix where
    norm m = oneNorm m `times` m
    oneNorm (IM r c m) = IM r r (konst 1 (r,r))
 
instance Indexed IndexedMatrix where
    rows = nRows
    cols = nCols
    fromInt i (IM r c m) = IM r c (fromInt i m)

instance Indexed (Matrix Double) where
    rows = Numeric.LinearAlgebra.Data.rows
    cols = Numeric.LinearAlgebra.Data.cols
    fromInt i m = (r><c)  [fromInteger (toInteger i) | _ <- [1..r*c]]
        where
            r = IndexedSemiring.rows m
            c = IndexedSemiring.cols m

instance IndexedSemiring IndexedMatrix where
    zero i j = IM i j (konst 0 (i,j))
    one i = IM i i (ident i)
    plus (IM i1 j1 m1) (IM i2 j2 m2) = IM i1 j1 (m1 + m2)
    times (IM i1 j1 m1) (IM i2 j2 m2) = IM i1 j2 (m1 <> m2)