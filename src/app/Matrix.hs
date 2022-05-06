{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Matrix where
import Data.List (transpose)
import Numeric.LinearAlgebra (Transposable (tr), Normed)
import AD.ForwardMode (Norm (..))
import Prelude hiding ((<>))

data M a = M Int Int (Matrix a)
type Matrix a = [Vector a]
type Vector a = [a]

instance Num a => Num (Vector a) where  
    (+) = zipWith (+)
    (*) = zipWith (*)
    abs = map abs
    signum = map signum
    negate = map negate

instance Fractional a => Fractional (Vector a) where
    (/) = zipWith (/)

instance Floating a => Floating (Vector a) where
    exp = map exp
    log = map log

instance Transposable (Matrix a) (Matrix a) where
    tr = transpose

instance Num a => Norm (Matrix a) where
    norm m = map (map (const $ sum $ concat m)) m

(<>) :: Num a => Matrix a -> Matrix a -> Matrix a
m1 <> m2 = [[v1 <.> v2 | v2 <- transpose m2] | v1 <- m1]

(<.>) :: Num a => Vector a -> Vector a -> a
[v1] <.> [v2] = v1*v2
(v1:vl1) <.> (v2:vl2) = v1*v2 + vl1 <.> vl2
_ <.> _ = error "Vector Dimensions not matching"

konst :: a -> (Int,Int) -> Matrix a
konst a (r,c) = [[a | _ <- [1..c]] | _ <- [1..r]]

ident :: Num a => Int -> Matrix a
ident i = [[if x == y then 1 else 0 | x <- [1..i]] | y <- [1..i]]