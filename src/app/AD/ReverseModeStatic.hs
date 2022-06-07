{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE UndecidableInstances  #-}

module AD.ReverseModeStatic where

------ IMPORTS ------
import Data.Array.IO (Ix, IOArray, readArray, writeArray, newArray, getAssocs)
import Prelude hiding ((++), (**))
import Data.Map (Map, unionWith, singleton, empty, toList, fromAscList)
import Control.Monad (forM_)
import IndexedSemiringStatic (IndexedSemiringStatic (..), IndexedExprStatic (..), evalStatic, NetVar (..))
import qualified Numeric.LinearAlgebra.Static as H
import GHC.TypeLits (Nat, KnownNat)
import AD.ForwardMode (SDual (..), Norm (..))
import Numeric.LinearAlgebra (Transposable (..))

reprBI :: (IndexedSemiringStatic d, KnownNat n, KnownNat m, KnownNat l) => (d n l -> d l m -> d n m)
reprBI = \l r -> l `times` r

absBI :: (IndexedSemiringStatic d, KnownNat n) => (d n n -> d n n -> d n1 m) -> d n1 m
absBI f  = f one one

ddotplus :: (IndexedSemiringStatic d, KnownNat n, KnownNat m) => (d n1 l -> d l m1-> d n m) -> (d n1 l -> d l m1 -> d n m) -> (d n1 l -> d l m1 -> d n m)
f `ddotplus` g = \l r -> f l r `plus` g l r

actL :: (IndexedSemiringStatic d, KnownNat n1, KnownNat n2, KnownNat l) => d n2 n1 -> (d n2 l -> d l m1 -> d n m) -> (d n1 l -> d l m1 -> d n m)
x `actL` f = \l r -> f (x `times`l) r

actPointwise :: forall (d::Nat->Nat-> *) l n n1 m m1.(Num (d n1 l)) => d n1 l -> (d n1 l -> d l m1 -> d n m) -> (d n1 l -> d l m1 -> d n m)
x `actPointwise` f = \l r -> f (x*l) r

actR :: (IndexedSemiringStatic d, KnownNat m1, KnownNat l, KnownNat m2) => (d n1 l -> d l m2 -> d n m) -> d m1 m2 -> (d n1 l -> d l m1 -> d n m)
f `actR` x = \l r -> f l (r `times` x)

{-|
Datatype for dual numbers for Static Indexed Reverse AD
-}
data StaticDualR d n m n1 m1  = SD {fstSD :: d n1 m1, sndSD :: d n1 1 -> d 1 m1 -> d n m}

instance (IndexedSemiringStatic d, KnownNat n, KnownNat m, KnownNat n1, KnownNat m1, Num (d n1 m1), Num (d n1 1), forall a b.(KnownNat a, KnownNat b)=>Transposable (d a b) (d b a)) => Num (StaticDualR d n m n1 m1) where
    (+)               = plus
    negate (SD f df)  = SD (negate f) (\l r -> df (negate l) r)
    fromInteger i = SD (fromInteger i) (const $ const zero)

instance (IndexedSemiringStatic d, KnownNat n, KnownNat m, KnownNat n1, Fractional (d n1 1), forall a b.(KnownNat a, KnownNat b)=>Transposable (d a b) (d b a)) => Fractional (StaticDualR d n m n1 1) where
    (/) (SD f df) (SD g dg) = SD (f / g) (\l r -> df (l / g) r `plus` dg (negate (f/(g*g))*l) r)

instance (IndexedSemiringStatic d, KnownNat n, KnownNat m, KnownNat n1, forall a b.(KnownNat a, KnownNat b)=>Floating (d a b), forall a b.(KnownNat a, KnownNat b)=>Transposable (d a b) (d b a)) => Floating (StaticDualR d n m n1 1) where
    exp (SD f df) = SD (exp f) (exp f `actPointwise` df)
    log (SD f df) = SD (log f) (\l r -> df (l/f) r)

instance (IndexedSemiringStatic d, KnownNat n, KnownNat m, forall a b.(KnownNat a, KnownNat b)=>Num (d a b), Norm (d n1 1), KnownNat n1) => Norm (StaticDualR d n m n1 1) where
    norm (SD f df) = SD (norm f) (\l r -> df (1 `times` l) r)

instance (forall a b.(KnownNat a, KnownNat b)=>Transposable (d a b) (d b a), KnownNat n1, KnownNat m1) => Transposable (StaticDualR d n m n1 m1) (StaticDualR d n m m1 n1) where
    tr (SD f df) = SD (tr f) (\l r -> df (tr r) (tr l))

instance (IndexedSemiringStatic d, KnownNat n, KnownNat m, forall a b.(KnownNat a, KnownNat b)=>Transposable (d a b) (d b a)) => IndexedSemiringStatic (StaticDualR d n m) where
    zero = SD zero (const $ const zero)
    one  = SD one (const $ const zero)
    plus (SD f df) (SD g dg) = SD (f `plus` g) (df `ddotplus` dg)
    times (SD f df) (SD g dg) = SD (f `times` g) ((df `actR` tr g) `ddotplus` (tr f `actL` dg))

{-|
@reverseAStatic env x e@ is a function to perform static indexed reverse AD to compute the derivative of @e@ to @x@ with values given in @env@.
 -}
reverseADStatic :: forall d n m.(IndexedSemiringStatic d, KnownNat n, KnownNat m,forall a.KnownNat a=> Norm (d a 1), forall a b.(KnownNat a, KnownNat b)=>Floating (d a b), forall a b.(KnownNat a, KnownNat b)=>Transposable (d a b) (d b a)) => (forall n1 m1.(KnownNat n1, KnownNat m1) => NetVar n1 m1 -> d n1 m1) -> NetVar n m -> IndexedExprStatic 1 1 -> StaticDualR d n m 1 1
reverseADStatic env W1 e = evalStatic gen e
    where
        gen :: (KnownNat n1, KnownNat m1) => NetVar n1 m1 -> StaticDualR d n m n1 m1
        gen W1 = SD (env W1) reprBI
        gen x = SD (env x) (const $ const zero)
reverseADStatic env B1 e = evalStatic gen e
    where
        gen :: (KnownNat n1, KnownNat m1) => NetVar n1 m1 -> StaticDualR d n m n1 m1
        gen B1 = SD (env B1) reprBI
        gen x = SD (env x) (const $ const zero)
reverseADStatic env W2 e = evalStatic gen e
    where
        gen :: (KnownNat n1, KnownNat m1) => NetVar n1 m1 -> StaticDualR d n m n1 m1
        gen W2 = SD (env W2) reprBI
        gen x = SD (env x) (const $ const zero)
reverseADStatic env B2 e = evalStatic gen e
    where
        gen :: (KnownNat n1, KnownNat m1) => NetVar n1 m1 -> StaticDualR d n m n1 m1
        gen B2 = SD (env B2) reprBI
        gen x = SD (env x) (const $ const zero)
reverseADStatic env W3 e = evalStatic gen e
    where
        gen :: (KnownNat n1, KnownNat m1) => NetVar n1 m1 -> StaticDualR d n m n1 m1
        gen W3 = SD (env W3) reprBI
        gen x = SD (env x) (const $ const zero)
reverseADStatic env B3 e = evalStatic gen e
    where
        gen :: (KnownNat n1, KnownNat m1) => NetVar n1 m1 -> StaticDualR d n m n1 m1
        gen B3 = SD (env B3) reprBI
        gen x = SD (env x) (const $ const zero)
reverseADStatic env W4 e = evalStatic gen e
    where
        gen :: (KnownNat n1, KnownNat m1) => NetVar n1 m1 -> StaticDualR d n m n1 m1
        gen W4 = SD (env W4) reprBI
        gen x = SD (env x) (const $ const zero)
reverseADStatic env B4 e = evalStatic gen e
    where
        gen :: forall n1 m1.(KnownNat n1, KnownNat m1) => NetVar n1 m1 -> StaticDualR d n m n1 m1
        gen B4 = SD (env B4) reprBI
        gen x = SD (env x) (const $ const zero)
reverseADStatic env W5 e = evalStatic gen e
    where
        gen :: forall n1 m1.(KnownNat n1, KnownNat m1) => NetVar n1 m1 -> StaticDualR d n m n1 m1
        gen W5 = SD (env W5) reprBI
        gen x = SD (env x) (const $ const zero)
reverseADStatic env B5 e = evalStatic gen e
    where
        gen :: forall n1 m1.(KnownNat n1, KnownNat m1) => NetVar n1 m1 -> StaticDualR d n m n1 m1
        gen B5 = SD (env B5) reprBI
        gen x = SD (env x) (const $ const zero)
reverseADStatic env I e = evalStatic gen e
    where
        gen :: forall n1 m1.(KnownNat n1, KnownNat m1) => NetVar n1 m1 -> StaticDualR d n m n1 m1
        gen I = SD (env I) reprBI
        gen x = SD (env x) (const $ const zero)
reverseADStatic env T e = evalStatic gen e
    where
        gen :: forall n1 m1.(KnownNat n1, KnownNat m1) => NetVar n1 m1 -> StaticDualR d n m n1 m1
        gen T = SD (env T) reprBI
        gen x = SD (env x) (const $ const zero)