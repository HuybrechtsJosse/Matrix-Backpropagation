{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE QuantifiedConstraints #-}

module IndexedSemiringStatic where

------ IMPORTS ------
import AD.ForwardMode (Norm(..))
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat)
import Prelude hiding ((<>))

import Numeric.LinearAlgebra (Transposable (..))
import Numeric.LinearAlgebra.Static ((<>))

import qualified Numeric.LinearAlgebra.Static as H

data NetVar n m where 
    I  :: NetVar 784 1
    W1 :: NetVar 50 784
    B1 :: NetVar 50 1
    W2 :: NetVar 20 50
    B2 :: NetVar 20 1
    W3 :: NetVar 20 20
    B3 :: NetVar 20 1
    W4 :: NetVar 20 20
    B4 :: NetVar 20 1
    W5 :: NetVar 10 20
    B5 :: NetVar 10 1
    T  :: NetVar 10 1
deriving instance Eq (NetVar n m)
deriving instance Ord (NetVar n m)
deriving instance Show (NetVar n m)

class IndexedSemiringStatic d where
    zero  :: (KnownNat n, KnownNat m) => d n m
    one   :: KnownNat n => d n n
    plus  :: (KnownNat n, KnownNat m) => d n m -> d n m -> d n m
    times :: (KnownNat l, KnownNat n, KnownNat m) => d n l -> d l m -> d n m

instance IndexedSemiringStatic H.L where
    zero = 0
    one = H.eye
    plus = (+)
    times = (<>)

instance KnownNat n => Norm (H.L n 1) where
    norm m  = 1 `times` m

data IndexedExprStatic n m where
    Var :: NetVar n m -> IndexedExprStatic n m
    Zero :: IndexedExprStatic n m
    One :: IndexedExprStatic n n
    FromInt :: Int -> IndexedExprStatic n m
    Negate :: IndexedExprStatic n m -> IndexedExprStatic n m
    Plus :: IndexedExprStatic n m -> IndexedExprStatic n m -> IndexedExprStatic n m
    Times :: KnownNat k => IndexedExprStatic n k -> IndexedExprStatic k m -> IndexedExprStatic n m
    Div :: IndexedExprStatic n 1 -> IndexedExprStatic n 1 -> IndexedExprStatic n 1
    Exp :: IndexedExprStatic n 1 -> IndexedExprStatic n 1
    Log :: IndexedExprStatic n 1 -> IndexedExprStatic n 1
    Norm :: IndexedExprStatic n 1 -> IndexedExprStatic n 1
    Transpose :: IndexedExprStatic m n -> IndexedExprStatic n m

deriving instance Show (IndexedExprStatic n m)

instance (KnownNat n, KnownNat m) => Num  (IndexedExprStatic n m) where
    (+)           = plus
    negate e      = Negate e
    fromInteger   = FromInt . fromInteger

instance (KnownNat n) => Fractional  (IndexedExprStatic n 1) where
    (/)            = Div

instance (KnownNat n) => Floating  (IndexedExprStatic n 1) where
    exp = Exp
    log = Log

instance Norm (IndexedExprStatic n 1) where
    norm = Norm

instance Transposable (IndexedExprStatic n m) (IndexedExprStatic m n) where
    tr = Transpose

instance IndexedSemiringStatic IndexedExprStatic where
    zero   = Zero
    one    = One
    plus   = Plus
    times  = Times


evalStatic :: (IndexedSemiringStatic d, KnownNat n, KnownNat m, forall a.KnownNat a=>Norm (d a 1),forall a.(KnownNat a)=>Floating (d a 1),forall a b.(KnownNat a, KnownNat b)=>Num (d a b),forall a b.(KnownNat a, KnownNat b)=>Transposable (d a b) (d b a)) => (forall n1 m1.(KnownNat n1, KnownNat m1) => NetVar n1 m1 -> d n1 m1) -> IndexedExprStatic n m -> d n m
evalStatic gen (Var x)       = gen x
evalStatic gen Zero          = zero
evalStatic gen One           = one
evalStatic gen (FromInt i)   = fromInteger $ toInteger i
evalStatic gen (Negate e)    = negate (evalStatic gen e)
evalStatic gen (Plus e1 e2)  = evalStatic gen e1 `plus` evalStatic gen e2
evalStatic gen (Times e1 e2) = evalStatic gen e1 `times` evalStatic gen e2
evalStatic gen (Div e1 e2)   = evalStatic gen e1 / evalStatic gen e2
evalStatic gen (Exp e)       = exp (evalStatic gen e)
evalStatic gen (Log e)       = log (evalStatic gen e)
evalStatic gen (Transpose e) = tr (evalStatic gen e)
evalStatic gen (Norm e)      = norm (evalStatic gen e)