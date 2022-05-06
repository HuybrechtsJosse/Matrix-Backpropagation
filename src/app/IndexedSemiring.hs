{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module IndexedSemiring where

------ IMPORTS ------
import AD.ForwardMode (Norm(..))
import GHC.Generics (Generic)
import Numeric.LinearAlgebra ((<>), ident, Transposable (tr), sumElements, fromLists, toLists, Element, (><))
import Numeric.LinearAlgebra.Data (Konst(konst))
import Numeric.LinearAlgebra.HMatrix (Matrix)
import Prelude hiding ((++), (**), (<>))

class Indexed d where
    rows :: d -> Int
    cols :: d -> Int

class IndexedSemiring d where
    zero  :: Int -> Int -> d
    one   :: Int -> d
    plus  :: d -> d -> d
    times :: d -> d -> d

instance IndexedSemiring Integer where
    zero _ _  = 0
    one  _    = 1
    plus      = (+)
    times     = (*)

instance IndexedSemiring Double where
    zero _ _  = 0
    one  _    = 1
    plus      = (+)
    times     = (*)

instance IndexedSemiring Float where
    zero _ _  = 0
    one  _    = 1
    plus      = (+)
    times     = (*)

data IndexedExpr v = Var v | Zero Int Int | One Int | Negate (IndexedExpr v) | Plus (IndexedExpr v) (IndexedExpr v) | Times (IndexedExpr v) (IndexedExpr v) | Div (IndexedExpr v) (IndexedExpr v) | Exp (IndexedExpr v) | Log (IndexedExpr v) | Norm (IndexedExpr v) | Transpose (IndexedExpr v)
    deriving (Show, Eq)

instance Num (IndexedExpr v) where
    (+)           = plus
    (*)           = times
    negate e      = Negate e

instance Fractional (IndexedExpr v) where
    (/)            = Div

instance Floating (IndexedExpr v) where
    exp = Exp
    log = Log

instance Norm (IndexedExpr v) where
    norm = Norm

instance Transposable (IndexedExpr v) (IndexedExpr v) where
    tr = Transpose

instance IndexedSemiring (IndexedExpr v) where
    zero   = Zero
    one    = One
    plus   = Plus
    times  = Times

evalIndexed :: (IndexedSemiring d, Floating d, Transposable d d, Norm d) => (v -> d) -> IndexedExpr v -> d
evalIndexed gen (Var x)       = gen x
evalIndexed gen (Zero r c)    = zero r c
evalIndexed gen (One r)       = one r
evalIndexed gen (Negate e)    = negate (evalIndexed gen e)
evalIndexed gen (Plus e1 e2)  = evalIndexed gen e1 `plus` evalIndexed gen e2
evalIndexed gen (Times e1 e2) = evalIndexed gen e1 `times` evalIndexed gen e2
evalIndexed gen (Div e1 e2)   = evalIndexed gen e1 / evalIndexed gen e2
evalIndexed gen (Exp e)       = exp (evalIndexed gen e)
evalIndexed gen (Log e)       = log (evalIndexed gen e)
evalIndexed gen (Transpose e) = tr (evalIndexed gen e)
evalIndexed gen (Norm e)      = norm (evalIndexed gen e)