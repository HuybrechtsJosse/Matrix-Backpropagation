{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module AD.ForwardMode where

------ IMPORTS ------
import Data.Array.IO (Ix)
import Data.Map (Map, singleton, unionWith, findWithDefault, empty)
import Numeric.LinearAlgebra (Transposable (..))
import Prelude hiding ((++), (**))

------ CLASSES ------

{- 
Class for Semiring with 4 operators:
    zero
    one
    plus --addition
    times -- multiplication
Instances of Semiring for Integer, Double and Float
-}
class Semiring d where
    zero   :: d
    one    :: d
    plus   :: d -> d -> d
    times   :: d -> d -> d

instance Semiring Integer where
    zero   = 0
    one    = 1
    plus   = (+)
    times   = (*)

instance Semiring Double where
    zero   = 0
    one    = 1
    plus   = (+)
    times   = (*)

instance Semiring Float where
    zero = 0
    one  = 1
    plus = (+)
    times = (*)

{- 
Class for Norm with 1 operator:
    norm
Instances of Norm for Integer, Double, Float and Expr v
-}
class Norm a where
    norm :: a -> a
    oneNorm :: a -> a

instance Norm Integer where
    norm = id

instance Norm Double where
    norm = id

instance Norm Float where
    norm = id

------ DATA TYPES AND FUNCTIONS ------
{-
Data type for expressions
Intstances Num, Fractional, Floating, Norm, Transposable, Semiring
-}
data Expr v = Var v | Zero | One | Negate (Expr v) | Plus (Expr v) (Expr v) | Times (Expr v) (Expr v) | Div (Expr v) (Expr v) | Exp (Expr v) | Log (Expr v) | Norm (Expr v) | Transpose (Expr v)
    deriving (Show, Eq)

instance Num (Expr v) where
    (+)           = plus
    (*)           = times
    abs e         = undefined
    signum        = undefined
    fromInteger 0 = Zero
    fromInteger i = One + (fromInteger (i-1))
    negate e      = Negate e

instance Fractional (Expr v) where
    fromRational r = undefined
    (/)            = Div

instance Floating (Expr v) where
    exp = Exp
    log = Log

instance Norm (Expr v) where
    norm = Norm

instance Transposable (Expr v) (Expr v) where
    tr = Transpose
    tr' = Transpose

instance Semiring (Expr v) where
    zero   = Zero
    one    = One
    plus   = Plus
    times   = Times

-- Eval function for evaluating an expression given a generator for the variables in the expression
eval :: (Semiring d, Floating d, Transposable d d, Norm d) => (v -> d) -> Expr v -> d
eval gen (Var x)       = gen x
eval gen Zero          = zero
eval gen One           = one
eval gen (Negate e)    = negate (eval gen e)
eval gen (Plus e1 e2)  = eval gen e1 `plus` eval gen e2
eval gen (Times e1 e2) = eval gen e1 `times` eval gen e2
eval gen (Div e1 e2)   = eval gen e1 / eval gen e2
eval gen (Exp e)       = exp (eval gen e)
eval gen (Log e)       = log (eval gen e)
eval gen (Transpose e) = tr (eval gen e)
eval gen (Norm e)      = norm (eval gen e)


{-
Data type for dual numbers
Instances: Functor, Num, Fractional, Floating, Norm, Transposable, Semiring
-}
data Dual d = D {fstD :: d, sndD :: d}
    deriving (Show)

instance Functor Dual where
    fmap h (D x x0) = D (h x) (h x0)

instance (Num d, Semiring d) => Num (Dual d) where
    (+)              = plus
    (*)              = times
    abs d            = fmap abs d
    signum d         = fmap signum d
    fromInteger i    = D (fromInteger i) 0
    negate (D f df)  = D (negate f) (negate df)

instance (Fractional d, Semiring d) => Fractional (Dual d) where
    fromRational r        = D (fromRational r) 0
    (/) (D f df) (D g dg) = D (f / g) ((df - f * dg / g) / g)

instance (Floating d, Semiring d) => Floating (Dual d) where
    exp (D f df) = D (exp f) (exp f * df)
    log (D f df) = D (log f) (df / f)

instance Norm d => Norm (Dual d) where
    norm d = fmap norm d

instance Transposable d d => Transposable (Dual d) (Dual d) where
    tr  d = fmap tr  d
    tr' d = fmap tr' d

instance Semiring d => Semiring (Dual d) where
    zero                 = D zero zero
    one                  = D one zero
    (D f df) `plus` (D g dg) = D (f `plus` g) (df `plus` dg)
    (D f df) `times` (D g dg) = D (f `times` g) ((df `times` g) `plus` (f `times` dg))

-- Functions for calculating the derivative of an expression
derive :: Eq v => v -> Expr v -> Expr v
derive x = sndD . derive' x

derive' :: Eq v => v -> Expr v -> Dual (Expr v)
derive' x e =let gen y = D (Var y) (if x == y then One else Zero)
             in eval gen e

evalDerive :: (Eq v, Semiring d, Floating d, Norm d, Transposable d d) => (v -> d) -> v -> Expr v -> d
evalDerive env x = eval env . derive x

-- Forward mode AD
forwardAD :: (Eq v, Semiring d, Floating d, Norm d, Transposable d d) => (v -> d) -> v -> Expr v -> d
forwardAD env x e = let gen y = D (env y) (if x == y then one else zero)
                    in sndD (eval gen e)

{-  
Type class for DualGrad v d, a function of type v -> Dual d
Intsances: Num, Fractional, Floating, Norm, Transposable, Semiring
-}
type DualGrad v d = v -> Dual d

instance Num d => Num (DualGrad v d)
instance Fractional d => Fractional (DualGrad v d)
instance Floating d => Floating (DualGrad v d)

instance Norm d => Norm (DualGrad v d) where
  norm f = norm . f

instance Transposable d d => Transposable (DualGrad v d) (DualGrad v d) where
  tr f = tr . f
  tr' f = tr' . f

instance Semiring d => Semiring (DualGrad v d) where
    zero     = \v -> D zero zero
    one      = \v -> D one zero
    x `plus` y   = \v -> x v `plus` y v
    x `times` y   = \v -> x v `times` y v

-- function for forwardGradient, representing the differentiation as a function of the variable with the dual number with the evaluation and differentiation as the result.
forwardGradient :: (Eq v, Semiring d, Floating d, Norm d, Transposable d d) => (v -> d) -> Expr v -> DualGrad v d
forwardGradient env e = let gen x = \y -> D (env x) (if x == y then one else zero)
                      in eval gen e

{-
Data type for AllDual'
Intsances: Num, Fractional, Floating, Norm, Transposable, Semiring
-}
data AllDual' v d = SH {fstSH :: d, sndSH :: v -> d}

instance Num d => Num (AllDual' v d)
instance Fractional d => Fractional (AllDual' v d)
instance Floating d => Floating (AllDual' v d)

instance Norm d => Norm (AllDual' v d) where
  norm (SH f df) = SH (norm f) (norm . df)

instance Transposable d d => Transposable (AllDual' v d) (AllDual' v d) where
  tr  (SH f df) = SH (tr  f) (tr  . df)
  tr' (SH f df) = SH (tr' f) (tr' . df)

instance Semiring d => Semiring (AllDual' v d) where
    zero                   = SH zero (const zero)
    one                    = SH one (const zero)
    (SH f df) `plus` (SH g dg) = SH (f `plus` g) (\v -> df v `plus` dg v)
    (SH f df) `times` (SH g dg) = SH (f `times` g) ((g `act` df) `dotplus` (f `act` dg))

dotplus :: Semiring d => (v -> d) -> (v -> d) -> (v -> d)
df `dotplus` dg = \ v -> df v `plus` dg v

act :: Semiring d => d -> (v -> d) -> (v -> d)
f `act` dg = \ v -> f `times` dg v
-- function to transform AllDual' to DualGrad
unshare :: AllDual' v d -> DualGrad v d
unshare (SH f df) = \v -> D f (df v)

-- Function for froward AD with shared evaluation
forwardSharedGradient :: (Eq v, Semiring d, Floating d, Norm d, Transposable d d) => (v -> d) -> Expr v -> AllDual' v d
forwardSharedGradient env e = let genShare x = SH (env x) (\y -> if x == y then one else zero)
                       in eval genShare e

{- 
Data type for SDual
Intsances: Num, Fractional, Floating, Norm, Transposable, Semiring
-}
data SDual v d = SP {fstSP :: d, sndSP :: Map v d}
    deriving (Show, Eq)

instance (Num d, Ord v, Semiring d) => Num (SDual v d) where
    (+)               = plus
    (*)               = times
    abs (SP f df)     = SP (abs f) (fmap abs df)
    signum (SP f df)  = SP (signum f) (fmap signum df)
    fromInteger i     = SP (fromInteger i) empty
    negate (SP f df)  = SP (negate f) (fmap negate df)

instance (Fractional d, Ord v, Semiring d) => Fractional (SDual v d) where
    fromRational r = SP (fromRational r) empty
    (/) (SP f df) (SP g dg) = SP (f / g) (unionWith (+) (fmap (\n-> n/g) df) (fmap (\n-> -f*n/g/g ) dg))

instance (Floating d, Ord v, Semiring d) => Floating (SDual v d) where
    exp (SP f df) = SP (exp f) (fmap (\n -> exp f * n) df)
    log (SP f df) = SP (log f) (fmap (\n -> n / f) df)

instance Norm d => Norm (SDual v d) where
    norm (SP f df) = SP (norm f) (fmap norm df)

instance Transposable d d => Transposable (SDual v d) (SDual v d) where
    tr  (SP f df) = SP (tr  f) (fmap tr  df)
    tr' (SP f df) = SP (tr' f) (fmap tr' df)

instance (Ord v, Semiring d) => Semiring (SDual v d) where
    zero = SP zero empty
    one = SP one empty
    (SP f df) `plus` (SP g dg) = SP (f `plus` g) (df `dotplus2` dg)
    (SP f df) `times` (SP g dg) = SP (f `times` g) ((g `act'` df) `dotplus2` (f `act'` dg))

-- function to transform SDual to AllDual'
expand :: (Ord v, Semiring d) => SDual v d -> AllDual' v d
expand (SP f df) = SH f (\x -> findWithDefault zero x df)

--  functions for easy working with maps in addition and multiplication differentiation
dotplus2 :: (Ord v, Semiring d) => Map v d -> Map v d -> Map v d
df `dotplus2` dg = unionWith plus df dg

act' :: (Ord v, Semiring d) => d -> Map v d -> Map v d
f `act'` dg = fmap (f `times`) dg

-- Function for froward AD with sparse differentiation and shared evaluation
forwardSparseGradient :: (Ord v, Semiring d, Floating d, Norm d, Transposable d d) => (v -> d) -> Expr v -> SDual v d
forwardSparseGradient env e = let genSparse x = SP (env x) (singleton x one)
                        in eval genSparse e


-- * Example expressions for test computations
{-| 
Data type for the variables in expressions of the example expressions
-}
data XY = X | Y
    deriving (Eq, Show, Ord, Ix)

-- | x*(x+1)
example1 :: Expr XY 
example1 = Times (Var X) (Plus (Var X) One)

-- | ((x*y)+x)+1
example2 :: Expr XY 
example2 = Plus (Plus (Times (Var X) (Var Y)) (Var X)) One

-- | x*((x+1)*(x+x))
example3 :: Expr XY 
example3 = Times (Var X) (Times (Plus (Var X) One) (Plus (Var X) (Var X)))