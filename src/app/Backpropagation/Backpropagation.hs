{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backpropagation.Backpropagation where

------ IMPORTS ------
import AD.ForwardMode (Expr(..), SDual (sndSP), eval, Semiring(..))
import AD.ReverseMode (reverseADSparse, absDual)
import Data.Map (Map, (!))
import Data.Vector.Generic.Lens (vector)
import GHC.Generics ( Generic )
import Numeric.LinearAlgebra (Transposable (..))
import Numeric.OneLiner (gPlus, gMinus, gTimes, gNegate, gAbs, gSignum, gFromInteger, gDivide, gRecip, gFromRational)

-- Instances to make it work with implementation for square matrices
instance (Transposable Double Double) where
  tr  d = d
  tr' d = d

-- Data type for small network example with 3 inputs and 1 output
data Net = N {w1::Double, w2::Double, w3::Double, b::Double}
  deriving (Generic, Show)

instance Num Net where
    (+)         = gPlus
    (-)         = gMinus
    (*)         = gTimes
    negate      = gNegate
    abs         = gAbs
    signum      = gSignum
    fromInteger = gFromInteger

instance Fractional Net where
    (/)          = gDivide
    recip        = gRecip
    fromRational = gFromRational

-- datatype for Var expression of network parameters
data NetVar = I1 | I2 | I3 | W1 | W2 | W3 | B | T
  deriving (Eq, Ord)

-- logistic activation function
logistic :: (Semiring d, Floating d) => [d] -> [d]
logistic x = fmap (\n -> one / (one + exp (-n))) x

-- Square Error function
squareError :: (Semiring d, Floating d) => [d] -> [d] -> d
squareError x target = vecProd (zipWith (-) x target) (zipWith (-) x target) / (one + one)

-- function to get the expression for running the network on an input, weight matrix and bias
runNet :: (Semiring d, Floating d) => [d] -> [[d]] -> [d] -> [d]
runNet input w b  = logistic (zipWith (+) (matrixVecProd w input) b)

-- Function for getting an expression of a matrix vector multiplication
matrixVecProd :: Semiring d => [[d]] -> [d] -> [d]
matrixVecProd []     _   = error "empty matrix"
matrixVecProd _      []  = error "empty vector"
matrixVecProd [w]    v   = [vecProd w v]
matrixVecProd (w:ws) v   = vecProd w v:matrixVecProd ws v


-- Function for getting an expression of a vector product
vecProd :: Semiring d => [d] -> [d] -> d
vecProd [w] [v] = w `times` v
vecProd (w:ws) (v:vs) = (w `times` v) `plus` vecProd ws vs
vecProd _ _ = error "Dimensions not matching"

-- Function for getting an expression of the error function
netErr :: (Semiring d, Floating d) => [d] -> [[d]] -> [d] -> [d] -> d
netErr input w b = squareError $ runNet input w b

-- function to perform 1 training step
stepNet :: [Double] -> [Double] -> Net -> Net
stepNet input targ net0 = net0 - 0.2 * gr
    where gr = backpropagation (netErr [Var I1, Var I2, Var I3] [[Var W1, Var W2, Var W3]] [Var B] [Var T]) net0 input targ

-- create a Net of the gradient of the given Net for a given error function, input array and target value
backpropagation :: Expr NetVar -> Net -> [Double] -> [Double] -> Net
backpropagation err n input target = createNet diffMap
    where
        diff = reverseADSparse (\x -> let {env W1 = w1 n; env W2 = w2 n; env W3 = w3 n; env B = b n; env I1 = head input; env I2 = input !! 1; env I3 = input !! 2; env T = head target} in env x) err
        diffMap = sndSP (absDual diff)

-- function to evaluate the network in the given input array and target value
evalNetErr :: [Double] -> [Double] -> Net -> Double
evalNetErr input target net = eval (\x -> let {env W1 = w1 net; env W2 = w2 net; env W3 = w3 net; env B = b net; env I1 = head input; env I2 = input !! 1; env I3 = input !! 2; env T = head target} in env x) (netErr [Var I1, Var I2, Var I3] [[Var W1, Var W2, Var W3]] [Var B] [Var T])

-- function to create a Net from a Map with the network parameters
createNet :: Map NetVar Double -> Net
createNet m = N w1 w2 w3 b
  where 
    w1 = m ! W1
    w2 = m ! W2
    w3 = m ! W3
    b  = m ! B

-- function to perform multiple training steps
loopStepNet :: Int -> Net -> [Double] -> [Double] -> Net
loopStepNet 0 n input target = stepNet input target n
loopStepNet i n input target = stepNet input target (loopStepNet (i-1) n input target)

-- main function for running the examples (works)
main :: IO()
main = do
    let n = N 0.1 0.52 (-0.9) 1.2
    -- print n
    let input = [2, 5, 1]
    let target = [0.4]
    let m = loopStepNet 50 n input target
    print ("network error = " ++ show (evalNetErr input target m))
