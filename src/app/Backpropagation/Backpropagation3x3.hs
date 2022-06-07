{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backpropagation.Backpropagation3x3 where

------ IMPORTS ------
import AD.ForwardMode (Expr(..), SDual (sndSP), eval, Semiring(..))
import AD.ReverseMode (reverseADSparse, absDual)
import Data.Map (Map, (!))
import Data.Vector.Generic.Lens (vector)
import GHC.Generics ( Generic )
import Numeric.LinearAlgebra (Transposable (..))
import Numeric.OneLiner (gPlus, gMinus, gTimes, gNegate, gAbs, gSignum, gFromInteger, gDivide, gRecip, gFromRational)

instance Transposable Double Double where
    tr   = id 
    tr'  = id

-- Data type for small network example with 3 inputs and 1 output
data Net = N {w11::Double, w12::Double, w13::Double, w21::Double, w22::Double, w23::Double, w31::Double, w32::Double, w33::Double, b1::Double, b2::Double, b3::Double}
  deriving (Generic, Show, Eq)

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
data NetVar = I1 | I2 | I3 | W11 | W12 | W13 | W21 | W22 | W23 | W31 | W32 | W33 | B1 | B2 | B3 | T1 | T2 | T3
  deriving (Eq, Ord, Show)

-- logistic activation function
logistic :: (Semiring d, Floating d) => [d] -> [d]
logistic x = fmap (\n -> one / (one + exp (-n))) x

-- softMax activation function
softMax :: Floating d => [d] -> [d]
softMax x = vecDiv expx (vecNorm expx)
    where
        expx = vecExp x

-- Pointwise division of 2 arrays of expressions
vecDiv :: Fractional d => [d] -> [d] -> [d]
vecDiv = zipWith (/)

-- mapping an array to the exponential of its elements
vecExp :: Floating d => [d] -> [d]
vecExp = fmap exp

-- mapping an array to the sum of its elements
vecNorm :: Num d => [d] -> [d]
vecNorm v = fmap (const vNorm) v
    where
        vNorm = sum v

-- Square Error function
squareError :: (Semiring d, Floating d) => [d] -> [d] -> d
squareError x target = vecProd (zipWith (-) x target) (zipWith (-) x target) / (one + one)

-- function to get the expression for running the network on an input, weight matrix and bias
runNet :: (Semiring d, Floating d) => [d] -> [[d]] -> [d] -> [d]
runNet input w b  = logistic (zipWith (+) (matrixVecProd w input) b)

-- Function for getting an expression of a matrix vector multiplication
matrixVecProd :: Semiring d => [[d]] -> [d] -> [d]
matrixVecProd [w] v     = [vecProd w v]
matrixVecProd (w:ws) v  = vecProd w v:matrixVecProd ws v
matrixVecProd _ _       = error "Dimensions not matching"

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
    where gr = backpropagation (netErr [Var I1, Var I2, Var I3] [[Var W11, Var W21, Var W31],[Var W12, Var W22, Var W32],[Var W13, Var W23, Var W33]] [Var B1, Var B2, Var B3] [Var T1, Var T2, Var T3]) net0 input targ

-- create a Net of the gradient of the given Net for a given error function, input array and target value
backpropagation :: Expr NetVar -> Net -> [Double] -> [Double] -> Net
backpropagation err n input target = createNet diffMap
    where
        diff = reverseADSparse (\x -> let {env W11 = w11 n; env W12 = w12 n; env W13 = w13 n; env W21 = w21 n; env W22 = w22 n; env W23 = w23 n; env W31 = w31 n; env W32 = w32 n; env W33 = w33 n; env B1 = b1 n; env B2 = b2 n; env B3 = b3 n; env I1 = head input; env I2 = input !! 1; env I3 = input !! 2; env T1 = head target; env T2 = target !! 1; env T3 = target !! 2} in env x) err
        diffMap = sndSP (absDual diff)

-- function to evaluate the network in the given input array and target value
evalNetErr :: [Double] -> [Double] -> Net -> Double
evalNetErr input target n = eval (\x -> let {env W11 = w11 n; env W12 = w12 n; env W13 = w13 n; env W21 = w21 n; env W22 = w22 n; env W23 = w23 n; env W31 = w31 n; env W32 = w32 n; env W33 = w33 n; env B1 = b1 n; env B2 = b2 n; env B3 = b3 n; env I1 = head input; env I2 = input !! 1; env I3 = input !! 2; env T1 = head target; env T2 = target !! 1; env T3 = target !! 2}in env x) (netErr [Var I1, Var I2, Var I3] [[Var W11, Var W21, Var W31],[Var W12, Var W22, Var W32],[Var W13, Var W23, Var W33]] [Var B1, Var B2, Var B3] [Var T1, Var T2, Var T3])

-- function to create a Net from a Map with the network parameters
createNet :: Map NetVar Double -> Net
createNet m = N w11 w12 w13 w21 w22 w23 w31 w32 w33 b1 b2 b3
  where
    w11 = m ! W11
    w12 = m ! W12
    w13 = m ! W13
    w21 = m ! W21
    w22 = m ! W22
    w23 = m ! W23
    w31 = m ! W31
    w32 = m ! W32
    w33 = m ! W33
    b1  = m ! B1
    b2  = m ! B2
    b3  = m ! B3

-- function to perform multiple training steps
loopStepNet :: Int -> Net -> [Double] -> [Double] -> Net
loopStepNet 0 n input target = stepNet input target n
loopStepNet i n input target = stepNet input target (loopStepNet (i-1) n input target)

errorList :: Int -> Net -> [Double] -> [Double] -> [Double]
errorList 0 _ _ _ = []
errorList i n input target = evalNetErr input target n:errorList (i-1) (stepNet input target n) input target

-- main function for running the examples
main :: IO()
main = do
    let n = N 0.4 0.1 0.04 0.99 0.12 0.37 (-0.82) 0.421 0.92 1.5 (-0.43) 0.067
    -- print n
    let input = [2, 5, 1]
    let target = [0.4, 0.5, 0.1]
    let m = loopStepNet 10000 n input target
    print ("network error = " ++ show (evalNetErr input target m))