{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric         #-}

module Backpropagation.BackpropagationFull where

------ IMPORTS ------
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Control.Monad.Trans.State (evalStateT, StateT (StateT))
import Data.Bitraversable (Bitraversable(bitraverse))
import Data.IDX (decodeIDXLabelsFile, decodeIDXFile, labeledIntData)
import Data.Foldable (Foldable(foldl'), forM_)
import Data.List (transpose, (!!))
import Data.List.Split (chunksOf)
import Data.Map (Map, (!), findWithDefault, fromList)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Traversable (for)
import Data.Tuple (swap)
import Data.Vector.Generic.Lens (vector)
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Printf (printf)

import AD.ForwardMode (Expr(..), SDual (sndSP), eval, Semiring(..), forwardSparseGradient)
import AD.ReverseMode (reverseADSparse, absDual, reverseAD, reverseADCayley, absSC)

import qualified Data.Vector                     as V
import qualified System.Random.MWC.Distributions as MWC
import qualified Numeric.LinearAlgebra           as HM
import qualified Numeric.LinearAlgebra.Data      as HD
import qualified Numeric.LinearAlgebra.Static    as H
import qualified Data.Vector.Generic             as VG
import qualified System.Random.MWC               as MWC

-- Instances to make it work with implementation for square matrices
instance (HM.Transposable Double Double) where
    tr  d = d
    tr' d = d

data NetVar2 = W1 | B1 | W2 | B2 | I | T
    deriving (Show, Eq, Ord, Generic)

-- Data type for small network example with 3 inputs and 1 output
data Net = N { w1:: (NetVar2, H.L 10 784)
             , b1::(NetVar2, H.L 10 1)
            --  , w2:: (NetVar2, H.L 4 10)
            --  , b2::(NetVar2, H.L 4 1)
             }
  deriving (Show, Generic)

-- Instances for Num and Fractional for Net
instance Num Net where
    (+) (N (w1s1, w1l1) (b1s1, b1l1)) (N (w1s2, w1l2) (b1s2, b1l2)) = N (w1s1, w1l1 + w1l2) (b1s1, b1l1 + b1l2)
    (-) (N (w1s1, w1l1) (b1s1, b1l1)) (N (w1s2, w1l2) (b1s2, b1l2)) = N (w1s1, w1l1 - w1l2) (b1s1, b1l1 - b1l2)
    (*) (N (w1s1, w1l1) (b1s1, b1l1)) (N (w1s2, w1l2) (b1s2, b1l2)) = N (w1s1, w1l1 * w1l2) (b1s1, b1l1 * b1l2)
    negate (N (w1s, w1l) (b1s, b1l))                                = N (w1s, negate w1l) (b1s, negate b1l)
    abs (N (w1s, w1l) (b1s, b1l))                                   = N (w1s, abs w1l) (b1s, abs b1l)
    signum (N (w1s, w1l) (b1s, b1l))                                = N (w1s, signum w1l) (b1s, signum b1l)
    fromInteger i                                                   = N (W1, fromInteger i) (B1, fromInteger i)

instance Fractional Net where
    (/) (N (w1s1, w1l1) (b1s1, b1l1)) (N (w1s2, w1l2) (b1s2, b1l2)) = N (w1s1, w1l1 / w1l2) (b1s1, b1l1 / b1l2)
    recip (N (w1s, w1l) (b1s, b1l))                                 = N (w1s, recip w1l) (b1s, recip b1l)
    fromRational r                                                  = N (W1, fromRational r) (B1, fromRational r)

-- function to create a Net from a Map with the network parameters
createNet :: Map MatrixVar Double -> Net
createNet matrixVarMap = N w1 b1
    where
        w1 = (W1, H.build (\n m -> findWithDefault 0 (W1, round n, round m) matrixVarMap))
        b1 = (B1, H.build (\n m -> findWithDefault 0 (B1, round n, round m) matrixVarMap))
        -- w2 = (W2, build (\n m -> findWithDefault 0 (W2, round n, round m) matrixVarMap))
        -- b2 = (B2, build (\n m -> findWithDefault 0 (B2, round n, round m) matrixVarMap))

-- Function to transform a Net and input and target into a map
getMatrixVarMap :: Net -> (NetVar2, H.L 784 1) -> Map NetVar2 [[Double]]
getMatrixVarMap (N (w1s, w1l) (b1s, b1l)) (is, il) = fromList [(w1s, HD.toLists (H.extract w1l)),(b1s, HD.toLists (H.extract b1l)),(is, HD.toLists (H.extract il))]

getMatrixVarMapErr :: Net -> (NetVar2, H.L 784 1) -> (NetVar2, H.L 10  1) -> Map NetVar2 [[Double]]
getMatrixVarMapErr (N (w1s, w1l) (b1s, b1l)) (is, il) (ts, tl) = fromList [(w1s, HD.toLists (H.extract w1l)),(b1s, HD.toLists (H.extract b1l)),(is, HD.toLists (H.extract il)),(ts, HD.toLists (H.extract tl))]

-- Function to get the value of a variable based on its MatrixVar (used in eval for evalNet or evalNetErr)
getVal :: MatrixVar -> Net -> (NetVar2, H.L 784 1) -> Double
getVal (s, i, j) n input = ((matrixVarMap ! s) !! i) !! j
    where
        matrixVarMap = getMatrixVarMap n input

getValErr :: MatrixVar -> Net -> (NetVar2, H.L 784 1) -> (NetVar2, H.L 10 1) -> Double
getValErr (s, i, j) n input output = ((matrixVarMap ! s) !! i) !! j
    where
        matrixVarMap = getMatrixVarMapErr n input output

-- type for Var expression of network parameters: (Matrix, Row, Col)
type MatrixVar = (NetVar2, Int, Int)

-- Function to create an array of expressions of an input matrix with a NetVar2 as name
createMatrixVar :: (KnownNat n, KnownNat m) => (NetVar2, H.L n m) -> [[Expr MatrixVar]]
createMatrixVar (s, l) = [[Var (s, i, j) | j <- [0..m-1]] | i <- [0..n-1]]
    where
        (n,m) = H.size l


------- ACTIVATION AND LOSS FUNCTIONS -------

-- logistic activation function
logistic :: (Floating d, Semiring d) => [[d]] -> [[d]]
logistic x =  fmap (fmap (\n -> one / (one + exp (-n)))) x

-- softMax activation function
softMax :: Floating d => [[d]] -> [[d]]
softMax x = fmap (fmap (\n -> exp n / expsum)) x
    where
        expsum = sum (fmap (sum . fmap exp) x)

-- Square Error function
squareError :: (Semiring d, Fractional d) => [[d]] -> [[d]] -> d
squareError x target = vecProd vec vec / (one `plus` one)
    where
        vec = head $ transpose $ zipWith (zipWith (+)) x (fmap (fmap negate) target)

-- crossEntropy loss function
crossEntropy :: (Semiring d, Floating d) => [[d]] -> [[d]] -> d
crossEntropy x target = - (vecProd (head $ transpose (fmap (fmap log) x)) (head $ transpose target))

------- NETWORK FUNCTIONS -------

-- Function to get the expression for running the network on an input, weight matrix and bias
runNet :: (NetVar2, H.L 784 1) -> Net -> [[Expr MatrixVar]]
runNet input n = softMax (zipWith (zipWith (+)) (matrixMul (createMatrixVar (w1 n)) (createMatrixVar input)) (createMatrixVar (b1 n)))

-- Function for getting an expression of the error function
netErr :: (NetVar2, H.L 784 1) -> Net -> (NetVar2, H.L 10  1) -> Expr MatrixVar
netErr input n t = crossEntropy (runNet input n) (createMatrixVar t)

-- Function to perform 1 training step
stepNet :: (NetVar2, H.L 784 1) -> (NetVar2, H.L 10  1) -> Net -> Net
stepNet input targ net0 = net0 - 0.02 * gr
    where
        gr = backpropagationCayley (netErr input net0 targ) net0 input targ

-- function to perform multiple training steps
loopStepNet :: Int -> Net -> (NetVar2, H.L 784 1) -> (NetVar2, H.L 10 1) -> Net
loopStepNet 0 n input target = stepNet input target n
loopStepNet i n input target = stepNet input target (loopStepNet (i-1) n input target)

-- Create a Net of the gradient of the given Net for a given error function, input array and target value
backpropagationCayley :: Expr MatrixVar -> Net -> (NetVar2, H.L 784  1) -> (NetVar2, H.L 10  1) -> Net
backpropagationCayley err n input target = createNet mapDiff
    where
        diff = reverseADCayley (\x -> getValErr x n input target) err
        mapDiff = sndSP $ absDual $ absSC diff

-- Create a Net of the gradient of the given Net for a given error function, input array and target value
backpropagationForwardSparse :: Expr MatrixVar -> Net -> (NetVar2, H.L 784  1) -> (NetVar2, H.L 10  1) -> Net
backpropagationForwardSparse err n input target = createNet diffMap
    where
        diff = forwardSparseGradient (\x -> getValErr x n input target) err
        diffMap = sndSP diff

-- Function to evaluate the network error in the given input array and target value
evalNetErr :: (NetVar2, H.L 784 1) -> (NetVar2, H.L 10 1) -> Net -> Double
evalNetErr input target net = eval (\x -> getValErr x net input target) (netErr input net target)

-- function to evaluate the network in the given input array and target value
evalNet :: (NetVar2, H.L 784 1) -> Net -> H.L 10 1
evalNet input net = H.matrix $ map (eval (\x -> getVal x net input)) (head $ transpose (runNet input net))


------- MATRIX OPERATIONS FOR EXPRESSIONS -------

-- Matrix multiplication of 2 arrays of expressions
matrixMul :: Semiring d => [[d]] -> [[d]] -> [[d]]
matrixMul m1 m2 |length (head m1) == length m2 = [[vecProd i j | j <- m2T] | i <- m1]
                |otherwise = error "Matrix dimensions not matching"
    where
        m2T = transpose m2

-- Vector product between 2 arrays of expressions
vecProd :: Semiring d => [d] -> [d] -> d
vecProd [v1] [v2] = v1 `times` v2
vecProd (v1:l1) (v2:l2) = (v1 `times` v2) `plus` (vecProd l1 l2)
vecProd _ _ = error "Vector dimensions not matching"

-- *********************************************
-- Plumbing for running the network on real data
-- *********************************************

main :: IO ()
main = MWC.withSystemRandom $ \g -> do
    datadir:_ <- getArgs
    Just train <- loadMNIST (datadir </> "train-images.idx3-ubyte")
                            (datadir </> "train-labels.idx1-ubyte")
    Just test  <- loadMNIST (datadir </> "t10k-images.idx3-ubyte")
                            (datadir </> "t10k-labels.idx1-ubyte")
    putStrLn "Loaded data."
    start <- getCurrentTime
    w1 <- MWC.uniformR (-0.5, 0.5) g
    b1 <- MWC.uniformR (-0.5, 0.5) g
    -- w2 <- MWC.uniformR (-0.5, 0.5) g
    -- b2 <- MWC.uniformR (-0.5, 0.5) g
    let net0 = N (W1, w1) (B1, b1)
    flip evalStateT net0 . forM_ [1..5] $ \e -> do
        train' <- liftIO . fmap V.toList $ MWC.uniformShuffle (V.fromList train) g
        test <- liftIO . fmap V.toList $ MWC.uniformShuffle (V.fromList test) g
        liftIO $ printf "[Epoch %d]\n" (e :: Int)

        forM_ ([1..5] `zip` chunksOf 50 train') $ \(b, chnk) -> StateT $ \n0 -> do
            printf "(Batch %d)\n" (b :: Int)

            n' <- evaluate . force $ trainList chnk n0
            printf "Trained on %d points.\n" (length chnk)

            let trainScore = testNet chnk n'
                testScore  = testNet (take 100 test) n'
            printf "Training error:   %.2f%%\n" ((1 - trainScore) * 100)
            printf "Validation error: %.2f%%\n" ((1 - testScore ) * 100)

            return ((), n')
    end <- getCurrentTime
    printf "------------------\nBackprop: %s \n------------------\n" (show $ diffUTCTime end start)

trainList :: [(H.R 784, H.R 10)] -> Net -> Net
trainList = flip $ foldl' (\n (x,y) -> stepNet (I,H.col x) (T,H.col y) n)

testNet :: [(H.R 784, H.R 10)] -> Net -> Double
testNet xs n = sum (map (uncurry test) xs) / fromIntegral (length xs)
  where
    test x (H.extract->t)
        | HD.maxIndex t == HD.maxIndex (H.extract $ H.uncol r) = 1
        | otherwise                                          = 0
      where
        r = evalNet (I, H.col x) n

loadMNIST
    :: FilePath
    -> FilePath
    -> IO (Maybe [(H.R 784, H.R 10)])
loadMNIST fpI fpL = runMaybeT $ do
    i <- MaybeT          $ decodeIDXFile       fpI
    l <- MaybeT          $ decodeIDXLabelsFile fpL
    d <- MaybeT . return $ labeledIntData l i
    MaybeT . return $ for d (bitraverse mkImage mkLabel . swap)
    where
        mkImage = H.create . VG.convert . VG.map (\i -> fromIntegral i / 255)
        mkLabel n = H.create $ HD.build 10 (\i -> if round i == n then 1 else 0)

instance KnownNat n => MWC.Variate (H.R n) where
    uniform g = H.randomVector <$> MWC.uniform g <*> pure H.Uniform
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance (KnownNat m, KnownNat n) => MWC.Variate (H.L m n) where
    uniform g = H.uniformSample <$> MWC.uniform g <*> pure 0 <*> pure 1
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

-- instance MWC.Variate Net where
--     uniform g = N <$> (W1, MWC.uniform g)
--                   <*> (B1, MWC.uniform g)
--                   <*> (W2, MWC.uniform g)
--                   <*> (B2, MWC.uniform g)
--     uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance NFData NetVar2
instance NFData Net