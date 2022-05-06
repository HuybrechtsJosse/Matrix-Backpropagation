{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Backpropagation.BackpropagationIndexed1 where

------ IMPORTS ------
import Control.Exception (evaluate)
import Control.DeepSeq (NFData, force)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Control.Monad.Trans.State (evalStateT, StateT (StateT))
import Data.Bitraversable (Bitraversable(bitraverse))
import Data.Foldable (Foldable(foldl'), forM_)
import Data.List.Split (chunksOf)
import Data.Map ((!), findWithDefault)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Traversable (for)
import Data.Tuple (swap)
import Data.IDX (labeledIntData, decodeIDXLabelsFile, decodeIDXFile)
import GHC.TypeLits (KnownNat)
import GHC.Generics (Generic)
import Numeric.OneLiner (gPlus, gMinus, gTimes, gNegate, gAbs, gSignum, gFromInteger, gDivide, gRecip, gFromRational)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Printf (printf)

import AD.ForwardMode (Norm (..), SDual (sndSP))
import AD.ReverseModeIndexed (IndexedDualR(sndID), absBI, reverseADI, reverseADCayleyI, absSCI, absDualI)
import IndexedSemiring (IndexedSemiring(..), IndexedExpr (..), Indexed (..), evalIndexed)

import qualified Data.Vector                     as V
import qualified Data.Vector.Generic             as VG
import qualified Numeric.LinearAlgebra           as HM
import qualified Numeric.LinearAlgebra.Data      as HD
import qualified Numeric.LinearAlgebra.Static    as H
import qualified System.Random.MWC               as MWC
import qualified System.Random.MWC.Distributions as MWC
import IndexedMatrix1 (IndexedMatrix (matrix, IM))
import Matrix (konst)

-- Example network of n inputs to n outputs
data Net = N {w::IndexedMatrix, b::IndexedMatrix }
    deriving (Generic, Show, Eq)

data NetVar = I | W | B | T
    deriving (Eq, Ord, Show)

-- Functions for network evaluation
correctDimensions :: IndexedMatrix -> [IndexedMatrix] -> [IndexedMatrix] -> IndexedMatrix -> Bool
correctDimensions input weights biases target = correctInput && correctWeightsBiases weights biases && correctBiases && correctWeightsBiases2 (tail weights) biases && correctTarget
    where
        -- Input has the same amout of rows as the first weight has columns and has only 1 column
        correctInput = cols (head weights) == rows input && cols input == 1
        -- Weights have the same amount of rows as the corresponding bias has rows
        correctWeightBias w b = rows w == rows b
        correctWeightsBiases [w] [b] = correctWeightBias w b
        correctWeightsBiases (w:wl) (b:bl) = correctWeightBias w b && correctWeightsBiases wl bl
        correctWeightsBiases _ _ = False
        correctBiases = all (\b -> cols b == 1) biases
        -- weights have the same amount of columns as the previous bias has rows
        correctWeightBias2 w b = cols w == rows b
        correctWeightsBiases2 [] [b] = True
        correctWeightsBiases2 (w:wl) (b:bl) = correctWeightBias2 w b && correctWeightsBiases2 wl bl
        correctWeightsBiases2 _ _ = False
        -- Target has the same amount of rows as the last bias and has only 1 column
        correctTarget = rows (last biases) == rows target && cols target == 1

-- SoftMax activation function
softmax :: (Floating d, Norm d) => d -> d
softmax x = exp x / norm (exp x)

-- crossEntropy loss function
crossEntropy :: (Floating d, IndexedSemiring d, HM.Transposable d d) => d -> d -> d
crossEntropy x target = negate (HM.tr (log x) `times` target)

-- Function for getting the expression of the full network
runNet :: (Floating d, IndexedSemiring d, Norm d) => d -> d -> d -> d
runNet input w b  = y
    where
        -- run first layer
        y = softmax ((w `times` input) `plus` b)

-- Function for getting the expression of the network error
netErr :: (Floating d, IndexedSemiring d, Norm d, HM.Transposable d d) => d -> d -> d -> d -> d
netErr input w b = crossEntropy $ runNet input w b

-- Function for 1 training step of a network with given input and target
stepNet :: IndexedMatrix -> IndexedMatrix -> Net -> Net
stepNet input targ net0 = net0 - 0.02 * gr
    where gr = backpropagation (netErr (Var I) (Var W) (Var B) (Var T)) net0 input targ

-- Function for calculating the gradient of a network for a given error expression, network, input and target
backpropagation :: IndexedExpr NetVar -> Net -> IndexedMatrix -> IndexedMatrix -> Net
backpropagation err n input target = N (findWithDefault (zero (rows $ w n) (cols $ w n)) W (sndSP mapDiff)) (findWithDefault (zero (rows $ b n) (cols $ b n)) B (sndSP mapDiff))
    where
        diff = reverseADCayleyI (\x -> let {env W = w n; env B = b n; env I = input; env T = target} in env x) err
        mapDiff = absDualI $ absSCI diff

-- Calculate the exact error of the network for given input and target
evalNetErr :: IndexedMatrix -> IndexedMatrix -> Net -> IndexedMatrix
evalNetErr input target net = evalIndexed (\x -> let {env W = w net; env B = b net; env I = input; env T = target} in env x) (netErr (Var I) (Var W) (Var B) (Var T))

-- Calculate the output of the network for a given input and target
evalNet :: IndexedMatrix -> Net -> IndexedMatrix
evalNet input net = evalIndexed (\x -> let {env W = w net; env B = b net; env I = input; env T = undefined} in env x) (runNet (Var I) (Var W) (Var B))

loopStepNet :: (KnownNat n, KnownNat m) => Int -> Net -> [(H.R n,H.R m)] -> Net
loopStepNet 0 n dataList = trainList dataList n
loopStepNet i n dataList = trainList dataList (loopStepNet (i-1) n dataList)


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
    (w :: H.L 10 784) <- MWC.uniformR (-0.5, 0.5) g
    (b :: H.R 10) <- MWC.uniformR (-0.5, 0.5) g
    let wMatrix = mkLIndexedMatrix w
    let bMatrix = mkRIndexedMatrix b
    let net0 = N wMatrix bMatrix
    if not $ correctDimensions (mkRIndexedMatrix $ fst $ head train) [wMatrix] [bMatrix] (mkRIndexedMatrix $ snd $ head train)
        then error "Network dimensions not correct"
        else do
            flip evalStateT net0 . forM_ [1..5] $ \e -> do
                train' <- liftIO . fmap V.toList $ MWC.uniformShuffle (V.fromList train) g
                liftIO $ printf "[Epoch %d]\n" (e :: Int)

                forM_ ([1..5] `zip` chunksOf 5000 train') $ \(b, chnk) -> StateT $ \n0 -> do
                    printf "(Batch %d)\n" (b :: Int)

                    n' <- evaluate . force $ trainList chnk n0
                    printf "Trained on %d points.\n" (length chnk)

                    let trainScore = testNet chnk n'
                        testScore  = testNet test n'
                    printf "Training error:   %.2f%%\n" ((1 - trainScore) * 100)
                    printf "Validation error: %.2f%%\n" ((1 - testScore ) * 100)

                    return ((), n')
            end <- getCurrentTime
            printf "------------------\nIndexed Backpropagation 1: %s \n------------------\n" (show $ diffUTCTime end start)

trainList :: (KnownNat n, KnownNat m) => [(H.R n, H.R m)] -> Net -> Net
trainList = flip $ foldl' (\n (x,y) -> stepNet (mkRIndexedMatrix x) (mkRIndexedMatrix y) n)

mkRIndexedMatrix :: KnownNat n => H.R n -> IndexedMatrix
mkRIndexedMatrix r = mkLIndexedMatrix (H.col r)

mkLIndexedMatrix :: (KnownNat n, KnownNat m) => H.L n m -> IndexedMatrix
mkLIndexedMatrix l = IM r c (HD.toLists $ H.extract l)
    where (r,c) = H.size l

testNet :: (KnownNat n, KnownNat m) => [(H.R n, H.R m)] -> Net -> Double
testNet xs n = sum (map (uncurry test) xs) / fromIntegral (length xs)
  where
    test x (H.extract->t)
        | HM.maxIndex t == HM.maxIndex (HD.flatten $ HD.fromLists $ matrix r) = 1
        | otherwise = 0
      where
        r = evalNet (mkRIndexedMatrix x) n


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
    mkLabel n = H.create $ HM.build 10 (\i -> if round i == n then 1 else 0)

instance Num Net where
    (+)         = gPlus
    (-)         = gMinus
    (*)         = gTimes
    negate      = gNegate
    abs         = gAbs
    signum      = gSignum

instance Fractional Net where
    (/)          = gDivide
    recip        = gRecip
    fromRational r = N (IM 10 784 (konst (fromRational r) (10,784))) (IM 10 1 (konst (fromRational r) (10,1)))

instance KnownNat n => MWC.Variate (H.R n) where
    uniform g = H.randomVector <$> MWC.uniform g <*> pure H.Uniform
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance (KnownNat m, KnownNat n) => MWC.Variate (H.L m n) where
    uniform g = H.uniformSample <$> MWC.uniform g <*> pure 0 <*> pure 1
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance NFData IndexedMatrix
instance NFData Net