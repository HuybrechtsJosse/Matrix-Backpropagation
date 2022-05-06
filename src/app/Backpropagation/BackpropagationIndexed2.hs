{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Backpropagation.BackpropagationIndexed2 where

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
import IndexedMatrix2 (IndexedMatrix (IM, matrix))
import IndexedSemiring (IndexedSemiring(..), IndexedExpr (..), Indexed (..), evalIndexed)

import qualified Data.Vector                     as V
import qualified Data.Vector.Generic             as VG
import qualified Numeric.LinearAlgebra           as HM
import qualified Numeric.LinearAlgebra.Data      as HD
import qualified Numeric.LinearAlgebra.Static    as H
import qualified System.Random.MWC               as MWC
import qualified System.Random.MWC.Distributions as MWC

-- Example network of n inputs to n outputs
data Net d = N {w1::d
               ,b1::d
            --    ,w2::d
            --    ,b2::d
               }
    deriving (Generic, Show, Eq)

data NetVar = I | W1 | B1 | W2 | B2 | T
    deriving (Eq, Ord, Show)

getNetExpr :: Net (IndexedExpr NetVar)
getNetExpr = N (Var W1) (Var B1)
            --    (Var W2) (Var B2)

-- Functions for network evaluation

-- SoftMax activation function
softmax :: (Floating d, Norm d) => d -> d
softmax x = exp x / norm (exp x)

-- crossEntropy loss function
crossEntropy :: (Floating d, IndexedSemiring d, HM.Transposable d d) => d -> d -> d
crossEntropy x target = negate (HM.tr (log x) `times` target)

-- Function for getting the expression of the full network
runNet :: (Floating d, IndexedSemiring d, Norm d) => d -> Net d -> d
runNet input n  = y
    where
        y = softmax ((w1 n `times` input) `plus` b1 n)
        -- z = softmax ((w2 n `times` y) `plus` b2 n)

-- Function for getting the expression of the network error
netErr :: (Floating d, IndexedSemiring d, Norm d, HM.Transposable d d) => d -> Net d -> d -> d
netErr input n = crossEntropy $ runNet input n

-- Function for 1 training step of a network with given input and target
stepNet ::  IndexedMatrix -> IndexedMatrix -> Net IndexedMatrix -> Net IndexedMatrix
stepNet input targ net0 = net0 - 0.02 * gr
    where gr = backpropagation (netErr (Var I) getNetExpr (Var T)) net0 input targ

-- Function for calculating the gradient of a network for a given error expression, network, input and target
backpropagation :: (IndexedSemiring d, Floating d, Norm d, HM.Transposable d d, Indexed d) => IndexedExpr NetVar -> Net d -> d -> d -> Net d
backpropagation err n input target = N (absBI (sndID diffW1) (one 1)) (absBI (sndID diffB1) (one 1))
                                    --    (absBI (sndID diffW2) (one 1)) (absBI (sndID diffB2) (one 1))
    where
        diffW1 = reverseADI (\x -> let {env W1 = w1 n; env B1 = b1 n; env I = input; env T = target} in env x) W1 err
        diffB1 = reverseADI (\x -> let {env W1 = w1 n; env B1 = b1 n; env I = input; env T = target} in env x) B1 err
        -- diffW2 = reverseADI (\x -> let {env W1 = w1 n; env B1 = b1 n; env W2 = w2 n; env B2 = b2 n; env I = input; env T = target} in env x) W2 err
        -- diffB2 = reverseADI (\x -> let {env W1 = w1 n; env B1 = b1 n; env W2 = w2 n; env B2 = b2 n; env I = input; env T = target} in env x) B2 err


-- Function for calculating the gradient of a network for a given error expression, network, input and target
backpropagationCayley :: (IndexedSemiring d, Floating d, Norm d, HM.Transposable d d, Indexed d) => IndexedExpr NetVar -> Net d -> d -> d -> Net d
backpropagationCayley err n input target = N (findWithDefault (zero (rows $ w1 n) (cols $ w1 n)) W1 (sndSP mapDiff)) 
                                             (findWithDefault (zero (rows $ b1 n) (cols $ b1 n)) B1 (sndSP mapDiff))
                                            --  (findWithDefault (zero (rows $ w2 n) (cols $ w2 n)) W2 (sndSP mapDiff))
                                            --  (findWithDefault (zero (rows $ b2 n) (cols $ b2 n)) B2 (sndSP mapDiff))
    where
        diff = reverseADCayleyI (\x -> let {env W1 = w1 n; env B1 = b1 n; env I = input; env T = target} in env x) err
        mapDiff = absDualI $ absSCI diff

-- Calculate the exact error of the network for given input and target
evalNetErr :: (IndexedSemiring d, Floating d, HM.Transposable d d, Norm d) => d -> d -> Net d -> d
evalNetErr input target net = evalIndexed (\x -> let {env W1 = w1 net; env B1 = b1 net; env I = input; env T = target} in env x) (netErr (Var I) getNetExpr (Var T))

-- Calculate the output of the network for a given input and target
evalNet :: (IndexedSemiring d, Floating d, HM.Transposable d d, Norm d) => d -> Net d -> d
evalNet input net = evalIndexed (\x -> let {env W1 = w1 net; env B1 = b1 net; env I = input; env T = undefined} in env x) (runNet (Var I) getNetExpr)

loopStepNet :: (KnownNat n, KnownNat m) => Int -> Net IndexedMatrix -> [(H.R n,H.R m)] -> Net IndexedMatrix
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
    (w1 :: H.L 10 784) <- MWC.uniformR (-0.5, 0.5) g
    (b1 :: H.R 10) <- MWC.uniformR (-0.5, 0.5) g
    -- (w2 :: H.L 10 250) <- MWC.uniformR (-0.5, 0.5) g
    -- (b2 :: H.R 10) <- MWC.uniformR (-0.5, 0.5) g
    let net0 = N (mkLIndexedMatrix w1) (mkRIndexedMatrix b1)
                --  (mkLIndexedMatrix w2) (mkRIndexedMatrix b2)
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
    printf "------------------\nIndexed Backpropagation 2: %s \n------------------\n" (show $ diffUTCTime end start)

trainList :: (KnownNat n, KnownNat m) => [(H.R n, H.R m)] -> Net IndexedMatrix -> Net IndexedMatrix
trainList = flip $ foldl' (\n (x,y) -> stepNet (mkRIndexedMatrix x) (mkRIndexedMatrix y) n)

mkRIndexedMatrix :: KnownNat n => H.R n -> IndexedMatrix
mkRIndexedMatrix r = mkLIndexedMatrix (H.col r)

mkLIndexedMatrix :: (KnownNat n, KnownNat m) => H.L n m -> IndexedMatrix
mkLIndexedMatrix l = IM r c (H.extract l)
    where (r,c) = H.size l

testNet :: (KnownNat n, KnownNat m) => [(H.R n, H.R m)] -> Net IndexedMatrix -> Double
testNet xs n = sum (map (uncurry test) xs) / fromIntegral (length xs)
  where
    test x (H.extract->t)
        | HM.maxIndex t == HM.maxIndex (HD.flatten $ matrix r) = 1
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

instance Num d => Num (Net d) where
    (+)         = gPlus
    (-)         = gMinus
    (*)         = gTimes
    negate      = gNegate
    abs         = gAbs
    signum      = gSignum

instance {-# OVERLAPPING #-} Fractional (Net IndexedMatrix) where
    (/)          = gDivide
    recip        = gRecip
    fromRational r = N (IM 10 784 (HD.konst (fromRational r) (10,784))) (IM 10 1 (HD.konst (fromRational r) (10,1)))
                    --    (IM 10 250 (HD.konst (fromRational r) (10,250))) (IM 10 1 (HD.konst (fromRational r) (10,1)))

instance {-# OVERLAPPABLE #-}Fractional d => Fractional (Net d) where
    (/)          = gDivide
    recip        = gRecip
    fromRational = gFromRational

instance KnownNat n => MWC.Variate (H.R n) where
    uniform g = H.randomVector <$> MWC.uniform g <*> pure H.Uniform
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance (KnownNat m, KnownNat n) => MWC.Variate (H.L m n) where
    uniform g = H.uniformSample <$> MWC.uniform g <*> pure 0 <*> pure 1
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance NFData IndexedMatrix
instance NFData d => NFData (Net d)