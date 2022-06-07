{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
module Backpropagation.BackpropagationIndexedIO where

------ IMPORTS ------
import Control.Exception (evaluate)
import Control.DeepSeq (NFData, force)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Control.Monad.Trans.State (evalStateT, StateT (StateT))
import Data.Array.IO (Ix)
import Data.Bitraversable (Bitraversable(bitraverse))
import Data.Foldable (Foldable(foldl'), forM_)
import Data.List.Split (chunksOf)
import Data.Map (findWithDefault)
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

import AD.ForwardMode (Norm (..))
import AD.ReverseModeIndexed (IndexedDualR(sndID), absBI, reverseADI, absSCI, absDualI, reverseADIOIndexed)
import IndexedMatrix (IndexedMatrix (IM, matrix))
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
               ,w2::d
               ,b2::d
               ,w3::d
               ,b3::d
               ,w4::d
               ,b4::d
               ,w5::d
               ,b5::d
               }
    deriving (Generic, Show, Eq)

data NetVar = I | W1 | B1 | W2 | B2 | W3 | B3 | W4 | B4 | W5 | B5 | T
    deriving (Eq, Ord, Show, Ix)

getNetExpr :: Net (IndexedExpr NetVar)
getNetExpr = N (Var W1) (Var B1)
               (Var W2) (Var B2)
               (Var W3) (Var B3)
               (Var W4) (Var B4)
               (Var W5) (Var B5)

-- Functions for network evaluation

-- Logistic activation function
logistic :: (Floating d, Indexed d) => d -> d
logistic x = (fromInt 1 x) / ((fromInt 1 x) + exp (negate x))

-- SoftMax activation function
softmax :: (Floating d, Norm d) => d -> d
softmax x = exp x / norm (exp x)

-- crossEntropy loss function
crossEntropy :: (Floating d, IndexedSemiring d, HM.Transposable d d) => d -> d -> d
crossEntropy x target = negate (HM.tr (log x) `times` target)

-- Function for getting the expression of the full network
runNet :: (Floating d, IndexedSemiring d, Norm d, Indexed d) => d -> Net d -> d
runNet input n  = o5
    where
        o1 = logistic ((w1 n `times` input) `plus` b1 n)
        o2 = logistic ((w2 n `times` o1) `plus` b2 n)
        o3 = logistic ((w3 n `times` o2) `plus` b3 n)
        o4 = logistic ((w4 n `times` o3) `plus` b4 n)
        o5 = softmax ((w5 n `times` o4) `plus` b5 n)

-- Function for getting the expression of the network error
netErr :: (Floating d, IndexedSemiring d, Norm d, HM.Transposable d d, Indexed d) => d -> Net d -> d -> d
netErr input n = crossEntropy $ runNet input n

-- Function for 1 training step of a network with given input and target
stepNet ::  IndexedMatrix -> IndexedMatrix -> Net IndexedMatrix -> IO (Net IndexedMatrix)
stepNet input targ net0 = do
    gr <- backpropagationIO (netErr (Var I) getNetExpr (Var T)) net0 input targ
    return (net0 - 0.02 * gr)

-- Function for calculating the gradient of a network for a given error expression, network, input and target
backpropagationIO :: (IndexedSemiring d, Floating d, Norm d, HM.Transposable d d, Indexed d, Show d) => IndexedExpr NetVar -> Net d -> d -> d -> IO (Net d)
backpropagationIO err n input target = do
        mapDiff <- reverseADIOIndexed (I,T) env err
        return (N   (findWithDefault (zero (rows $ w1 n) (cols $ w1 n)) W1 mapDiff)
                    (findWithDefault (zero (rows $ b1 n) (cols $ b1 n)) B1 mapDiff)
                    (findWithDefault (zero (rows $ w2 n) (cols $ w2 n)) W2 mapDiff)
                    (findWithDefault (zero (rows $ b2 n) (cols $ b2 n)) B2 mapDiff)
                    (findWithDefault (zero (rows $ w3 n) (cols $ w3 n)) W3 mapDiff)
                    (findWithDefault (zero (rows $ b3 n) (cols $ b3 n)) B3 mapDiff)
                    (findWithDefault (zero (rows $ w4 n) (cols $ w4 n)) W4 mapDiff)
                    (findWithDefault (zero (rows $ b4 n) (cols $ b4 n)) B4 mapDiff)
                    (findWithDefault (zero (rows $ w5 n) (cols $ w5 n)) W5 mapDiff)
                    (findWithDefault (zero (rows $ b5 n) (cols $ b5 n)) B5 mapDiff)
               )
    where
        env W1 = w1 n
        env B1 = b1 n
        env W2 = w2 n
        env B2 = b2 n
        env W3 = w3 n
        env B3 = b3 n
        env W4 = w4 n
        env B4 = b4 n
        env W5 = w5 n
        env B5 = b5 n
        env I = input
        env T = target

-- Calculate the exact error of the network for given input and target
evalNetErr :: (IndexedSemiring d, Floating d, HM.Transposable d d, Norm d, Indexed d) => d -> d -> Net d -> d
evalNetErr input target n = evalIndexed env (netErr (Var I) getNetExpr (Var T))
    where
        env W1 = w1 n
        env B1 = b1 n
        env W2 = w2 n
        env B2 = b2 n
        env W3 = w3 n
        env B3 = b3 n
        env W4 = w4 n
        env B4 = b4 n
        env W5 = w5 n
        env B5 = b5 n
        env I = input
        env T = target

-- Calculate the output of the network for a given input and target
evalNet :: (IndexedSemiring d, Floating d, HM.Transposable d d, Norm d, Indexed d) => d -> Net d -> d
evalNet input n = evalIndexed env (runNet (Var I) getNetExpr)
    where
        env W1 = w1 n
        env B1 = b1 n
        env W2 = w2 n
        env B2 = b2 n
        env W3 = w3 n
        env B3 = b3 n
        env W4 = w4 n
        env B4 = b4 n
        env W5 = w5 n
        env B5 = b5 n
        env I = input
        env T = undefined

loopStepNet :: (KnownNat n, KnownNat m) => Int -> Net IndexedMatrix -> [(H.R n,H.R m)] -> IO (Net IndexedMatrix)
loopStepNet 0 n dataList = trainList dataList (return n)
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
    (w1 :: H.L 50 784) <- MWC.uniformR (-0.5, 0.5) g
    (b1 :: H.R 50) <- MWC.uniformR (-0.5, 0.5) g
    (w2 :: H.L 20 50) <- MWC.uniformR (-0.5, 0.5) g
    (b2 :: H.R 20) <- MWC.uniformR (-0.5, 0.5) g
    (w3 :: H.L 20 20) <- MWC.uniformR (-0.5, 0.5) g
    (b3 :: H.R 20) <- MWC.uniformR (-0.5, 0.5) g
    (w4 :: H.L 20 20) <- MWC.uniformR (-0.5, 0.5) g
    (b4 :: H.R 20) <- MWC.uniformR (-0.5, 0.5) g
    (w5 :: H.L 10 20) <- MWC.uniformR (-0.5, 0.5) g
    (b5 :: H.R 10) <- MWC.uniformR (-0.5, 0.5) g
    let net0 = N (mkLIndexedMatrix w1) (mkRIndexedMatrix b1)
                 (mkLIndexedMatrix w2) (mkRIndexedMatrix b2)
                 (mkLIndexedMatrix w3) (mkRIndexedMatrix b3)
                 (mkLIndexedMatrix w4) (mkRIndexedMatrix b4)
                 (mkLIndexedMatrix w5) (mkRIndexedMatrix b5)
    
    flip evalStateT net0 . forM_ [1..5] $ \e -> do
      train' <- liftIO . fmap V.toList $ MWC.uniformShuffle (V.fromList train) g
      liftIO $ printf "[Epoch %d]\n" (e :: Int)

      forM_ ([1..5] `zip` chunksOf 5000 train') $ \(b, chnk) -> StateT $ \n0 -> do
        printf "(Batch %d)\n" (b :: Int)

        n0' <- trainList chnk (return n0)
        n' <- evaluate . force $ n0'
        printf "Trained on %d points.\n" (length chnk)

        let trainScore = testNet chnk n'
            testScore  = testNet test n'
        printf "Training error:   %.2f%%\n" ((1 - trainScore) * 100)
        printf "Validation error: %.2f%%\n" ((1 - testScore ) * 100)

        return ((), n')
    end <- getCurrentTime
    printf "------------------\nIndexed Backpropagation Mutable Arrays: %s \n------------------\n" (show $ diffUTCTime end start)

trainList :: (KnownNat n, KnownNat m) => [(H.R n, H.R m)] -> IO (Net IndexedMatrix) -> IO (Net IndexedMatrix)
trainList = flip $ foldl' (\n (x,y) -> do 
    n' <- n
    stepNet (mkRIndexedMatrix x) (mkRIndexedMatrix y) n')

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
    fromRational r = N (IM 50 784 (HD.konst (fromRational r) (50,784))) (IM 50 1 (HD.konst (fromRational r) (50,1)))
                       (IM 20 50 (HD.konst (fromRational r) (20,50))) (IM 20 1 (HD.konst (fromRational r) (20,1)))
                       (IM 20 20 (HD.konst (fromRational r) (20,20))) (IM 20 1 (HD.konst (fromRational r) (20,1)))
                       (IM 20 20 (HD.konst (fromRational r) (20,20))) (IM 20 1 (HD.konst (fromRational r) (20,1)))
                       (IM 10 20 (HD.konst (fromRational r) (10,20))) (IM 10 1 (HD.konst (fromRational r) (10,1)))

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