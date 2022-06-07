{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}

module Backpropagation.BackpropagationIndexedStatic where

------ IMPORTS ------
import Control.Exception (evaluate)
import Control.DeepSeq (NFData, force)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Control.Monad.Trans.State (evalStateT, StateT (StateT))
import Data.Bitraversable (Bitraversable(bitraverse))
import Data.Foldable (Foldable(foldl'), forM_)
import Data.List.Split (chunksOf)
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

import IndexedSemiringStatic (IndexedSemiringStatic(..), IndexedExprStatic(..), evalStatic, NetVar (..))
import AD.ReverseModeStatic (reverseADStatic, absBI, StaticDualR (sndSD))

import qualified Data.Vector                     as V
import qualified Data.Vector.Generic             as VG
import qualified Numeric.LinearAlgebra           as HM
import qualified Numeric.LinearAlgebra.Data      as HD
import qualified Numeric.LinearAlgebra.Static    as H
import qualified System.Random.MWC               as MWC
import qualified System.Random.MWC.Distributions as MWC
import AD.ForwardMode (Norm(..))

-- Example network of n inputs to n outputs

data Net d = N {w1::d 50 784, b1::d 50 1
                  ,w2::d 20 50,  b2::d 20 1
                  ,w3::d 20 20,  b3::d 20 1
                  ,w4::d 20 20,  b4::d 20 1
                  ,w5::d 10 20,  b5::d 10 1
                  }
    deriving (Generic)

getNetExpr :: Net IndexedExprStatic
getNetExpr = N (Var W1) (Var B1)
                (Var W2) (Var B2)
                (Var W3) (Var B3)
                (Var W4) (Var B4)
                (Var W5) (Var B5)

-- Functions for network evaluation

-- Logistic activation function
logistic :: (Floating d) => d -> d
logistic x = 1 / (1 + exp (negate x))

-- SoftMax activation function
softmax :: (Floating (d n l), KnownNat n, KnownNat l, Norm (d n l)) => d n l -> d n l
softmax x = exp x / norm (exp x)

-- crossEntropy loss function
crossEntropy :: (KnownNat n) => IndexedExprStatic n 1 -> IndexedExprStatic n 1 ->  IndexedExprStatic 1 1
crossEntropy x target = negate (HM.tr (log x) `times` target)

-- Function for getting the expression of the full network
runNet :: IndexedExprStatic 784 1 -> Net IndexedExprStatic -> IndexedExprStatic 10 1
runNet input n  = o5
    where
        o1 = logistic ((w1 n `times` input) `plus` b1 n)
        o2 = logistic ((w2 n `times` o1) `plus` b2 n)
        o3 = logistic ((w3 n `times` o2) `plus` b3 n)
        o4 = logistic ((w4 n `times` o3) `plus` b4 n)
        o5 = softmax  ((w5 n `times` o4) `plus` b5 n)

-- Function for getting the expression of the network error
netErr :: IndexedExprStatic 784 1 -> Net IndexedExprStatic -> IndexedExprStatic 10 1 -> IndexedExprStatic 1 1
netErr input n = crossEntropy $ runNet input n

-- Function for 1 training step of a network with given input and target
stepNet ::  H.L 784 1 -> H.L 10 1 -> Net H.L -> Net H.L
stepNet input targ net0 = net0 - 0.02 * gr
    where gr = backpropagation (netErr (Var I) getNetExpr (Var T)) net0 input targ

-- Function for calculating the gradient of a network for a given error expression, network, input and target
backpropagation :: IndexedExprStatic 1 1 -> Net H.L -> H.L 784 1 -> H.L 10 1 -> Net H.L
backpropagation err n input target = N (absBI $ sndSD diffW1) (absBI $ sndSD diffB1)
                                       (absBI $ sndSD diffW2) (absBI $ sndSD diffB2)
                                       (absBI $ sndSD diffW3) (absBI $ sndSD diffB3)
                                       (absBI $ sndSD diffW4) (absBI $ sndSD diffB4)
                                       (absBI $ sndSD diffW5) (absBI $ sndSD diffB5)
    where
        diffW1 = reverseADStatic env W1 err
        diffB1 = reverseADStatic env B1 err
        diffW2 = reverseADStatic env W2 err
        diffB2 = reverseADStatic env B2 err
        diffW3 = reverseADStatic env W3 err
        diffB3 = reverseADStatic env B3 err
        diffW4 = reverseADStatic env W4 err
        diffB4 = reverseADStatic env B4 err
        diffW5 = reverseADStatic env W5 err
        diffB5 = reverseADStatic env B5 err
        env :: NetVar n m -> H.L n m
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
evalNetErr :: H.L 784 1 -> H.L 10 1 -> Net H.L -> H.L 1 1
evalNetErr input target n = evalStatic env (netErr (Var I) getNetExpr (Var T))
    where
        env :: NetVar n m -> H.L n m
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
evalNet :: H.L 784 1 -> Net H.L -> H.L 10 1
evalNet input n = evalStatic env (runNet (Var I) getNetExpr)
    where
        env :: NetVar n m -> H.L n m
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
    (net0::Net H.L) <- MWC.uniformR (-0.5, 0.5) g

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
    printf "------------------\nIndexed Backpropagation Static: %s \n------------------\n" (show $ diffUTCTime end start)

trainList :: [(H.R 784, H.R 10)] -> Net H.L -> Net H.L
trainList = flip $ foldl' (\n (x,y) -> stepNet (H.col x) (H.col y) n)

testNet :: [(H.R 784, H.R 10)] -> Net H.L -> Double
testNet xs n = sum (map (uncurry test) xs) / fromIntegral (length xs)
  where
    test x (H.extract->t)
        | HM.maxIndex t == HM.maxIndex (H.extract $ H.uncol r) = 1
        | otherwise = 0
      where
        r = evalNet (H.col x) n


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

instance (forall a b.(KnownNat a, KnownNat b) => Num (d a b)) => Num (Net d) where
    (+)         = gPlus
    (-)         = gMinus
    (*)         = gTimes
    negate      = gNegate
    abs         = gAbs
    signum      = gSignum
    fromInteger = gFromInteger

instance (forall a b.(KnownNat a, KnownNat b) => Fractional (d a b)) => Fractional (Net d) where
    (/)          = gDivide
    recip        = gRecip
    fromRational = gFromRational

instance KnownNat n => MWC.Variate (H.R n) where
    uniform g = H.randomVector <$> MWC.uniform g <*> pure H.Uniform
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance (KnownNat m, KnownNat n) => MWC.Variate (H.L m n) where
    uniform g = H.uniformSample <$> MWC.uniform g <*> pure 0 <*> pure 1
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance (forall a b.(KnownNat a, KnownNat b) => MWC.Variate (d a b), forall a b.(KnownNat a, KnownNat b) => Num (d a b)) => MWC.Variate (Net d) where
    uniform g = N <$> MWC.uniform g
                  <*> MWC.uniform g
                  <*> MWC.uniform g
                  <*> MWC.uniform g
                  <*> MWC.uniform g
                  <*> MWC.uniform g
                  <*> MWC.uniform g
                  <*> MWC.uniform g
                  <*> MWC.uniform g
                  <*> MWC.uniform g
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance (forall a b.(KnownNat a, KnownNat b) => NFData (d a b)) => NFData (Net d)