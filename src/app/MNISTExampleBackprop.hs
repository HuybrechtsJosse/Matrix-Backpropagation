{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE FlexibleInstances #-}

module MNISTExampleBackprop where

------ IMPORTS ------
import Control.DeepSeq ( NFData, force )
import Control.Exception ( evaluate )
import Control.Lens ( makeLenses )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Trans.Maybe ( MaybeT(MaybeT, runMaybeT) )
import Control.Monad.Trans.State ( StateT(StateT), evalStateT )
import Data.Bitraversable ( Bitraversable(bitraverse) )
import Data.Foldable ( Foldable(foldl'), forM_ )
import Data.IDX ( decodeIDXFile, decodeIDXLabelsFile, labeledIntData )
import Data.List.Split ( chunksOf )
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Traversable ( for )
import Data.Tuple ( swap )
import GHC.Generics (Generic)
import GHC.TypeLits ( KnownNat )
import Numeric.OneLiner (gPlus, gMinus, gTimes, gNegate, gAbs, gSignum, gFromInteger, gDivide, gRecip, gFromRational)
import System.Environment ( getArgs )
import System.FilePath ( (</>) )
import Text.Printf ( printf )

import Numeric.Backprop ( Backprop, Reifies, W, BVar, (^^.), gradBP, evalBP, constVar )
import Numeric.LinearAlgebra.Static.Backprop ( L, R, (#>), (<.>), konst, norm_1V )

import qualified Data.Vector                     as V
import qualified Data.Vector.Generic             as VG
import qualified Numeric.LinearAlgebra           as HM
import qualified Numeric.LinearAlgebra.Static    as H
import qualified System.Random.MWC               as MWC
import qualified System.Random.MWC.Distributions as MWC

data Net = N { _weights1 :: L 50 784
             , _bias1    :: R 50
             , _weights2 :: L 20 50
             , _bias2    :: R 20
             , _weights3 :: L 20 20
             , _bias3    :: R 20
             , _weights4 :: L 20 20
             , _bias4    :: R 20
             , _weights5 :: L 10 20
             , _bias5    :: R 10
             }
  deriving (Generic, Show)

instance Backprop Net

makeLenses ''Net

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

softMax
    :: (Reifies s W, KnownNat n)
    => BVar s (R n)
    -> BVar s (R n)
softMax x = expx / konst (norm_1V expx)
  where
    expx = exp x

crossEntropy
    :: (Reifies s W, KnownNat n)
    => BVar s (R n)
    -> BVar s (R n)
    -> BVar s Double
crossEntropy targ res = -(log res <.> targ)

runNet
    :: Reifies s W
    => BVar s Net
    -> BVar s (R 784)
    -> BVar s (R 10)
runNet n x = o5
  where
    o1 = logistic $ (n ^^. weights1) #> x  + (n ^^. bias1)
    o2 = logistic $ (n ^^. weights2) #> o1 + (n ^^. bias2)
    o3 = logistic $ (n ^^. weights3) #> o2 + (n ^^. bias3)
    o4 = logistic $ (n ^^. weights4) #> o3 + (n ^^. bias4)
    o5 = softMax $ (n ^^. weights5) #> o4 + (n ^^. bias5)

netErr
    :: Reifies s W
    => BVar s (R 784)
    -> BVar s (R 10)
    -> BVar s Net
    -> BVar s Double
netErr x targ n = crossEntropy targ (runNet n x)

stepNet :: R 784 -> R 10 -> Net -> Net
stepNet x targ net0 = net0 - 0.02 * gr
  where
    gr :: Net
    gr = gradBP (netErr (constVar x) (constVar targ)) net0

-- *********************************************
-- Plumbing for running the network on real data
-- *********************************************
instance Show (BVar s Double)

main :: IO ()
main = MWC.withSystemRandom $ \g -> do
    datadir:_ <- getArgs
    Just train <- loadMNIST (datadir </> "train-images.idx3-ubyte")
                            (datadir </> "train-labels.idx1-ubyte")
    Just test  <- loadMNIST (datadir </> "t10k-images.idx3-ubyte")
                            (datadir </> "t10k-labels.idx1-ubyte")
    putStrLn "Loaded data."
    start <- getCurrentTime
    net0 <- MWC.uniformR (-0.5, 0.5) g
    flip evalStateT net0 . forM_ [1..5] $ \e -> do
      train' <- liftIO . fmap V.toList $ MWC.uniformShuffle (V.fromList train) g
      liftIO $ printf "[Epoch %d]\n" (e :: Int)

      forM_ ([1..5] `zip` chunksOf 5000 train') $ \(b, chnk) -> StateT $ \n0 -> do
        printf "(Batch %d)\n" (b :: Int)

        n' <- evaluate . force $ trainList chnk n0
        printf "Trained on %d points.\n" (length chnk)

        let trainScore = testNet chnk n'
            testScore  = testNet test n'
        -- appendFile "Results/temp1.txt"  (show ((1 - trainScore) * 100) ++ ",")
        -- appendFile "Results/temp2.txt"  (show ((1 - testScore) * 100) ++ ",")
        printf "Training error:   %.2f%%\n" ((1 - trainScore) * 100)
        printf "Validation error: %.2f%%\n" ((1 - testScore ) * 100)

        return ((), n')
    end <- getCurrentTime
    -- appendFile "Results/resultsBackprop.txt" "Backprop example 1 hidden (250) layers:\n"
    -- appendFile "Results/resultsBackprop.txt" "    trainError: []\n"
    -- appendFile "Results/resultsBackprop.txt" "    testError: []\n"
    -- appendFile "Results/resultsBackprop.txt" ("    execution time: " ++ show (diffUTCTime end start) ++ "\n\n")
    printf "------------------\nBackprop: %s \n------------------\n" (show $ diffUTCTime end start)

trainList :: [(R 784, R 10)] -> Net -> Net
trainList = flip $ foldl' (\n (x,y) -> stepNet x y n)

testNet :: [(R 784, R 10)] -> Net -> Double
testNet xs n = sum (map (uncurry test) xs) / fromIntegral (length xs)
  where
    test x (H.extract->t)
        | HM.maxIndex t == HM.maxIndex (H.extract r) = 1
        | otherwise                                  = 0
      where
        r = evalBP (`runNet` constVar x) n

loadMNIST
    :: FilePath
    -> FilePath
    -> IO (Maybe [(R 784, R 10)])
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
    fromInteger = gFromInteger

instance Fractional Net where
    (/)          = gDivide
    recip        = gRecip
    fromRational = gFromRational

instance KnownNat n => MWC.Variate (R n) where
    uniform g = H.randomVector <$> MWC.uniform g <*> pure H.Uniform
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance (KnownNat m, KnownNat n) => MWC.Variate (L m n) where
    uniform g = H.uniformSample <$> MWC.uniform g <*> pure 0 <*> pure 1
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance MWC.Variate Net where
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

instance NFData Net