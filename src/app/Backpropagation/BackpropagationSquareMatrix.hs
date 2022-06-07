{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Backpropagation.BackpropagationSquareMatrix where

------ IMPORTS ------
import Control.DeepSeq ( NFData, force )
import Control.Exception ( evaluate )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Trans.Maybe ( MaybeT(MaybeT, runMaybeT) )
import Control.Monad.Trans.State ( StateT(StateT), evalStateT )
import Data.Bitraversable ( Bitraversable(bitraverse) )
import Data.Foldable ( Foldable(foldl'), forM_ )
import Data.IDX ( decodeIDXFile, decodeIDXLabelsFile, labeledIntData )
import Data.List.Split ( chunksOf )
import Data.Map (Map, findWithDefault)
import Data.Traversable ( for )
import Data.Tuple ( swap )
import GHC.Float (int2Double)
import GHC.Generics ( Generic )
import GHC.TypeLits ( KnownNat )
import Numeric.OneLiner (gPlus, gMinus, gTimes, gNegate, gAbs, gSignum, gFromInteger, gDivide, gRecip, gFromRational)
import Prelude hiding ((<>), (++), (**))
import System.Environment ( getArgs )
import System.FilePath ( (</>) )
import Text.Printf ( printf )

import AD.ForwardMode (Semiring(..), Norm(..), Expr(..), eval)
import AD.NCReverseMode (NCDualR (NCD, sndNCD), reverseADNC, absB, ddotplus, actR, actL)

import Numeric.LinearAlgebra.Data ((!))
import qualified Data.Vector                     as V
import qualified Data.Vector.Generic             as VG
import qualified Numeric.LinearAlgebra           as HM
import qualified Numeric.LinearAlgebra.Static    as H
import qualified System.Random.MWC               as MWC
import qualified System.Random.MWC.Distributions as MWC
import IndexedSemiring (Indexed(..))


-- Example network of n inputs to n outputs
data Net n = N {w::H.Sq n, b::H.Sq n}
    deriving (Generic, Show, Eq)

instance KnownNat n => Eq (H.Sq n) where
  (==) s1 s2 = H.extract s1 == H.extract s2

instance KnownNat n => Semiring (H.Sq n) where
    zero = 0
    one = H.eye
    plus = (+)
    times = (H.<>)

-- Instance should only be used for vectors in a square matrix representation
instance KnownNat n => Norm (H.Sq n) where
    norm sq = 1 `times` sq

-- function to get the dimension of the square matrices of the input network
getDim :: KnownNat n => Net n -> Int
getDim n = fst $ H.size $ w n

data NetVar = I | W | B | T
    deriving (Eq, Ord)

-- Functions for network evaluation

-- SoftMax activation function
softmax :: (Floating d, Norm d) => d -> d
softmax x = exp x / norm (exp x)

-- SquareError loss function
squareError :: (Semiring d, Num d, HM.Transposable d d) => d -> d -> d
squareError x target = H.tr mat `times` mat
    where
        mat = x `plus` negate target

-- Function for getting the expression of the full network
runNet :: (Floating d, Semiring d, Norm d) => d -> d -> d -> d
runNet input w b  = y
    where
        -- run first layer
        y = softmax ((w `times` input) `plus` b)

-- Function for getting the expression of the network error
netErr :: (Floating d, Semiring d, Norm d, HM.Transposable d d) => d -> d -> d -> d -> d
netErr input w b = squareError $ runNet input w b

-- Function for 1 training step of a network with given input and target
stepNet :: KnownNat n => H.Sq n -> H.Sq n -> Net n -> Net n
stepNet input targ net0 = net0 - 0.02 * gr
    where gr = backpropagation (netErr (Var I) (Var W) (Var B) (Var T)) net0 input targ

-- Function to perform multiple training steps on 1 input and target
loopStepNet :: KnownNat n => Int -> Net n -> [(H.R n, H.R n)] -> Net n
loopStepNet 0 n dataList = trainList dataList n
loopStepNet i n dataList = trainList dataList (loopStepNet (i-1) n dataList)

-- Function for calculating the gradient of a network for a given error expression, network, input and target
backpropagation :: KnownNat n => Expr NetVar -> Net n -> H.Sq n -> H.Sq n -> Net n
backpropagation err n input target = N (absB (sndNCD diffW) / H.konst (int2Double $ getDim n)) (absB $ sndNCD diffB)
    where
        diffW = reverseADNC (\x -> let {env W = w n; env B = b n; env I = input; env T = target} in env x) W err
        diffB = reverseADNC (\x -> let {env W = w n; env B = b n; env I = input; env T = target} in env x) B err

-- Calculate the exact error of the network for given input and target
evalNetErr :: KnownNat n => H.Sq n -> H.Sq n -> Net n -> H.Sq n
evalNetErr input target net = eval (\x -> let {env W = w net; env B = b net; env I = input; env T = target} in env x) (netErr (Var I) (Var W) (Var B) (Var T))

averageError :: KnownNat n => [(H.R n, H.R n)] -> Net n -> Double
averageError dataList n = sum (map (\(i,t) -> H.extract (evalNetErr (mkRSquare (getDim n) i) (mkRSquare (getDim n) t) n) ! 0 ! 0) dataList)/int2Double (getDim n)

-- Calculate the output of the network for a given input and target
evalNet :: KnownNat n => H.Sq n -> Net n -> H.Sq n
evalNet input net = eval (\x -> let {env W = w net; env B = b net; env I = input; env T = undefined} in env x) (runNet (Var I) (Var W) (Var B))

errorList :: KnownNat n => Int -> Net n -> [(H.R n, H.R n)] -> [Double]
errorList 0 _ _ = []
errorList i n dataList = averageError dataList n:errorList (i-1) (trainList dataList n) dataList

testData :: [(H.R 5, H.R 5)]
testData = [(H.vector [1.7670488430964988,1.7685432321245518,-1.76481459092573,-1.0730787772094268,0.17096162874762033], H.vector [0.20127744370664472,0.27989187020090234,0.12539257828730796,0.18305288330757638,0.21038522449756855]),
    (H.vector [-1.4775739291112748,1.7501713483362327,0.3473621431493026,-0.7155082741358822,-1.0967498864497756], H.vector [0.146945137277467,0.2647495217024989,0.20740555968631516,0.14261590742191385,0.23828387391180506]),
    (H.vector [1.566897872633719,-0.599374222848273,-1.8863917104370853,0.6233772703555305,-0.41360301077999306], H.vector [0.19711208765639238,0.11702920197522196,0.21008226904227795,0.1889481388867347,0.286828302439373]),
    (H.vector [-0.4904092431489422,-1.3104074053980446,-1.4608813838385424,0.45278699624016294,0.8912973287009156], H.vector [0.24811487757016737,0.20777282035021408,0.21745830104140063,0.1622741975119194,0.1643798035262985]),
    (H.vector [-0.7677669091000068,-0.588909204392186,0.2031740929014858,1.2019032888123315,-0.26625811414153233], H.vector [0.20250189007110955,0.12258734139116788,0.27504244137882483,0.2664543609095971,0.13341396624930066]),
    (H.vector [-0.1637653308751832,-1.5393339728654054,-0.20436268402466684,1.8128311391048277,-0.9722507647109455], H.vector [0.198060933683571,0.16294717246471985,0.2790945262994971,0.17021192891208325,0.18968543864012888]),
    (H.vector [1.729265019171529,1.5049514581938048,-0.5850129642454036,0.9365729554260955,-1.7108992485845924], H.vector [0.22530150388850642,0.18733949771365258,0.18244428176309244,0.27178427622347806,0.13313044041127062]),
    (H.vector [-1.9977856650938213,1.602919513174761,1.032677342664766,-1.0601191888843287,1.6221231406657597], H.vector [0.30804222053628655,0.17804250303019467,0.15788007078996938,0.14688086932032166,0.20915433632322766]),
    (H.vector [0.8625464042939925,1.9628317504948152,1.4112822261691478,0.43207774610821037,1.428955258535666], H.vector [0.1350049630786681,0.2119381064209733,0.29136274162051923,0.198682035231479,0.16301215364836047]),
    (H.vector [-1.097321597438921,-0.3269954967903883,-0.8534412564958638,1.141650542217144,-0.8074734931846492], H.vector [0.23549136141749138,0.15345707782045817,0.2666942424062719,0.19120233906700213,0.15315497928877655])]

-- Main function for running the program
main :: IO()
main = do
    MWC.withSystemRandom $ \g -> do
        w :: H.Sq 5 <- MWC.uniformR (-0.5, 0.5) g
        b :: H.R 5 <- MWC.uniformR (-0.5, 0.5) g
        let n = N w (mkRSquare 5 b)
        let m = loopStepNet 200 n testData
        print "Network error: "
        print (evalNetErr (mkRSquare 5 (fst (head testData))) (mkRSquare 5 (snd (head testData))) m)

trainList :: (KnownNat n, KnownNat m) => [(H.R n, H.R m)] -> Net n -> Net n
trainList = flip $ foldl' (\n (x,y) -> stepNet (mkRSquare (getDim n) x) (mkRSquare (getDim n) y) n)

-- Function to transform an H.R m vector into an H.Sq n matrix by duplicating the vector and adding 0's below if necessary
mkRSquare :: (KnownNat m, KnownNat n) => Int -> H.R m -> H.Sq n
mkRSquare n r = H.build (\a b -> if round a < x then H.extract r Numeric.LinearAlgebra.Data.! round a else 0)
  where
    x = H.size r

mkLSquare :: (KnownNat l, KnownNat m, KnownNat n) => Int -> H.L l m -> H.Sq n
mkLSquare n l = H.build (\a b -> if round a < x && round b < y then (H.extract l Numeric.LinearAlgebra.Data.! round a) Numeric.LinearAlgebra.Data.! round b else 0)
  where
    (x,y) = H.size l

testNet :: (KnownNat n, KnownNat m) => [(H.R n, H.R m)] -> Net n -> Double
testNet xs n = sum (Prelude.map (uncurry test) xs) / fromIntegral (length xs)
  where
    test x (H.extract->t)
        | HM.maxIndex t == HM.maxIndex (H.extract r Numeric.LinearAlgebra.Data.! 0) = 1
        | otherwise                                  = 0
      where
        r = evalNet (mkRSquare (getDim n) x) n

instance KnownNat n => Num (Net n) where
    (+)         = gPlus
    (-)         = gMinus
    (*)         = gTimes
    negate      = gNegate
    abs         = gAbs
    signum      = gSignum
    fromInteger = gFromInteger

instance KnownNat n => Fractional (Net n) where
    (/)          = gDivide
    recip        = gRecip
    fromRational = gFromRational

instance KnownNat n => MWC.Variate (H.R n) where
    uniform g = H.randomVector <$> MWC.uniform g <*> pure H.Uniform
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance (KnownNat m, KnownNat n) => MWC.Variate (H.L m n) where
    uniform g = H.uniformSample <$> MWC.uniform g <*> pure 0 <*> pure 1
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance NFData (Net n)