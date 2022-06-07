{-# LANGUAGE MultiParamTypeClasses #-}
module Tests.ForwardADTest where

------ IMPORTS ------
import AD.ForwardMode hiding (main)
import Data.Map (fromList)
import Numeric.LinearAlgebra.HMatrix (Transposable(..))
import Test.HUnit

instance Transposable Double Double where
    tr   = id 
    tr'  = id

roundTo :: Double -> Double -> Double
roundTo i d = (fromIntegral (round ((10**i)*d))) /(10**i)

roundSDualTo :: Double -> SDual v Double -> SDual v Double
roundSDualTo i (SP f df) = SP (roundTo i f) (fmap (roundTo i) df)

testForwardADExp  = TestCase (assertEqual "for (forwardSparseGradient (const 2.45) (Exp (Times (Var X) (Var X))))"
                                          (roundSDualTo 6 $ SP (404.4386272426999::Double) (fromList [(X, 1981.7492734892296)]))
                                          (roundSDualTo 6 $ forwardSparseGradient (const 2.45) (exp (times (Var X) (Var X))))
                             )

testForwardADLog  = TestCase (assertEqual "for (forwardSparseGradient (const 2.45) (Log (Times (Var X) (Var X))))"
                                          (roundSDualTo 6 $ SP (1.7921760491132714::Double) (fromList [(X, 0.8163265306122448)]))
                                          (roundSDualTo 6 $ forwardSparseGradient (const 2.45) (log (times (Var X) (Var X))))
                             )

testForwardADDiv  = TestCase (assertEqual "for (forwardSparseGradient (const 2.45) (Div (Times (Var X) (Var X)) (Plus (Var X) One)))"
                                          (roundSDualTo 6 $ SP (1.7398550724637685::Double) (fromList [(X, 0.9159840369670238)]))
                                          (roundSDualTo 6 $ forwardSparseGradient (const 2.45) (Div (Times (Var X) (Var X)) (Plus (Var X) One)))
                             )

testForwardADNegate  = TestCase (assertEqual "for (forwardSparseGradient (const 2.45) (Negate (Times (Var X) (Var X))))"
                                          (roundSDualTo 6 $ SP (-6.0025::Double) (fromList [(X, -4.9)]))
                                          (roundSDualTo 6 $ forwardSparseGradient (const 2.45) (Negate (Times (Var X) (Var X))))
                             )

tests = TestList [TestLabel "testForwardADExp" testForwardADExp, TestLabel "testForwardADLog" testForwardADLog, TestLabel "testForwardADDiv" testForwardADDiv, TestLabel "testForwardADNegate" testForwardADNegate]

main :: IO Counts
main = runTestTT tests