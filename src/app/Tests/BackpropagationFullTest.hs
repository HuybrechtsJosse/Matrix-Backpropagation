{-# LANGUAGE DataKinds #-}
module Tests.BackpropagationFullTest where

------ IMPORTS ------
import AD.ForwardMode (Expr (..), XY (X), forwardSparseGradient, SDual (SP))
import Backpropagation.BackpropagationFull hiding (main)
import Data.Map (fromList)
import Numeric.LinearAlgebra.Static (Sq, L, matrix)
import Test.HUnit

roundTo :: Double -> Double -> Double
roundTo i d = (fromIntegral (round ((10**i)*d))) /(10**i)

roundSDualTo :: Double -> SDual v Double -> SDual v Double
roundSDualTo i (SP f df) = SP (roundTo i f) (fmap (roundTo i) df)

w :: Sq 3
w = matrix [0.94, -1, -0.4, 0.6, 0.36, -2, 1.2, 1.02, 0.8]

input :: L 3 1
input = matrix [2, 1.5, -3.1]

b :: L 3 1
b = matrix [0.3, -1.42, 2.1]


testCreateMatrixVar = TestCase (assertEqual "for (createMatrixVar (W1, w))"
                                            [[Var (W1, 0, 0), Var (W1, 0, 1), Var (W1, 0, 2)],[Var (W1, 1, 0), Var (W1, 1, 1), Var (W1, 1, 2)],[Var (W1, 2, 0), Var (W1, 2, 1), Var (W1, 2, 2)]]
                                            (createMatrixVar (W1, w))
                               )

testLogistic        = TestCase (assertEqual "for (logistic [[0.94, -1, -0.4], [0.6, 0.36, -2], [1.2, 1.02, 0.8]])"
                                            (fmap (fmap (roundTo 6)) [[0.71909966, 0.2689414, 0.4013123],[0.6456563, 0.58904, 0.1192029],[0.76852478, 0.7349726, 0.68997448]])
                                            (fmap (fmap (roundTo 6)) (logistic [[0.94, -1, -0.4], [0.6, 0.36, -2], [1.2, 1.02, 0.8]]))
                               )

testSoftMax         = TestCase (assertEqual "for (softMax [[0.94, -0.4], [0.36, -2], [1.2, 0.8]])"
                                            (fmap (fmap (roundTo 5)) [[0.24746974, 0.06479888],[0.1385579, 0.013082668],[0.32095, 0.21513986]])
                                            (fmap (fmap (roundTo 5)) (softMax [[0.94, -0.4], [0.36, -2], [1.2, 0.8]]))
                               )

testCrossEntropy    = TestCase (assertEqual "for (crossEntropy [[Var X], [Plus (Var X) (One)], [Times (Plus One One) (Var X)]] [[Plus (Var X) (Var X)], [Times (Var X) (Plus One One)], [Zero]])"
                                            (Negate ((Log (Var X) `Times` Plus (Var X) (Var X)) `Plus` ((Log (Plus (Var X) One) `Times` Times (Var X) (Plus One One)) `Plus` (Log (Times (Plus One One) (Var X)) `Times` Zero))))
                                            (crossEntropy [[Var X], [Plus (Var X) One], [Times (Plus One One) (Var X)]] [[Plus (Var X) (Var X)], [Times (Var X) (Plus One One)], [Zero]])
                               )

testCrossEntropyForwardAD = TestCase (assertEqual "for (forwardSparseGradient (const 2.45) (crossEntropy [[Var X], [Plus (Var X) (One)], [Times (Plus One One) (Var X)]] [[Plus (Var X) (Var X)], [Times (Var X) (Plus One One)], [Zero]]))"
                                                  (fmap (fmap (roundSDualTo 6)) SP (-10.458865::Double) (fromList [(X,-7.689214)]))
                                                  (roundSDualTo 6 $ forwardSparseGradient (const 2.45) (crossEntropy [[Var X], [Plus (Var X) One], [Times (Plus One One) (Var X)]] [[Plus (Var X) (Var X)], [Times (Var X) (Plus One One)], [Zero]]))
                                     )
testSoftMaxForwardAD = TestCase (assertEqual "fmap (fmap (forwardSparseGradient (const 2.45))) (softMax [[Var X, Plus (Var X) One, Times (Plus One One) (Var X)]]))"
                                             (fmap (fmap (roundSDualTo 6)) [[SP (0.06533117315056354::Double) (fromList [(X,-0.04946094325631081)]), SP (0.17758854080708833::Double) (fromList [(X,-0.13444878327207369)]), SP (0.7570802860423482::Double) (fromList [(X,0.18390972652838444)])]])
                                             (fmap (fmap (roundSDualTo 6 . forwardSparseGradient (const 2.45))) (softMax [[Var X, Plus (Var X) One, Times (Plus One One) (Var X)]]))
                                )

testLogisticForwardAD = TestCase (assertEqual "fmap (fmap (forwardSparseGradient (const 2.45))) (logistic [[Var X, Plus (Var X) One, Times (Plus One One) (Var X)]]))"
                                             (fmap (fmap (roundSDualTo 6)) [[SP (0.9205614508160216::Double) (fromList [(X,0.07312806608752301)]), SP (0.969231140642852::Double) (fromList [(X,0.029822136651008052)]), SP (0.9926084586557181::Double) (fromList [(X,0.014673812921675484)])]])
                                             (fmap (fmap (roundSDualTo 6 . forwardSparseGradient (const 2.45))) (logistic [[Var X, Plus (Var X) One, Times (Plus One One) (Var X)]]))
                                )

tests = TestList [TestLabel "testLogisticForwardAD" testLogisticForwardAD, TestLabel "testSoftMaxForwardAD" testSoftMaxForwardAD, TestLabel "testCreateMatrixVar" testCreateMatrixVar, TestLabel "testLogistic" testLogistic, TestLabel "testSoftMax" testSoftMax, TestLabel "testCrossEntropyForwardAD" testCrossEntropyForwardAD, TestLabel "testCrossEntropy" testCrossEntropy]

main :: IO Counts
main = runTestTT tests