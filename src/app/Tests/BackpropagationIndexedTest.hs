{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Tests.BackpropagationIndexedTest where

------ IMPORTS ------
import AD.ForwardMode (Norm(norm))
import IndexedMatrix (IndexedMatrix (IM))
import IndexedSemiring (IndexedExpr (..), IndexedSemiring (..))
import Numeric.LinearAlgebra (Transposable(tr))
import Numeric.LinearAlgebra.Data (fromLists, (><), cmap)
import Numeric.LinearAlgebra.Static (R, L, matrix, vector, (#>), col, (<>))
import Prelude hiding ((<>))
import Test.HUnit
import Backpropagation.BackpropagationIndexed

roundTo :: Double -> Double -> Double
roundTo i d = (fromIntegral (round ((10**i)*d))) /(10**i)

roundNetTo :: Double -> Net IndexedMatrix -> Net IndexedMatrix
roundNetTo i (N w1 b1) = N (roundIMTo i w1) (roundIMTo i b1)

roundIMTo :: Double -> IndexedMatrix -> IndexedMatrix
roundIMTo i (IM r c m) = IM r c (cmap (roundTo i) m)

w :: L 2 3
w = matrix [0.94, -1, -0.4, 0.6, 0.36, -2]

wIM :: IndexedMatrix
wIM = mkLIndexedMatrix w

b :: R 2
b = vector [0.13, 1.45]

bIM :: IndexedMatrix
bIM = mkRIndexedMatrix b

input :: R 3
input = vector [2,1.3,4]

inputIM :: IndexedMatrix
inputIM = mkRIndexedMatrix input

target :: R 2
target = vector [0.12,0.88]

targetIM :: IndexedMatrix
targetIM = mkRIndexedMatrix target

output :: R 2
output = vector [0.3, 0.7]

outputIM :: IndexedMatrix
outputIM = mkRIndexedMatrix output


net :: Net IndexedMatrix
net = N wIM bIM

testRunNet = TestCase (assertEqual "for (runNet input wIM bIM)"
                                   (roundIMTo 6 $ IM 2 1 ((2><1) [0.9818719421636715, 0.018128057836328464]))
                                   (roundIMTo 6 $ runNet inputIM net)
                               )

testNorm = TestCase (assertEqual "for (norm bIM)"
                                        (roundIMTo 6 $ IM 2 1 ((2><1) [1.58,1.58]))
                                        (roundIMTo 6 $ norm bIM)
                               )

testCrossEntropy = TestCase (assertEqual "for (crossEntropy outputIM targetIM)"
                                            (roundIMTo 6 $ IM 1 1 0.4583506871851969)
                                            (roundIMTo 6 $ crossEntropy outputIM targetIM)
                               )

testBackpropagationvectorMultiplication  = TestCase (assertEqual "for (backpropagation (tr (Var B1) `times` Var B1) net inputIM targetIM)"
                                          (roundNetTo 6 $ N (IM 2 3 ((2><3) [0,0,0,0,0,0])) (IM 2 1 2*bIM))
                                          (roundNetTo 6 $ backpropagation (tr (Var B1) `times` Var B1) net inputIM targetIM)
                             )
testBackpropagationvectorMultiplication2  = TestCase (assertEqual "for (backpropagation (tr (Var W1 `times` Var I) `times` Var B1) net inputIM targetIM)"
                                          (roundNetTo 6 $ N (IM 2 3 ((2><3) [ 0.26,0.169,0.52,  2.9,1.885,5.8 ])) (IM 2 1 ((2><1) [-1.02,-6.332])))
                                          (roundNetTo 6 $ backpropagation (tr (Var W1 `times` Var I) `times` Var B1) net inputIM targetIM)
                             )
testBackpropagationExp  = TestCase (assertEqual "for (backpropagation (tr (exp (Var B1)) `times` exp (Var B1)) net inputIM targetIM)"
                                          (roundNetTo 6 $ N (IM 2 3 ((2><3) [0,0,0,0,0,0])) (IM 2 1 2*exp bIM*exp bIM))
                                          (roundNetTo 6 $ backpropagation (tr (exp (Var B1)) `times` exp (Var B1)) net inputIM targetIM)
                             )
testBackpropagationLog  = TestCase (assertEqual "for (backpropagation (tr (log (Var B1)) `times` log (Var B1)) net inputIM targetIM)"
                                          (roundNetTo 6 $ N (IM 2 3 ((2><3) [0,0,0,0,0,0])) (IM 2 1 ((2><1) [-31.388012746562374,0.5125014571482523])))
                                          (roundNetTo 6 $ backpropagation (tr (log (Var B1)) `times` log (Var B1)) net inputIM targetIM)
                             )
testBackpropagationNorm  = TestCase (assertEqual "for (backpropagation (tr (Var B1) `times` norm (Var B1)) net inputIM targetIM)"
                                          (roundNetTo 6 $ N (IM 2 3 ((2><3) [0,0,0,0,0,0])) (IM 2 1 2*norm bIM))
                                          (roundNetTo 6 $ backpropagation (tr (Var B1) `times` norm (Var B1)) net inputIM targetIM)
                             )
testBackpropagationNorm2  = TestCase (assertEqual "for (backpropagation (tr (Var B1) `times` norm ((Var W1 `times` Var I) `plus` Var B1)) net inputIM targetIM)"
                                          (roundNetTo 6 $ N (IM 2 3 ((2><3) [3.16,2.054,6.32,3.16,2.054,6.32])) (IM 2 1 ((2><1) [-4.192,-4.192])))
                                          (roundNetTo 6 $ backpropagation (tr (Var B1) `times` norm ((Var W1 `times` Var I) `plus` Var B1)) net inputIM targetIM)
                             )
testBackpropagationDiv  = TestCase (assertEqual "for (backpropagation ((tr (exp (Var B1)/(norm (exp (Var B1)))) `times` exp (Var B1))) net inputIM targetIM)"
                                          (roundNetTo 6 $ N (IM 2 3 ((2><3) [0,0,0,0,0,0])) (IM 2 1 ((2><1) [-0.27971393902056474,3.884171783537797])))
                                          (roundNetTo 6 $ backpropagation (tr (exp (Var B1)/norm (exp (Var B1))) `times` exp (Var B1)) net inputIM targetIM)
                             )
testBackpropagationSoftmax  = TestCase (assertEqual "for (backpropagation (tr (softmax (Var W1 `times` Var I)) `times` softmax (Var B1)) net inputIM targetIM)"
                                          (roundNetTo 6 $ N (IM 2 3 ((2><3) [-0.00564917622657507,-0.0036719645472738094,-0.01129835245315014,0.005649176226575027,0.0036719645472737673,0.011298352453150053])) (IM 2 1 ((2><1) [0.1647408649284523,-0.1647408649284523])))
                                          (roundNetTo 6 $ backpropagation (tr (softmax (Var W1 `times` Var I)) `times` softmax (Var B1)) net inputIM targetIM)
                             )
testBackpropagationSoftmax2  = TestCase (assertEqual "for (backpropagation (tr (softmax (Var B1)) `times` softmax (Var B1)) net inputIM targetIM)"
                                          (roundNetTo 6 $ N (IM 2 3 ((2><3) [0,0,0,0,0,0])) (IM 2 1 ((2><1) [-0.1924492002690556,0.19244920026905565])))
                                          (roundNetTo 6 $ backpropagation (tr (softmax (Var B1)) `times` softmax (Var B1)) net inputIM targetIM)
                             )
testBackpropagationCrossEntropy  = TestCase (assertEqual "for (backpropagation (crossEntropy (exp ((Var W1 `times` Var I) `plus` Var B1)) (Var T)) net inputIM targetIM)"
                                          (roundNetTo 6 $ N (IM 2 3 ((2><3) [-0.24,-0.156,-0.48,-1.76,-1.144,-3.52])) (IM 2 1 ((2><1) [-0.12,-0.88])))
                                          (roundNetTo 6 $ backpropagation (crossEntropy (exp ((Var W1 `times` Var I) `plus` Var B1)) (Var T)) net inputIM targetIM)
                             )

testBackpropagationCrossEntropy2  = TestCase (assertEqual "for (backpropagation (crossEntropy (exp (Var B1)) (Var W1 `times` Var I)) net inputIM targetIM)"
                                          (roundNetTo 6 $ N (IM 2 3 ((2><3) [1.471386615204802,0.9564012998831214,2.942773230409604,-0.0013216317014309435,-0.0008590606059301133,-0.002643263402861887])) (IM 2 1 ((2><1) [-2.773807232100601,-0.001226533285004117])))
                                          (roundNetTo 6 $ backpropagation (crossEntropy (Var B1) (exp (Var W1 `times` Var I))) net inputIM targetIM)
                             )
testBackpropagationCrossEntropy3 = TestCase (assertEqual "for (backpropagation (crossEntropy (exp (Var B1)/exp (Var W1 `times` Var I)) (Var W1 `times` Var I)) net inputIM targetIM)"
                                          (roundNetTo 6 $ N (IM 2 3 ((2><3) [-3.82,-2.483,-7.64,-22.428,-14.5782,-44.856])) (IM 2 1 ((2><1) [-1.02,-6.332])))
                                          (roundNetTo 6 $ backpropagation (crossEntropy (exp (Var B1)/exp ((Var W1 `times` Var I) `plus` Var B1)) ((Var W1 `times` Var I) `plus` Var B1)) net inputIM targetIM)
                             )
testBackpropagation  = TestCase (assertEqual "for (backpropagation (netErr (Var I) (Var W1) (Var B1) (Var T)) net inputIM targetIM)"
                                          (roundNetTo 6 $ N (IM 2 3 ((2><3) [1.7237438843273432,  1.120433524812773,  3.4474877686546863, -1.7237438843273434, -1.120433524812773, -3.4474877686546868])) (IM 2 1 ((2><1) [0.8618719421636716, -0.8618719421636717])))
                                          (roundNetTo 6 $ backpropagation (netErr (Var I) (N (Var W1) (Var B1)) (Var T)) net inputIM targetIM)
                             )

tests = TestList [TestLabel "testRunNet" testRunNet,
                  TestLabel "testNorm" testNorm,
                  TestLabel "testCrossEntropy" testCrossEntropy,
                  TestLabel "testBackpropagationvectorMultiplication" testBackpropagationvectorMultiplication,
                  TestLabel "testBackpropagationvectorMultiplication2" testBackpropagationvectorMultiplication2,
                  TestLabel "testBackpropagationExp" testBackpropagationExp,
                  TestLabel "testBackpropagationLog" testBackpropagationLog,
                  TestLabel "testBackpropagationNorm" testBackpropagationNorm,
                  TestLabel "testBackpropagationNorm2" testBackpropagationNorm2,
                  TestLabel "testBackpropagationDiv" testBackpropagationDiv,
                  TestLabel "testBackpropagationSoftmax" testBackpropagationSoftmax,
                  TestLabel "testBackpropagationSoftmax2" testBackpropagationSoftmax2,
                  TestLabel "testBackpropagationCrossEntropy" testBackpropagationCrossEntropy,
                  TestLabel "testBackpropagationCrossEntropy2" testBackpropagationCrossEntropy2,
                  TestLabel "testBackpropagationCrossEntropy3" testBackpropagationCrossEntropy3,
                  TestLabel "testBackpropagation" testBackpropagation]



main :: IO Counts
main = runTestTT tests