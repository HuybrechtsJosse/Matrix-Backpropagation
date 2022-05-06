{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Tests.BackpropagationIndexed1Test where

------ IMPORTS ------
import AD.ForwardMode (Norm(norm))
import Backpropagation.BackpropagationIndexed1 hiding (main, w, b)
import IndexedMatrix1 (IndexedMatrix (IM))
import IndexedSemiring (IndexedExpr (..), IndexedSemiring (..))
import Numeric.LinearAlgebra (Transposable(tr))
import Numeric.LinearAlgebra.Data (fromLists, (><))
import Numeric.LinearAlgebra.Static (R, L, matrix, vector, (#>), col, (<>))
import Prelude hiding ((<>))
import Test.HUnit

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


net :: Net
net = N wIM bIM

testMkRIndexed = TestCase (assertEqual "for (mkRIndexedMatrix input)"
                                   (IM 3 1 [[2],[1.3],[4]])
                                   (mkRIndexedMatrix input)
                               )
testMkLIndexed = TestCase (assertEqual "for (mkRIndexedMatrix w)"
                                   (IM 2 3 [[0.94, -1, -0.4],[0.6, 0.36, -2]])
                                   (mkLIndexedMatrix w)
                               )
testRunNet = TestCase (assertEqual "for (runNet input wIM bIM)"
                                   (IM 2 1 [[0.9818719421636715], [0.01812805783632847]])
                                   (runNet inputIM wIM bIM)
                               )

testNorm = TestCase (assertEqual "for (norm bIM)"
                                        (IM 2 1 [[1.58],[1.58]])
                                        (norm bIM)
                               )

testCrossEntropy = TestCase (assertEqual "for (crossEntropy outputIM targetIM)"
                                            (IM 1 1 [[0.4583506871851969]])
                                            (crossEntropy outputIM targetIM)
                               )

testBackpropagationvectorMultiplication  = TestCase (assertEqual "for (backpropagation (tr (Var B) `times` Var B) net inputIM targetIM)"
                                          (N (IM 2 3 [[0,0,0],[0,0,0]]) (IM 2 1 [[2],[2]]*bIM))
                                          (backpropagation (tr (Var B) `times` Var B) net inputIM targetIM)
                             )
testBackpropagationvectorMultiplication2  = TestCase (assertEqual "for (backpropagation (tr (Var W `times` Var I) `times` Var B) net inputIM targetIM)"
                                          (N (IM 2 3 [[0.26,0.169,0.52],[2.9,1.885,5.8]]) (IM 2 1 [[-1.0200000000000005],[-6.332]]))
                                          (backpropagation (tr (Var W `times` Var I) `times` Var B) net inputIM targetIM)
                             )
testBackpropagationExp  = TestCase (assertEqual "for (backpropagation (tr (exp (Var B)) `times` exp (Var B)) net inputIM targetIM)"
                                          (N (IM 2 3 [[0,0,0],[0,0,0]]) (IM 2 1 [[2],[2]]*exp bIM*exp bIM))
                                          (backpropagation (tr (exp (Var B)) `times` exp (Var B)) net inputIM targetIM)
                             )
testBackpropagationLog  = TestCase (assertEqual "for (backpropagation (tr (log (Var B)) `times` log (Var B)) net inputIM targetIM)"
                                          (N (IM 2 3 [[0,0,0],[0,0,0]]) (IM 2 1 [[-31.388012746562374],[0.5125014571482523]]))
                                          (backpropagation (tr (log (Var B)) `times` log (Var B)) net inputIM targetIM)
                             )
testBackpropagationNorm  = TestCase (assertEqual "for (backpropagation (tr (Var B) `times` norm (Var B)) net inputIM targetIM)"
                                          (N (IM 2 3 [[0,0,0],[0,0,0]]) (IM 2 1 [[2],[2]]*norm bIM))
                                          (backpropagation (tr (Var B) `times` norm (Var B)) net inputIM targetIM)
                             )
testBackpropagationNorm2  = TestCase (assertEqual "for (backpropagation (tr (Var B) `times` norm ((Var W `times` Var I) `plus` Var B)) net inputIM targetIM)"
                                          (N (IM 2 3 [[3.16,2.0540000000000003,6.32],[3.16,2.0540000000000003,6.32]]) (IM 2 1 [[-4.192],[-4.192]]))
                                          (backpropagation (tr (Var B) `times` norm ((Var W `times` Var I) `plus` Var B)) net inputIM targetIM)
                             )
testBackpropagationDiv  = TestCase (assertEqual "for (backpropagation ((tr (exp (Var B)/(norm (exp (Var B)))) `times` exp (Var B))) net inputIM targetIM)"
                                          (N (IM 2 3 [[0,0,0],[0,0,0]]) (IM 2 1 [[-0.27971393902056474],[3.884171783537797]]))
                                          (backpropagation (tr (exp (Var B)/norm (exp (Var B))) `times` exp (Var B)) net inputIM targetIM)
                             )
testBackpropagationSoftmax  = TestCase (assertEqual "for (backpropagation (tr (softmax (Var W `times` Var I)) `times` softmax (Var B)) net inputIM targetIM)"
                                          (N (IM 2 3 [[-0.00564917622657507,-0.0036719645472738094,-0.01129835245315014],[0.005649176226575028,0.003671964547273768,0.011298352453150057]]) (IM 2 1 [[0.1647408649284523],[-0.1647408649284523]]))
                                          (backpropagation (tr (softmax (Var W `times` Var I)) `times` softmax (Var B)) net inputIM targetIM)
                             )
testBackpropagationSoftmax2  = TestCase (assertEqual "for (backpropagation (tr (softmax (Var B)) `times` softmax (Var B)) net inputIM targetIM)"
                                          (N (IM 2 3 [[0,0,0],[0,0,0]]) (IM 2 1 [[-0.1924492002690556],[0.19244920026905565]]))
                                          (backpropagation (tr (softmax (Var B)) `times` softmax (Var B)) net inputIM targetIM)
                             )
testBackpropagationCrossEntropy  = TestCase (assertEqual "for (backpropagation (crossEntropy (exp ((Var W `times` Var I) `plus` Var B)) (Var T)) net inputIM targetIM)"
                                          (N (IM 2 3 [[-0.24000000000000002,-0.15600000000000003,-0.48000000000000004],[-1.7600000000000005,-1.1440000000000003,-3.520000000000001]]) (IM 2 1 [[-0.12000000000000001],[-0.8800000000000002]]))
                                          (backpropagation (crossEntropy (exp ((Var W `times` Var I) `plus` Var B)) (Var T)) net inputIM targetIM)
                             )

testBackpropagationCrossEntropy2  = TestCase (assertEqual "for (backpropagation (crossEntropy (exp (Var B)) (Var W `times` Var I)) net inputIM targetIM)"
                                          (N (IM 2 3 [[1.4713866152048019,0.9564012998831213,2.9427732304096037],[-0.0013216317014309435,-0.0008590606059301133,-0.002643263402861887]]) (IM 2 1 [[-2.7738072321006007],[-0.001226533285004117]]))
                                          (backpropagation (crossEntropy (Var B) (exp (Var W `times` Var I))) net inputIM targetIM)
                             )
testBackpropagationCrossEntropy3 = TestCase (assertEqual "for (backpropagation (crossEntropy (exp (Var B)/exp (Var W `times` Var I)) (Var W `times` Var I)) net inputIM targetIM)"
                                          (N (IM 2 3 [[-3.820000000000002,-2.4830000000000014,-7.640000000000004],[-22.428,-14.578200000000002,-44.856]]) (IM 2 1 [[-1.0200000000000005],[-6.332]]))
                                          (backpropagation (crossEntropy (exp (Var B)/exp ((Var W `times` Var I) `plus` Var B)) ((Var W `times` Var I) `plus` Var B)) net inputIM targetIM)
                             )
testBackpropagation  = TestCase (assertEqual "for (backpropagation (netErr (Var I) (Var W) (Var B) (Var T)) net inputIM targetIM)"
                                          (N (IM 2 3 [[1.723743884327343,  1.120433524812773,  3.447487768654686],[-1.7237438843273434, -1.120433524812773, -3.4474877686546868]]) (IM 2 1 [[0.8618719421636715],[-0.8618719421636717]]))
                                          (backpropagation (netErr (Var I) (Var W) (Var B) (Var T)) net inputIM targetIM)
                             )

tests = TestList [TestLabel "testMkRIndexed" testMkRIndexed,
                  TestLabel "testMkLIndexed" testMkLIndexed,
                  TestLabel "testRunNet" testRunNet,
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