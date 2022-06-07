{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Tests.BackpropagationSquareMatrixTest where

------ IMPORTS ------
import AD.ForwardMode (Norm(norm), Expr (..), Semiring (times))
import Backpropagation.BackpropagationSquareMatrix hiding (w, b, main)
import GHC.TypeNats (KnownNat)
import Numeric.LinearAlgebra (sumElements, Transposable (tr), Indexable ((!)))
import Numeric.LinearAlgebra.Static (Sq, R, vector, matrix, Sized (extract, konst), (#>), (<>), eye, (<.>), col, Domain (dmmap))
import Prelude hiding ((<>))
import Test.HUnit



roundTo :: Double -> Double -> Double
roundTo i d = (fromIntegral (round ((10**i)*d))) /(10**i)

roundNetTo :: KnownNat m => Double -> Net m -> Net m
roundNetTo i (N w b) = N (dmmap (roundTo i) w) (dmmap (roundTo i) b)

w :: Sq 3
w = matrix [0.94, -1, -0.4, 0.6, 0.36, -2, 1.2, 1.02, 0.8]

bR :: R 3
bR = vector [0.13, 1.45, -2.4]

b :: Sq 3
b = matrix [0.13, 0.13, 0.13, 1.45, 1.45, 1.45, -2.4, -2.4, -2.4]

inputR :: R 3
inputR = vector [2,1.3,4]

input :: Sq 3
input = matrix [2, 2, 2, 1.3, 1.3, 1.3, 4, 4, 4]

targetR :: R 3
targetR = vector [0.12,0.6,0.28]

target :: Sq 3
target = matrix [0.12, 0.12, 0.12, 0.6, 0.6, 0.6, 0.28, 0.28, 0.28]

output :: Sq 3
output = matrix [0.3, 0.3, 0.3, 0.55, 0.55, 0.55, 0.15, 0.15, 0.15]

net :: Net 3
net = N w b

testRunNet = TestCase (assertEqual "for (runNet input w b)"
                                   (dmmap (roundTo 6) $ matrix [0.004424859556066427,0.004424859556066427,0.004424859556066427,  0.0000816950831421474,0.0000816950831421474,0.0000816950831421474,  0.9954934453607913,0.9954934453607913,0.9954934453607913])
                                   (dmmap (roundTo 6) $ runNet input w b)
                               )

testNormSquare = TestCase (assertEqual "for (norm b)"
                                            (dmmap (roundTo 6) $ konst (-0.82)) -- rounding error, normally -0.82
                                            (dmmap (roundTo 6) $ norm b)
                               )
testMkRSquare = TestCase (assertEqual "for (mkRSquare 3 bR)"
                                            (dmmap (roundTo 6) b)
                                            (dmmap (roundTo 6) $ mkRSquare 3 bR)
                               )

testSquareError = TestCase (assertEqual "for (squareError output target)"
                                            (dmmap (roundTo 6) $ matrix [0.0518, 0.0518, 0.0518, 0.0518, 0.0518, 0.0518, 0.0518, 0.0518, 0.0518])
                                            (dmmap (roundTo 6) $ squareError output target)
                               )

testBackpropagationvectorMultiplication  = TestCase (assertEqual "for (backpropagation (tr (Var B) `times` Var B) net input target)"
                                          (roundNetTo 6 $ N (konst 0) (2*b))
                                          (roundNetTo 6 $ backpropagation (tr (Var B) `times` Var B) net input target)
                             )
testBackpropagationvectorMultiplication2  = TestCase (assertEqual "for (backpropagation (tr ((Var W) `times` (Var I)) `times` Var B) net input target)"
                                          (roundNetTo 6 $ N (matrix [ 0.26,0.16899999999999998,0.52,  2.8999999999999995,1.885,5.799999999999999, -4.799999999999999,-3.1199999999999997,-9.599999999999998 ]) (matrix [-1.0200000000000002,-1.0200000000000002,-1.0200000000000002,  -6.332,-6.332,-6.332,  6.926,6.926,6.926]))
                                          (roundNetTo 6 $ backpropagation (tr (Var W `times` Var I) `times` Var B) net input target)
                             )
testBackpropagationExp  = TestCase (assertEqual "for (backpropagation (tr (exp (Var B)) `times` exp (Var B)) net input target)"
                                          (roundNetTo 6 $ N (konst 0) (2*exp b*exp b))
                                          (roundNetTo 6 $ backpropagation (tr (exp (Var B)) `times` exp (Var B)) net input target)
                             )
testBackpropagationNorm  = TestCase (assertEqual "for (backpropagation (norm (exp (Var B))) net input target)"
                                          (roundNetTo 6 $ N (konst 0) (exp b))
                                          (roundNetTo 6 $ backpropagation (norm (exp (Var B))) net input target)
                             )
testBackpropagationDiv  = TestCase (assertEqual "for (backpropagation ((tr (exp (Var B)) `times` exp (Var B)) / norm (exp (Var B))) net input target)"
                                          (roundNetTo 6 $ N (konst 0) (matrix [-0.2630616325903559,-0.2630616325903559,-0.2630616325903559,  3.865062663629692,3.865062663629692,3.865062663629692,  -0.055576860896900226,-0.055576860896900226,-0.055576860896900226]))
                                          (roundNetTo 6 $ backpropagation ((tr (exp (Var B)) `times` exp (Var B)) / norm (exp (Var B))) net input target)
                             )
testBackpropagationSoftmax  = TestCase (assertEqual "for (backpropagation (tr (softmax (Var B)) `times` softmax (Var B)) net input target)"
                                          (roundNetTo 6 $ N (konst 0) (matrix [-0.18176333598549885,-0.18176333598549885,-0.18176333598549885,  0.20254567788586553,0.20254567788586553,0.20254567788586553,  -0.020782341900366624,-0.020782341900366624,-0.020782341900366624]))
                                          (roundNetTo 6 $ backpropagation (tr (softmax (Var B)) `times` softmax (Var B)) net input target)
                             )
testBackpropagation  = TestCase (assertEqual "for (backpropagation (netErr (Var I) (Var W) (Var B) (Var T)) net input target)"
                                          (roundNetTo 6 $ N (matrix [-0.014642457828107816,-0.009517597588270081,-0.029284915656215632,  -0.00042861388302919967,-0.0002785990239689797,-0.0008572277660583993,  0.01507107171113701,0.00979619661223922,0.03014214342227402]) (matrix [-0.007321228914053908,-0.007321228914053908,-0.007321228914053908,  -2.1430694151459978e-4,-2.1430694151459978e-4,-2.1430694151459978e-4,  7.535535855568654e-3,7.535535855568654e-3,7.535535855568654e-3]))
                                          (roundNetTo 6 $ backpropagation (netErr (Var I) (Var W) (Var B) (Var T)) net input target)
                             )

tests = TestList [TestLabel "testRunNet" testRunNet, TestLabel "testNormSquare" testNormSquare, TestLabel "testMkRSquare" testMkRSquare, TestLabel "testSquareError" testSquareError, TestLabel "testBackpropagationvectorMultiplication" testBackpropagationvectorMultiplication, TestLabel "testBackpropagationvectorMultiplication2" testBackpropagationvectorMultiplication2, TestLabel "testBackpropagationExp" testBackpropagationExp, TestLabel "testBackpropagationNorm" testBackpropagationNorm, TestLabel "testBackpropagationDiv" testBackpropagationDiv, TestLabel "testBackpropagationSoftmax" testBackpropagationSoftmax, TestLabel "testBackpropagation" testBackpropagation]

main :: IO Counts
main = runTestTT tests