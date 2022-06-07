module Tests.Backpropagation3x3Test where

------ IMPORTS ------
import AD.ForwardMode (Expr (..))
import Backpropagation.Backpropagation3x3 hiding (main)
import Data.Map ( fromList )
import Test.HUnit

roundTo :: Double -> Double -> Double
roundTo i d = (fromIntegral (round ((10**i)*d))) /(10**i)

roundNetTo :: Double -> Net -> Net
roundNetTo i (N w11 w12 w13 w21 w22 w23 w31 w32 w33 b1 b2 b3) = N (roundTo i w11) (roundTo i w12) (roundTo i w13) (roundTo i w21) (roundTo i w22) (roundTo i w23) (roundTo i w31) (roundTo i w32) (roundTo i w33) (roundTo i b1) (roundTo i b2) (roundTo i b3)

testLogistic      = TestCase (assertEqual "for (logistic [2, 9.5, -3.6])"
                                          [0.880797, 0.999925, 0.026597]
                                          (fmap (\x-> roundTo 6 x) (logistic [2, 9.5, -3.6]))
                             )

testSquareError   = TestCase (assertEqual "for (squareError [2, 9.5, -3.6] [1.2, -8, 0.1])"
                                          160.29
                                          (roundTo 6 (squareError [2, 9.5, -3.6] [1.2, -8, 0.1]))
                             )

testVecProd       = TestCase (assertEqual "for (vecProd [1,2,3,4] [0.1,0.2,0.3,0.6]),"
                                          3.8
                                          (roundTo 2 (vecProd [1,2,3,4] [0.1,0.2,0.3,0.6]))
                             )

testMatrixVecProd = TestCase (assertEqual "for (matrixVecProd [[1,2,3,4],[5,6,7,8],[3.5,1.5,8.2,9]] [0.1,0.2,0.3,0.6]),"
                                          [3.8, 8.6, 8.51]
                                          (fmap (\x-> roundTo 6 x) (matrixVecProd [[1,2,3,4],[5,6,7,8],[3.5,1.5,8.2,9]] [0.1,0.2,0.3,0.6]))
                             )

testRunNet        = TestCase (assertEqual "for (runNet [9, 0.3, 2] [[0.5, 3.1, 9.04], [0.99, -6.12, 2.37], [4.82, -0.421, 0.92]] [1.15, 8.43, 0.067]),"
                                          [1, 1, 1]
                                          (fmap (\x-> roundTo 6 x) (runNet [9, 0.3, 2] [[0.5, 3.1, 9.04], [0.99, -6.12, 2.37], [4.82, -0.421, 0.92]] [1.15, 8.43, 0.067]))
                             )

testNetUpdate     = TestCase (assertEqual "for (createNet (fromList [(W11, 1), (W12, 2), (W13, 3), (W21, 4), (W22, 5), (W23, 6), (W31, 7), (W32, 8), (W33, 9), (B1, 10), (B2, 11), (B3, 12)]))"
                                          (N 1 2 3 4 5 6 7 8 9 10 11 12)
                                          (roundNetTo 6 $ createNet (fromList [(W11, 1), (W12, 2), (W13, 3), (W21, 4), (W22, 5), (W23, 6), (W31, 7), (W32, 8), (W33, 9), (B1, 10), (B2, 11), (B3, 12)]))
                             )

testCreateNet     = TestCase (assertEqual "for (N 1 1 1 1 1 1 1 1 1 1 1 1- 0.2*N 0.3 10 7 2 3.2 15 (-1) (-0.1) 1 1 1 1)"
                                          (N 0.94 (-1) (-0.4) 0.6 0.36 (-2) 1.2 1.02 0.8 0.8 0.8 0.8)
                                          (roundNetTo 6 $ N 1 1 1 1 1 1 1 1 1 1 1 1- 0.2*N 0.3 10 7 2 3.2 15 (-1) (-0.1) 1 1 1 1)
                             )

w :: [[Expr NetVar ]]
w = [[Var W11, Var W12, Var W13],[Var W21, Var W22, Var W23],[Var W31, Var W32, Var W33]]

input :: [Expr NetVar]
input = [Var I1, Var I2, Var I3]

b :: [Expr NetVar]
b = [Var B1, Var B2, Var B3]

testZipWith       = TestCase (assertEqual "for (zipWith (+) (matrixVecProd w input) b)"
                                          [((Var W11 `Times` Var I1) `Plus` ((Var W12 `Times` Var I2) `Plus` (Var W13 `Times` Var I3))) `Plus` Var B1, ((Var W21 `Times` Var I1) `Plus` ((Var W22 `Times` Var I2) `Plus` (Var W23 `Times` Var I3))) `Plus` Var B2, ((Var W31 `Times` Var I1) `Plus` ((Var W32 `Times` Var I2) `Plus` (Var W33 `Times` Var I3))) `Plus` Var B3]
                                          (zipWith (+) (matrixVecProd w input) b)
                             )
tests = TestList [TestLabel "testVecProd" testVecProd, TestLabel "testMatrixVecProd" testMatrixVecProd, TestLabel "testLogistic" testLogistic, TestLabel "testSquareError" testSquareError, TestLabel "testRunNet" testRunNet, TestLabel "testNetUpdate" testNetUpdate, TestLabel "testCreateNet" testCreateNet, TestLabel "testZipWith" testZipWith]

main :: IO Counts
main = runTestTT tests