module Tests.MatrixTest where

------ IMPORTS ------
import Matrix
import Test.HUnit
import Prelude hiding ((<>))
import AD.ForwardMode (Norm(norm))

testMatrixSum  = TestCase (assertEqual "for ([[1,2],[3]] + [[-3,1.5],[0.01]])"
                                          [[-2,3.5],[3.01]]
                                          ([[1,2],[3]] + [[-3,1.5],[0.01]])
                             )

testMatrixMultiplication  = TestCase (assertEqual "for ([[1,2,3],[1.5,-1,4]] <> [[2,4],[0.1,-1],[3.5,8]])"
                                          [[26.2],[34.9]]
                                          ([[1,2,3],[1.5,-1,4]] <> [[2],[0.1],[8]])
                             )
testPointwiseMultiplication  = TestCase (assertEqual "for ([[1,2,3],[1.5,-1,4]] * [[-3,1.5,0.2],[2,1,9]])"
                                          [[-3, 3, 0.6000000000000001],[3,-1,36]]
                                          ([[1,2,3],[1.5,-1,4]] * [[-3,1.5,0.2],[2,1,9]])
                             )
testExp  = TestCase (assertEqual "for (exp [[1,2,3],[1.5,-1,4]])"
                                          [[2.7182818284590452, 7.3890560989306502, 20.0855369231876677],[4.4816890703380648,0.36787944117144233,54.5981500331442391]]
                                          (exp [[1,2,3],[1.5,-1,4]])
                             )
testLog  = TestCase (assertEqual "for (log [[1,2,3],[1.5,2.3,4]])"
                                          [[0, 0.6931471805599453, 1.0986122886681097],[0.4054651081081644,0.8329091229351039,1.3862943611198906]]
                                          (log [[1,2,3],[1.5,2.3,4]])
                             )
testNorm  = TestCase (assertEqual "for (norm [[1],[2],[8.3],[3]])"
                                          ([[14.3],[14.3],[14.3],[14.3]]::Matrix Double)
                                          (norm [[1],[2],[8.3],[3]])
                             )
tests = TestList [
    TestLabel "testMatrixSum" testMatrixSum,
    TestLabel "testMatrixMultiplication" testMatrixMultiplication,
    TestLabel "testPointwiseMultiplication" testPointwiseMultiplication,
    TestLabel "testExp" testExp,
    TestLabel "testLog" testLog,
    TestLabel "testNorm" testNorm
    ]

main :: IO Counts
main = runTestTT tests