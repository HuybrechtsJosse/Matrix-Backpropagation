{-# LANGUAGE MultiParamTypeClasses #-}
module Tests.ReverseModeIndexedTest where

------ IMPORTS ------
import AD.ForwardMode (XY(X))
import AD.ReverseModeIndexed (reverseADI, absBI, IndexedDualR (sndID))
import IndexedMatrix2 (IndexedMatrix(IM))
import IndexedSemiring (IndexedExpr(..), IndexedSemiring (one), Indexed(..))
import Numeric.LinearAlgebra (Transposable(..))
import Test.HUnit

instance Indexed Double where
    rows _ = 1
    cols _ = 1
instance Transposable Double Double where
    tr   = id 
    tr'  = id

testIndexedADExp  = TestCase (assertEqual "for (absBI (sndID $ reverseADI (const (IM 1 1 2.45)) X (Exp (Times (Var X) (Var X)))) (one 1))"
                                          (IM 1 1 1981.7492734892296::IndexedMatrix)
                                          (absBI (sndID $ reverseADI (const (IM 1 1 2.45)) X (Exp (Times (Var X) (Var X)))) (one 1))
                             )

testIndexedADLog  = TestCase (assertEqual "for (absBI (sndID $ reverseADI (const (IM 1 1 2.45)) X (Log (Times (Var X) (Var X)))) (one 1))"
                                          (IM 1 1 0.8163265306122448)
                                          (absBI (sndID $ reverseADI (const (IM 1 1 2.45)) X (Log (Times (Var X) (Var X)))) (one 1))
                             )

testIndexedADDiv  = TestCase (assertEqual "for (absBI (sndID $ reverseADI (const (IM 1 1 2.45)) (Div (Times (Var X) (Var X)) (Plus (Var X) One)) (one 1)))"
                                          (IM 1 1 0.9159840369670236)
                                          (absBI (sndID $ reverseADI (const (IM 1 1 2.45)) X (Div (Times (Var X) (Var X)) (Plus (Var X) (One 1)))) (one 1))
                             )

testIndexedADNegate  = TestCase (assertEqual "for (absBI (sndID $ reverseADI (const (IM 1 1 2.45)) X (Negate (Times (Var X) (Var X)))) (one 1))"
                                          (IM 1 1 (-4.9))
                                          (absBI (sndID $ reverseADI (const (IM 1 1 2.45)) X (Negate (Times (Var X) (Var X)))) (one 1))
                             )

-- Reverse Mode IO Tests

tests = TestList [
                  TestLabel "testIndexedADExp" testIndexedADExp,
                  TestLabel "testIndexedADLog" testIndexedADLog,
                  TestLabel "testIndexedADDiv" testIndexedADDiv,
                  TestLabel "testIndexedADNegate" testIndexedADNegate
                 ]

main :: IO Counts
main = runTestTT tests