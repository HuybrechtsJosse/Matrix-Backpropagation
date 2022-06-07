module Main where

import qualified Backpropagation.BackpropagationFull as BPF
import qualified Backpropagation.BackpropagationIndexed as BPI
import qualified Backpropagation.BackpropagationIndexedIO as BPIIO
import qualified Backpropagation.BackpropagationIndexedStatic as BPS
import qualified Tests.BackpropagationFullTest as TBPF
import qualified Tests.BackpropagationSquareMatrixTest as TBPSQ
import qualified Tests.BackpropagationIndexedTest as TBPI
import qualified Tests.Backpropagation3x3Test as TBP3x3
import qualified Tests.ForwardADTest as TFAD
import Text.Printf (printf)

runTests :: IO ()
runTests = do
    printf "------------------\nStart Tests \n------------------\n"
    TBPF.main
    TBPSQ.main
    TBPI.main
    TBP3x3.main
    TFAD.main
    printf "------------------\nFinished Tests \n------------------\n"

main :: IO ()
main = do
    runTests
    BPIIO.main
    BPI.main
    BPF.main
    BPS.main