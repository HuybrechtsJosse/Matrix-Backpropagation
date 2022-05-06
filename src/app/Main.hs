{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
import qualified MNISTExampleBackprop as BPM
import qualified Backpropagation.BackpropagationFull as BPF
import qualified Backpropagation.BackpropagationSquareMatrix as BPSQ
import qualified Backpropagation.BackpropagationIndexed1 as BPI1
import qualified Backpropagation.BackpropagationIndexed2 as BPI2
import qualified Backpropagation.BackpropagationIndexedIO as BPIIO

main :: IO ()
main = do
    BPM.main
    BPIIO.main
    BPI2.main
    BPI1.main
    BPF.main
    BPSQ.main