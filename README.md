# Framework for Backpropagation with Matrices

This is the Github repository for my masters thesis about backpropagation with matrices.  
This repository contains all the code I wrote and some examples of how to use it.

The different implementations for the automatic differentiation prt are in folder [./src/app/AD](src/app/AD).  
The implementations for backpropagation with examples for feed forward neural networks can be found in [./src/app/Backpropagation](src/app/Backpropagation).  
There are some unit test files in the [./src/app/Tests](src/app/Tests) folder, the can be used to check the corretness of the algorithms.

## Running the code
### Prerequisites
To run the code, you need [cabal](https://www.haskell.org/cabal/), here we use version 2.4.  
You need GHC with base version >=4.11 and < 5, we used base version 4.13.3.0.  
GHC can easily be installed or changed versions using [ghcup](https://www.haskell.org/ghcup/).  
You need to have the [MNIST dataset](http://yann.lecun.com/exdb/mnist/) files downloaded and know the path to the folder they are in, you need the four files at the top of [this page](http://yann.lecun.com/exdb/mnist/).  
Lastly, you need to have the hmatrix library installed, you can find their installation guide [here](https://github.com/haskell-numerics/hmatrix/blob/master/INSTALL.md).

### Executing the examples
To run the examples in the [./src/app/Backpropagation](src/app/Backpropagation) folder,  you can follow these steps.  
In the command line you start  in the folder where you downloaded this github page.
```
$ cd  src
$ cabal run Matrix-Backpropagation "[path to MNIST dataset]"
```
Here you need to replace `[path to MNIST dataset]` with the actual path where you have downloaded the MNIST dataset on you computer.  
To change what examples run when running that command, you can change the [./src/app/Main.hs](src/app/Main.hs) file to the examples you want.