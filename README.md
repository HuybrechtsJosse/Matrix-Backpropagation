# Framework for Backpropagation with Matrices

This is the Github repository for my masters thesis about backpropagation with matrices.  
This repository contains all the code I wrote and some examples of how to use it.

The different implementations for the automatic differentiation part are in folder [./src/app/AD](src/app/AD).  
The implementations for backpropagation with examples for feed forward neural networks can be found in [./src/app/Backpropagation](src/app/Backpropagation).  
There are some unit test files in the [./src/app/Tests](src/app/Tests) folder, they can be used to check the correctness of the algorithms.

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

This runs the test files and executes 4 examples.  
The first 2 examples use the following network without hidden layers to train on the MNIST dataset:  
<img src="https://user-images.githubusercontent.com/43749574/172375332-daee12e8-806c-49c8-9ec4-ccb77dd9a959.png" alt="drawing" height="300"/>  
The last 2 examplles use this network with 4 hidden layers to train on the MNIST dataset:  
<img src="https://user-images.githubusercontent.com/43749574/172375743-a6f5f9e0-e8a7-4a73-b6e2-129e2482d618.png" alt="drawing" height="300"/>  
The result of these commands will look like this.  

```
------------------
Start Tests 
------------------
Cases: 7  Tried: 7  Errors: 0  Failures: 0
Cases: 11  Tried: 11  Errors: 0  Failures: 0
Cases: 16  Tried: 16  Errors: 0  Failures: 0
Cases: 8  Tried: 8  Errors: 0  Failures: 0
Cases: 4  Tried: 4  Errors: 0  Failures: 0
------------------
Finished Tests 
------------------
Loaded data.
[Epoch 1]
(Batch 1)
Trained on 50 points.
Training error:   78.00%
Validation error: 87.00%
(Batch 2)
Trained on 50 points.
Training error:   52.00%
Validation error: 56.00%
(Batch 3)
Trained on 50 points.
Training error:   34.00%
Validation error: 62.00%
(Batch 4)
Trained on 50 points.
Training error:   26.00%
Validation error: 42.00%
(Batch 5)
Trained on 50 points.
Training error:   22.00%
Validation error: 50.00%

... (The same for every Epoch)

[Epoch 5]
(Batch 1)
Trained on 50 points.
Training error:   16.00%
Validation error: 22.00%
(Batch 2)
Trained on 50 points.
Training error:   18.00%
Validation error: 23.00%
(Batch 3)
Trained on 50 points.
Training error:   6.00%
Validation error: 21.00%
(Batch 4)
Trained on 50 points.
Training error:   12.00%
Validation error: 24.00%
(Batch 5)
Trained on 50 points.
Training error:   18.00%
Validation error: 18.00%
------------------
Backprop: 6746.063842521s 
------------------
Loaded data.
[Epoch 1]
(Batch 1)
Trained on 5000 points.
Training error:   12.98%
Validation error: 14.46%
(Batch 2)
Trained on 5000 points.
Training error:   9.18%
Validation error: 10.27%
(Batch 3)
Trained on 5000 points.
Training error:   9.68%
Validation error: 10.44%
(Batch 4)
Trained on 5000 points.
Training error:   10.48%
Validation error: 10.73%
(Batch 5)
Trained on 5000 points.
Training error:   8.42%
Validation error: 9.83%

... (The same for every Epoch)

[Epoch 5]
(Batch 1)
Trained on 5000 points.
Training error:   7.50%
Validation error: 8.44%
(Batch 2)
Trained on 5000 points.
Training error:   7.62%
Validation error: 9.00%
(Batch 3)
Trained on 5000 points.
Training error:   6.94%
Validation error: 8.44%
(Batch 4)
Trained on 5000 points.
Training error:   8.82%
Validation error: 10.62%
(Batch 5)
Trained on 5000 points.
Training error:   7.56%
Validation error: 8.45%
------------------
Indexed Backpropagation: 14.391924205s 
------------------
Loaded data.
[Epoch 1]
(Batch 1)
Trained on 5000 points.
Training error:   89.04%
Validation error: 88.68%
(Batch 2)
Trained on 5000 points.
Training error:   62.04%
Validation error: 62.65%
(Batch 3)
Trained on 5000 points.
Training error:   30.50%
Validation error: 30.01%
(Batch 4)
Trained on 5000 points.
Training error:   19.50%
Validation error: 19.92%
(Batch 5)
Trained on 5000 points.
Training error:   14.66%
Validation error: 16.15%

... (The same for every Epoch)

[Epoch 5]
(Batch 1)
Trained on 5000 points.
Training error:   4.34%
Validation error: 6.24%
(Batch 2)
Trained on 5000 points.
Training error:   4.82%
Validation error: 6.80%
(Batch 3)
Trained on 5000 points.
Training error:   4.50%
Validation error: 6.47%
(Batch 4)
Trained on 5000 points.
Training error:   4.06%
Validation error: 6.09%
(Batch 5)
Trained on 5000 points.
Training error:   4.08%
Validation error: 6.09%
------------------
Indexed Backpropagation Mutable Arrays: 1656.247498055s 
------------------
Loaded data.
[Epoch 1]
(Batch 1)
Trained on 5000 points.
Training error:   88.88%
Validation error: 89.72%
(Batch 2)
Trained on 5000 points.
Training error:   65.24%
Validation error: 65.07%
(Batch 3)
Trained on 5000 points.
Training error:   51.04%
Validation error: 51.94%
(Batch 4)
Trained on 5000 points.
Training error:   30.90%
Validation error: 30.10%
(Batch 5)
Trained on 5000 points.
Training error:   23.20%
Validation error: 23.79%

... (The same for every Epoch)

[Epoch 5]
(Batch 1)
Trained on 5000 points.
Training error:   4.04%
Validation error: 5.91%
(Batch 2)
Trained on 5000 points.
Training error:   4.66%
Validation error: 6.23%
(Batch 3)
Trained on 5000 points.
Training error:   4.90%
Validation error: 6.42%
(Batch 4)
Trained on 5000 points.
Training error:   3.94%
Validation error: 5.93%
(Batch 5)
Trained on 5000 points.
Training error:   3.78%
Validation error: 5.59%
------------------
Indexed Backpropagation Static: 376.700868944s 
------------------
```
For every example it follows the same structure, for every Epoch and Batch it shows the number of datapoints it has trained on and the training- and testerror at that point in the trainingprocess. At the end of each example it shows the execution time needed to run that example.
The percentages and execution time differ a little bit every time, but should be more or less the same.