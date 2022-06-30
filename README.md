# Simple Imperative Minimal Probabilistic Language
This is a toy probabilistic programming system written in beautiful racket, which is by no means efficient or even safe.
The goal is to gain more insight in the mechanics of step-by-step probabilistic computation, with language constructs for sampling and bayesian inversion.

## Installation Procedures
Install docker on your machine if not running Linux.
The system has been tested on Debian (bullseye) and Arch Linux.

### On Linux
Make sure you have installed `git`, `racket`, and `sqlite3`.
Also install `beautiful-racket` by executing the following command:
```
$ raco pkg install beautiful-racket
```
After cloning this repository and changing the working directory to it, run
```
$ raco pkg install
```
to install the programming system.

### Using Docker
After cloning this repository, build a docker image by executing the following command within the local repository:
```
$ docker build -t debian:simpl . 
```
If successful, the docker image you just installed shows up in the list of images:
```
$ docker image list
REPOSITORY    TAG     IMAGE ID    ...
debian        simpl   N           ...
```
You can run the image in an interactive container by doing
```
$ docker run -it N
```

## Testing
Run the unit tests with 
```
$ raco test tests
```
Note that in some test cases, sampled values are checked to be within a certain range, and may fail with some low but nonzero probability.
This is to be expected.

## Examples
There are some examples available in the directory `examples`. 

### Normal sampling
The file `normal-sampling.rkt`, for example, samples 1000 times from a normal(100,20) distribution and calculates the sample mean and sample variance.  
```
$ racket examples/normal-sampling.rkt
```
The result should be _approximately_ (100 20).

The first line of `normal-sampling.rkt` is 
```
#lang simpl
```
This tells racket that it is supposed to interpret the rest of the file as the simple imperative minimalist programming language that is this tool.

### Burglary alarm
The file `gender-height.rkt` contains a probabilistic model for a simple Bayesian inference procedure on the Bernoulli distributed random variable for the gender of a person, conditioned on the fact that we observe a height of 190 cm.
```
$ racket examples/gender-height.rkt
```
The first line contains `#lang simpl/sybex`, which tells racket to do symbolic execution, rather than concrete. 
The result will thus (always) be
```
(list
 (end-config 1 9.684491216181099e-5 '((< X_0 0.496)))
 (end-config 0 0.008598284478336488 '((not (< X_0 0.496)))))
```
The structures `end-config` contain respectively the _return value_ (1 and 0 here), the _likelihood_ of the outcome, and the path condition.
The path condition is expressed using symbolic random variables, in this case X_0.
