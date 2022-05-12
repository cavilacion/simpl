# Simple Imperative Minimal Probabilistic Language
This is a toy probabilistic programming system written in beautiful racket.
The goal is to provide an off-the-shelf imperative programming language with built-in language constructs for statistical computations such as sampling and bayesian inversion.

## Installation Procedures
Install docker on your machine if not running Linux.
The system has only been tested on Debian (bullseye) and Arch Linux.

### In Linux
Make sure you have installed `git`, `racket`, and `sqlite3`.
Also install `beautiful-racket` by executing the following command:
```
$ raco pkg install beautiful-racket
```
After cloning this repository and changing the working directory to it, run
```
$ raco pkg install
```
to install simpl.
Run the unit (some are probabilistic and might give errors) tests with
```
$ raco test tests
```

### Using Docker
After cloning this repository, build a docker image by executing the following command within the repository:
```
$ docker build . 
```

If successful, inspect the IDs of all the images you have to obtain your appropriate image ID `N`.
This can be done by executing
```
$ docker image list
REPOSITORY    TAG     IMAGE ID    ...
<none>        <none>  N
```
Use this ID `N` to execute the image as follows (only the first couple of digits is required to enter):
```
$ docker run -it N
```

## Testing
Run the unit tests with the command below.
NB: some tests fail with a probability of approximately 5%; most test runs should succeed though, so rerunning the tests after a failure likely "fixes" things.
```
$ raco test tests
```

## Examples
There is some examples available in the directory `examples`. 

### Normal sampling
The file `normal-sampling.rkt`, for example, samples 1000 times from a normal(100,20) distribution and calculates the sample mean and sample variance.  
```
$ racket examples/normal-sampling.rkt
```
The result should be _approximately_ (100 20).

### Burglary alarm
The file `burglary.rkt` contains a probabilistic model for a simple Bayesian inference procedure on Bernoulli distributed random variables representing the occurence of a burglary and an alarm going off.
```
$ racket examples/burglary.rkt
```
Out of 10.000 executions, the result will 9.999 times be (0.001 0), meaning that outcome 0 under this model has a weight (unnormalized probability) of 0.001.
Very rarely, the result is (0.95 1), meaning that outcome 1 has a weight (unnormalized probability) of 0.95.
