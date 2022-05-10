# Simple Imperative Minimal Probabilistic Language
This is a toy probabilistic programming system written in beautiful racket

## Installation Procedures
Install docker on your machine if not running debian/ubuntu.

### In Debian/Ubuntu
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

## After Installation
Run the unit (some are probabilistic and might give errors) tests with
```
$ raco test tests
```


