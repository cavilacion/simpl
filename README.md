# simpl
This is a toy probabilistic programming system written in beautiful racket

## Installation procedures
Install docker on your machine.
After cloning this repository, build a docker image by executing the following command within the repository:
''' 
$ docker build . 
'''

If successful, inspect the IDs of all the images you have to obtain your appropriate image ID `N`.
This can be done by executing
'''
$ docker image list
REPOSITORY    TAG     IMAGE ID    ...
<none>        <none>  N
'''

Use this ID `N` to execute the image as follows (only the first couple of digits is required to enter):
'''
$ docker run -it N
'''


