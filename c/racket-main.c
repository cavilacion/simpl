#include <stdio.h>
#include <stdlib.h>

#include "sample.h"

double likelihood;

void score(double t) {
  likelihood *= t;
}

void racket_main () {
  likelihood=1.0;
  unsigned x = sampleBern(0.0001);
  double a;
  if (x==1) {
    a=0.95;
  } else {
    a=0.001;
  }
  score (a);
  unsigned y = sampleBern(a);
  printf ("%u (%f)\n", x, likelihood);
}

