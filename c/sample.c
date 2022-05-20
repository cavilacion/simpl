#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

#include "distributions.h"

int buffer=0;
double N=0.0;

double sampleUniformC (double a, double b) {
  double div = (RAND_MAX / (b-a));
  return a + (rand() / div);
}

unsigned sampleBern (double p) {
  double u = sampleUniformC(0,1);
  return (u<p)?1:0;
}

unsigned sampleBinom (unsigned n, double p) {
  double u = sampleUniformC(0,1);
  unsigned k=0;
  double s = binomPMF (k,n,p);
  printf ("binomPDF value: %f\n", s);
  while (s<u) {
    s += binomPMF (++k,n,p);
  }
  return (unsigned)k;
}

unsigned samplePoisson (double r) {
  double u = sampleUniformC(0,1);
  unsigned k=0;
  double s = poissonPMF (k,r);
  while (s < u) {
    s += poissonPMF (++k, r);
  }
  return (unsigned)k;
}

double sampleNormal (double mu, double var) {
  double stddev = sqrt(var);
  if (buffer) {
    buffer=0;
    return stddev*N+mu;
  }
  double u = sampleUniformC(0,1);
  double v = sampleUniformC(0,1);
  double s1 = sqrt(-2*log(u))*cos(2*PI*v);
  N = sqrt(-2*log(u))*sin(2*PI*v);
  buffer=1;
  return stddev*s1 + mu;
}

double sampleExprate (double r) {
  double u = sampleUniformC(0,1);
  return -1.0/r*log(1-u);
}



