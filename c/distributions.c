#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "distributions.h"

/* This function implements the PMF of a
 * binomial distribution, without use of combinations
 * to prevent integer overflow
 */
double binomPMF (unsigned k, unsigned n, double p) {
  double P = 1.0;
  for (unsigned i=0; i < n; i++) {
    P *= (n-i);
    P /= (i < k)?(i+1):1;
    P /= (i < n-k)?(i+1):1;
    P *= (i < k)?p:1;
    P *= (i < (n-k))?(1-p):1;
  }
  return P;
}

/* This function implements the PMF of a
 * poisson distribution
 */
double poissonPMF (unsigned k, double r) {
  double p = exp(-r);
  for (unsigned i=1; i<=k; i++) {
    p *= r;
    p /= (k-i+1);
  }
  return p;
}

