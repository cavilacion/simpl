#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include "sample.h"
#include "racket.h"
#include "simpson.h"
#include "distributions.h"

void printUsage(char *name) {
  printf ("Usage: %s -n N \n | N is a number in the range of 1 to 10^6\n", name);
}

int main (int argc, char *argv[]) {
  srand(time(NULL));
/*  double m=0.0;
  int n;
  if (argc==1) {
    n=10;
  }
  else if (strcmp(argv[1],"-n")==0) {
    if (argc < 3) printUsage(argv[0]);
    else n=atoi(argv[2]);
  }
  else {
    printUsage(argv[0]);
  }
  printf ("Sampling n=%d times (adjust with -n option) ...\n", n);

  for (int i=0; i<n; i++) {
    racket_main();
  }*/

  set_normal_parameters (300,4.0);
  double A = adaptiveSimpson (normalPDF, 270, 300.0, 0.000000000000000001);
  printf ("A=%f\n", A);

  return 0;
}
