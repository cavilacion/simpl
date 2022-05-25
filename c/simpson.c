#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "simpson.h"

/* Example function
 */
double square (double x) {
  return x*x;
}

double simpsonEstimate (double a, double fa, 
  double m, double fm,
  double b, double fb ) {
  return fabs(b-a)/6*(fa+4*fm+fb);
}

double adaptiveSimpsonRec (f_ptr f, 
    double a, double fa,
    double m, double fm,
    double b, double fb,
    double eps,
    double A) {
  double lm = (a+m)/2;
  double flm = f(lm);
  double left = simpsonEstimate (a, fa, lm, flm, m, fm);
  double rm = (m+b)/2;
  double frm = f(rm);
  double right = simpsonEstimate (m, fm, rm, frm, b, fb);
  double delta = left + right - A;
  if (fabs(delta) <= 15*eps) {
    return left + right + delta/15;
  }
  double L=adaptiveSimpsonRec (f,a,fa,lm,flm,m,fm,eps/2,left);
  double R=adaptiveSimpsonRec (f,m,fm,rm,frm,b,fb,eps/2,right);
  return L+R;
}

double adaptiveSimpson (f_ptr f,  // function to be integrated
    double a,                     // left-end of interval
    double b,                     // right-end of interval
    double eps) {                 // precision 
  double fa = f(a);
  double fb = f(b);
  double m = (a+b)/2;
  double fm = f(m);
  double A = simpsonEstimate (a, fa, m, fm, b, fb);
  return adaptiveSimpsonRec (f,a,fa,m,fm,b,fb,eps,A);
}

/*int main (int argc, char* argv[]) {
  f_ptr f;
  f = square;
  printf ("mu f (0,3)=%f\n",adaptiveSimpson(f,0,3.0,0.00000000001));
  return 0;
}*/

