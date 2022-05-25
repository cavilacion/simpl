#ifndef DISTRIBUTIONS_H
#define DISTRIBUTIONS_H

#define PI 3.14159265358979323846264338327950288

double binomPMF (unsigned k, unsigned n, double p);
double poissonPMF (unsigned k, double r);
void set_normal_parameters (double m, double s);
double normalPDF (double x);

#endif
