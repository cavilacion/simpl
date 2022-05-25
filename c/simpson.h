#ifndef SIMPSON_H
#define SIMPSON_H

typedef double (*f_ptr)(double);

double adaptiveSimpson (f_ptr f, double a, double b, double eps);

#endif
