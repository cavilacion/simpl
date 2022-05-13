#lang simpl
i := 0;
k := 0;
n := 5000;
while (i < n) {
  u ~ uniform(0,1);
  v ~ uniform(0,1);
  if (u*u + v*v < 1) {
    /* random point is within quarter of a circle */
    k:=k+1
  };
  i := i+1
};

// this value should be approximately pi
return 4.0*k/n