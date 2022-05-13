#lang simpl
m := 0;
n := 1000;
x := array(1000);
i := 0;
while (i < n) {
  x[i] ~ poisson(10);
  m    := m + x[i]/n;
  i:=i+1
};
var := 0;
i := 0;
while (i < n) {
  var := var + (m - x[i])^2/(n-1);
  i := i + 1
};
return (m,var)
