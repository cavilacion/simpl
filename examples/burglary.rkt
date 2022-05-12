#lang simpl
b ~ bern(0.0001);
if (b == 1) {
  a := 0.95
} else {
  a := 0.001
};
score (a);
return b
