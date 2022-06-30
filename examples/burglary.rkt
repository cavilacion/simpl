#lang simpl/symbex
b ~ bern(0.0001);
if (b == 1) {
  a := 0.95
} else {
  a := 0.001
};
observe 1 from bern(a); // sugar for `score(a)`
return b
