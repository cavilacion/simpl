#lang simpl/symbex
gender ~ bern(0.496);
if (gender == 0) {
  mean := 175;
  var  := 64
} else {
  mean := 165;
  var  := 49
};
observe 190 from normal (mean,var);
return gender
