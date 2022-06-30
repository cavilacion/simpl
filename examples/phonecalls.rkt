#lang simpl/symbex
/*
 In this model, we are interested in finding the probability
 of it being a week day, given that we get 4 phone calls in
 one hour as a telephone employee. On week days, we expect
 10 calls a day; on weekends 3.
*/

x ~ bern(5/7);  // is it a week day?
if (x == 1) {   
  r:=10         // we expect 10 calls
} else {
  r:=3          // we expect 3 calls
};

// we observe 4 from poisson(r) (use density function)
score (r^4*2.718281828459045^(-r)/24); 

return x
