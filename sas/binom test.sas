     /* sas */
 /* binom test */
data binom_test;
input outcome$ count;
datalines;
success 7
failure 3
run;

proc freq data = binom_test;
tables outcome / binomial(p=0.5);
weight count;
run;

     /* sas iml */
proc iml;
 x = 7;
 n= 10;
 p0= 0.5;

 pv = 2* min(cdf("Binomial",x,p0,n), 1- cdf("Binomial",x-1,p0,n));

print x n p0 pv;
