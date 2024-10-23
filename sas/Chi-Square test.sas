     /* sas */
 /* Chi-Square test */
data chi_square_test;
 input gender$ choice$ count;
datalines;
M Coffee 30
M Tea 20
F Coffee 25
F Tea 25
run;

proc freq data = chi_square_test;
tables gender* choice / chisq;
weight count;
run;

     /* sas iml */
proc iml;
 observed = {30 20,
             25, 25}

 row_sum = observed[,+];
 col_sum =observed[+,];
 total = sum(observed);

 expected = (row_sum * col_sum) / total;

 chi_square = sum((observed - expected)##2 / expected);

 df = (nrow(observed)-1)*(ncol(observed)-1);
 pv = 1- probchi(chi_square,df);

print observed expected chi_square pv;
