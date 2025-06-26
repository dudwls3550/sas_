data regression_data;
    input y x1 x2 x3 x4;
    datalines;
250 1 2 30 300
320 1 3 28 350
380 1 2 32 400
420 1 1 36 450
430 1 2 29 510
;
run;

proc reg data=regression_data;
    model y = x1 x2 x3 x4 / vif;
run;



proc iml;
    X = {1 2 30 300,
         1 3 28 350,
         1 2 32 400,
         1 1 36 450,
         1 2 29 510};
         
 x2 = X[,2];
 X_rest = X[,{1 3 4}];
 
 Xt = t(X_rest);
 XtX = Xt * X_rest;
 XtX_inv = inv(XtX);
 beta = XtX_inv * Xt * x2;
 x2_hat = X_rest * beta;
 residuals = x2 - x2_hat;
 SSE = t(residuals) * residuals;
 
 /* R^2 */
 x2_mean = mean(x2);
 SST = t(x2 - x2_mean) * (x2 - x2_mean);
 R_squared = 1 - (SSE / SST);
 
 
 /* VIF */
 VIF = 1 / (1 - R_squared);
 
print R_squared;
print VIF;