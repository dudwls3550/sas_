proc iml;
 y = {250, 320, 380, 420, 430};
 x = {1 2 30 300,
      1 3 28 350,
      1 2 32 400,
      1 1 36 450,
      1 2 29 510};
 
 xt =t(x);
 xtx = xt*x;
 xtx_inv = inv(xtx);
 xty = xt*y;
 beta = xtx_inv * xty;
 y_hat = x*beta;
 residuals = y - y_hat;
 SSE = residuals`*residuals;
 y_mean = mean(y);
 MSE = SSE / 4
 SST = t(y - y_mean)*(y - y_mean);
 R_squared = 1 - (SSE / SST);
 
 print 'beta' beta;
 print 'y_hat' y_hat;
 print 'Residuals' residuals;
 print 'SSE' SSE;
 print "MSE" MSE;
 print 'SST' SST;
 print 'R-squared' R_squared;
 

proc iml;
 y = {250, 320, 380, 410, 430};
 x = {300, 350, 400, 450, 510};
  beta1 = 0.8; n = nrow(x);
 diff= y - beta1*x;
 P=0; Q=0;
 do i=1 to n-1;
  do j = i + 1 to n;
   if (x[i,1] - x[j,1]) * (diff[i,1] - diff[j,1]) > 0 then P = P + 1;
    else Q = Q + 1 ;
    print i j P Q ;
  end;
 end;
 tau = P-Q;
print diff tau;

data st2;
 input univ toeic;
cards;
1 700	
1 800	
1 750	
1 850	
1 870	
2 500	 
2 600	
2 700	
2 550	
3 650	
3 300	
3 250	
3 400	
3 500	
3 600
run;

proc anova;
 class univ;
 model toeic=univ;
run;

proc iml;
 x1 = {700, 800, 750, 850, 870};
 x2 = {500, 600, 700, 550};
 x3 = {650, 300, 250, 400, 500, 600};
 n1 = nrow(x1); n2=nrow(x2); n3=nrow(x3); n = n1+ n2+ n3;
 x1bar = sum(x1)/n1;
 x2bar = sum(x2)/n2;
 x3bar = sum(x3)/n3;
 txbar = sum(x1//x2//x3)/n;
 ss1 = ssq(x1-x1bar);ss2 = ssq(x2-x2bar);ss3 = ssq(x3-x3bar);
 SSE = ss1 + ss2 + ss3;
 TSS = ssq((x1//x2//x3)-txbar);
 SSa = TSS - SSE;
 F0 = ((SSa/2)/(SSE/(n-3)));
 pv = 1-probf(F0,2,n-3);
 print x1 x2 x3 txbar SSa SSE TSS F0 pv;
     