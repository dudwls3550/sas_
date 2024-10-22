/*일표본 t검정_sas*/
data data;
input x;
cards;
170
168
172
167
165
159
run;

proc univariate mu0=170;
var x;
run;

/*일표본 t검정_sas iml*/
proc iml;
 x= {170,168,172,167,165,159};
 nx = nrow(x);
 sx =sum(x);
 mx = sx/6;
 dx = x-mx;
 s2 = ssq(dx)/(nx-1);
 t0 = (mx-170)/sqrt(s2/6);
 pv = 2*(1-probt(t0,nx-1));

print x nx sx mx dx s2 t0 pv;

/*일표본 비모수 검정 rank test_sas iml */
proc iml;
 x={700, 820, 850, 900, 950, 720, 770, 760, 680, 750};
 n=nrow(x);
 mu0=800;
 
 
 B=0;
 do i=1 to n;
  if x[i,1] > mu0 then B=B+1;
    /*print i  B;*/
 end;
 
 print x  B;

 /*일표본 비모수 검정 wilcoxon signed rank test_sas iml*/
    proc iml;
 x={700, 820, 850, 900, 950, 720, 770, 760, 680, 750};
 n=nrow(x);
 mu0=800;

 Zi=abs(x-800);
 rtZi = ranktie(Zi);
 Wp = sum((x>800)#rtZi);
 print x Zi rtZi Wp;

 
