/* 독립 t검정 sas */
data st2;
 input class x;
cards;
1 90
1 95
1 80
1 88
1 92
2 75
2 80
2 85
2 90
2 95
2 72
2 77
2 76

run;

proc ttest;
 class class;
 var x;
run;

/* 독립 t검정 sas iml */
proc iml;
 x1 = {90 ,95 ,80 ,88 ,92};
 x2 = {75, 80, 85, 90, 95, 72, 77, 76};
 nx1 = nrow(x1);
 nx2 = nrow(x2);
 mx1 = sum(x1)/5;
 mx2 = sum(x1)/8;
 d2x1 = sum((x1-mx1)#(x1-mx1));
 d2x2 = sum((x2-mx2)#(x2-mx2));
 sp2 = (d2x1 + d2x2) / 11;
 to = (mx1-mx2)/sqrt(sp2*(1/5+1/8));
 pv =2*(1-probt(abs(t0),11));

 print x1 x2 nx1 nx2 mx1 mx2 sp2 t0 pv;

/* 독립 t검정 비모수 sign test */
proc iml;
 x1 = {90 ,95 ,80 ,88 ,92};
 x2 = {75, 80, 85, 90, 95};
 diff = x1 - x2;
 sign = j(nrow(diff),1,.);
  do i = 1 to nrow(diff);
   if diff[i] >0 then sign[i] =1 ;
   else if diff[i] <0 then sign[i] = -1;
 end;

posCount = sum(sign =1);
negCount = sum(sign =-1);
print diff sign posCount negCount;

/* 표본의 길이가 동일하지 않을 경우 rign test가 불가능 그래서 wilcoxon signed rank test 진행*/
 /* 독립 t검정 비모수 wilcoxon signed rank test */
 proc iml;
 x1 = {90 ,95 ,80 ,88 ,92};
 x2 = {75, 80, 85, 90, 95, 72, 77, 76};
 n1 = nrow(x1); n2= nrow(x2);
 x12 = x1//x2;
 rx12 = ranktie(x12);
 temp= j(n1,1,1)//j(n2,1,0);
 Wp=sum(temp#rx12);
 
 print x1 x2 x12 temp Wp;
