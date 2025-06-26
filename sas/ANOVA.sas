data example;
 input group  value;
 cards;
 1 90
 1 85
 1 88
 1 92
 1 87
 2 75
 2 80
 2 78
 2 85
 2 82
 3 82
 3 85
 3 88
 3 90
 3 87
run;

 /* 2. ANOVA */
proc anova data =example;
class group;
model  value = group;
run;


proc iml;
 x= {90, 85, 88, 92, 87,   
     75, 80, 78, 85, 82,   
     82, 85, 88, 90, 87};  
 group = {1, 1, 1, 1, 1,    
          2, 2, 2, 2, 2,    
          3, 3, 3, 3, 3}; 


 mean1 = mean(x[loc(group=1)]);
 mean2 = mean(x[loc(group=2)]);
 mean3 = mean(x[loc(group=3)]);
 
 overall_mean = mean(x);
 
 /* 그룹 간 제곱합 (SSB) */
 n1 = sum(group=1);
 n2 = sum(group=2);
 n3 = sum(group=3);
 SSB = n1*(mean1 - overall_mean)##2 +
       n2*(mean2 - overall_mean)##2 +
       n3*(mean3 - overall_mean)##2;
       
  /* 그룹 내 제곱합 (SSW) */
 SSE1 = sum((x[loc(group=1)] - mean1)##2);
 SSE2 = sum((x[loc(group=2)] - mean2)##2);
 SSE3 = sum((x[loc(group=3)] - mean3)##2);
 SSW = SSE1 + SSE2 + SSE3;
 
 /* 총 제곱합 (SST) */
 SST = SSB + SSW;
 
 /* 자유도 계산 */
 dfb = 3 -1 ;
 dfw = nrow(x) - 3;
 
 /* 분산 계산*/
 MSB = SSB / dfb;
 MSW = SSW / dfw;
 
 F = MSB / MSW;
 
 pv = 1- probf(F, dfb,dfw);
 
 print mean1 mean2 mean3 overall_mean SSB SSW SST F pv;
 

 /* 등분산성 검증 (Levene's test) */
proc glm data =example;
class group;
model value = group;
means group / hovtest = levene;
run;

 /* 사후검정 (Post Hoc test) */
proc glm data=example;
class group;
model value = group;
means group / tukey;
run;
