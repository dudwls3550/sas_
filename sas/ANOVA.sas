          /* 2. ANOVA */
  /* sas */
data example;
 input gtoup value;
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

proc anova data= example;
class group;
model value = group;
run;

  /* sas iml */
proc iml;
 x= {90, 85, 88, 92, 87,   
     75, 80, 78, 85, 82,   
     82, 85, 88, 90, 87};  
 group = {1, 1, 1, 1, 1,    
          2, 2, 2, 2, 2,    
          3, 3, 3, 3, 3}; 

mean1 = mean(x[loc(gorup=1)]);
mean2 = mean(x[loc(gorup=2)]);
mean3 = mean(x[loc(group=3)]);

overall_mean = mean(x);


 /* 그룹 간 제곱합 (SSB) */
n1 = nrow(group=1);
n2 = nrow(group=2);
n3 = nrow(group=3);
SSB = n1*(mean1 - overall_mean)##2 +
      n2*(mean2 - overall_mean)##2 +
      n3*(mean3 - overall_mean)##2;

 /* 그룹 내 제곱합 (SSW) */
SSE1 = sum((x[loc(group=1)]-mean1)##2);
SSE2 = sum((x[loc(group=2)]-mean2)##2);
SSE3 = sum((x[loc(group=3)]-mean3)##2);
SSW = SSE1 + SSE2 + SSE3 ;

 /* 총 제곱합 (SST) */
SST = SSB + SSW;

 /* 자유도 계산 */
dfb = 3-1;
dfw = nrow(x) -3;

 /* 분산 계산 */
MSB = SSB / dfb;
MSW = SSW / dfw;

F = MSB / MSW;

pv = 1- probf(F,dfb,dfw);

print mean1 mean2 mena3 overall_mean SSB SSW SST F pv;


 /* 1. 등분산성 검증 (Levene's test) */
proc glm data =example;
class group;
model value = group;
means group / hovtest = levene;
run;
/*
R-Square (R^2) = 회귀 모델의 설명력 
Coeff Var (Coefficinet of Variation) = 변동성 표준화 
Root MSE (Mean Square Error) = 평균 제곱 오차의 제곱근, 데이터의 예측값 간의 차이
value maen (Mean)= 전체 데이터 평균
*/

 /* 3. 사후검정 (Post Hoc test) */
proc glm data =example;
class group;
model value = group;
means group / tukey;
run;
