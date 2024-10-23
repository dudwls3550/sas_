     /* sas */
 /* Paired ttest */
data pre post;
input pre postl
 diff = post - pre;
datalines;
90 95
85 87
88 92
92 94
87 89
run;

proc ttest data = paired_test;
paired pre*post;
run;

     /* sas iml */
proc iml; 
 pre = {90, 85, 88, 92, 87};  
 post = {95, 87, 92, 94, 89};

 diff =post-pre;

 mean_diff = mean(diff);
 std_diff = std(diff);

 n= nrow(pre);
 t0 = mean_diff /(std_diff / sqrt(n));

 df = n -1;
 pv = 2*(1-probt(abs(t0),df));
print diff mean_diff std_diff n t0 df pv;
