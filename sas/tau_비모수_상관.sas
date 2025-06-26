data kendall_data;
 input x y;
 datalines;
 300 250
 350 320
 400 380
 450 410
 510 430
;
run;

proc corr data = kendall_data kendall;
 var x;
 with y;
run;




proc iml;
    /* 데이터 입력 */
    x = {300, 350, 400, 450, 510}; /* 소득 */
    y = {250, 320, 380, 410, 430}; /* 소비 */
    n = nrow(x); /* 데이터 개수 */

    /* Concordant와 Discordant를 계산하기 위한 초기화 */
    C = 0; /* Concordant Pair 개수 */
    D = 0; /* Discordant Pair 개수 */

    /* 순서쌍 비교 */
    do i = 1 to n-1;
        do j = i+1 to n;
            /* (x_i, x_j)와 (y_i, y_j)를 비교 */
            if (x[i] < x[j] & y[i] < y[j]) | (x[i] > x[j] & y[i] > y[j]) then
                C = C + 1; /* Concordant Pair */
            else if (x[i] < x[j] & y[i] > y[j]) | (x[i] > x[j] & y[i] < y[j]) then
                D = D + 1; /* Discordant Pair */
            /* 동일한 경우 (tie)는 무시 */
        end;
    end;

    /* Kendall's Tau 계산 */
    Tau = (C - D) / (0.5 * n * (n-1)); /* 0.5 * n * (n-1)은 총 순서쌍의 개수 */

    /* 결과 출력 */
    print "Concordant Pairs (C):" C;
    print "Discordant Pairs (D):" D;
    print "Kendall's Tau:" Tau;
quit;


proc iml;
    /* 데이터 입력 */
    x = {300, 350, 400, 450, 510};
    y = {250, 320, 380, 410, 430};
    beta1 = 0.8;
    n = nrow(x);


    residuals = y - beta1 * x; /* r_i = y - beta1 * x */

    P = 0; /* Concordant Pairs */
    Q = 0; /* Discordant Pairs */

    do i = 1 to n-1;
        do j = i+1 to n;
            if residuals[j] > residuals[i] then
                P = P + 1;
            else if residuals[j] < residuals[i] then
                Q = Q + 1;
        end;
    end;


    Tau = (P - Q) / (P + Q);

    print "Residuals (잔차):" residuals;
    print "부합 순서쌍 (P):" P;
    print "비부합 순서쌍 (Q):" Q;
    print "Kendall's Tau (with Residuals):" Tau;
quit;
