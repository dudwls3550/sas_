proc iml;
 y = {700, 800, 750, 850, 870, 500, 600, 700, 550, 650, 300, 250, 400, 500, 600};
 group = {1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3}; 
 rankidx = rank(y);
 
 /*그룹별 랭킹*/
 numgroup = max(group);
 ngroup = j(numgroup, 1, 0);
 ranksum = j(numgroup, 1, 0);
 n = nrow(y);
 do i = 1 to n;
  ranksum[group[i]] = ranksum[group[i]] + rankidx[i];
  ngroup[group[i]] = ngroup[group[i]] + 1;
 end;
  groupmeanrank = ranksum / ngroup;
 
 
 rbar = mean(rankidx);
 
 numerator = sum(ngroup # (groupmeanrank - rbar)##2);
 denominator = sum((rankidx - rbar)##2);
 
 H = (n - 1) * (numerator / denominator);
 df = numgroup - 1;
 pv = 1 - probchi(H, df);
 print rankidx numgroup ngroup ranksum groupmeanrank rbar numerator denominator H pv;
 
proc iml;
    y = {700, 800, 750, 850, 870, 500, 600, 700, 550, 650, 300, 250, 400, 500, 600};
    group = {1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3}; 
    N = nrow(y);
    numGroups = max(group);

    /* Step 1: Calculate observed H */
    rankIdx = rank(y);
    groupRanks = j(numGroups, 1, 0);
    groupSizes = j(numGroups, 1, 0);

    do i = 1 to N;
        groupRanks[group[i]] = groupRanks[group[i]] + rankIdx[i];
        groupSizes[group[i]] = groupSizes[group[i]] + 1;
    end;

    meanRank = mean(rankIdx);
    H_obs = (12 / (N*(N+1))) * sum(groupSizes#((groupRanks / groupSizes) - meanRank)##2);

    /* Step 2: Generate all possible permutations */
    usePermutations = 10000; /* Set the number of permutations */
    extremeCount = 0;

    do perm = 1 to usePermutations;
        permRanks = rank(ranperm(y));
        groupRanks = j(numGroups, 1, 0);

        do i = 1 to N;
            groupRanks[group[i]] = groupRanks[group[i]] + permRanks[i];
        end;

        H_perm = (12 / (N*(N+1))) * sum(groupSizes#((groupRanks / groupSizes) - meanRank)##2);

        /* Count extreme cases */
        if H_perm >= H_obs then extremeCount = extremeCount + 1;
    end;

    /* Calculate exact p-value */
    p_value = extremeCount / usePermutations;
    print H_obs p_value;
