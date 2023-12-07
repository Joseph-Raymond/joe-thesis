numIterations = 300; %must choose a multiple of 3 here
numYears = 10;
% intialize the results matrices
results = zeros(numIterations, 11);
results_A = zeros(numIterations, 4);
results_B = zeros(numIterations, 4);
results_C = zeros(numIterations, 4);
results_AB = zeros(numIterations, 4);
results_AC = zeros(numIterations, 4);
results_BC = zeros(numIterations, 4);
results_ABC = zeros(numIterations, 4);
strategies = cell(numIterations, 1);
mean_CV_simp_avgsimp = zeros(numIterations, 4);

%specify the fishery strategies for each specialist strategy
fishery1_strats = repmat({'A', 'B', 'C'}, [1,numIterations/3]);
fishery2_strats = repmat({'BC', 'AC', 'AB'}, [1,numIterations/3]);
fishery3_strats = repmat({'ABC'}, [1,numIterations]);
isequal(length(fishery1_strats),length(fishery2_strats),length(fishery3_strats))
for i = 1:numIterations
    n = 1;
    a1 = 5 + 2*rand(n, 1);
    a2 = 5 + 2*rand(n, 1);
    a3 = 5 + 2*rand(n, 1);
    eps1 = 0.1 * randn(n, 1);
    eps3 = 0.1 * randn(n, 1);
    eps2 = 0.1 * randn(n, 1);
    c1 = 2;
    c2 = 2;
    c3 = 2;
    b1 = 5 * rand(n, 1);
    b2 = 5 * rand(n, 1);
    b3 = 5 * rand(n, 1);
    fc1 = 0.2 + 0.1*rand(n, 1);
    fc2 = 0.2 + 0.1*rand(n, 1);
    fc3 = 0.2 + 0.1*rand(n, 1);
    rev_store = zeros(numYears, 6);
    [results,start_strat] = select_one_fishery(a1,a2,a3,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,i,results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC);
    firstyear_strat1 = start_strat;
    %specialist years
    for j = 1:numYears
    %simulate for numYears and store the  rev_temp with a shock each year
        a1_temp = a1 - 2 + 4 * rand; %adding a random variable ~ Unif[-2,2]
        a2_temp = a2 - 2 + 4 * rand;
        a3_temp = a3 - 2 + 4 * rand;
        [rev_temp,s_temp] = oneYearChoice_fs(firstyear_strat1,a1_temp,a2_temp,a3_temp,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,i,results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC);
        %store the revenue
        rev_store(j,1) = rev_temp(i,4);
        %store the simpson index
        rev_store(j,2) = rev_temp(i,5);
        %store the income index
        rev_store(j,3) = rev_temp(i,6);
    end
    [results,start_strat] = select_two_fisheries(a1_temp,a2_temp,a3_temp,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,i,results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC);
    firstyear_strat2 = start_strat;
    for j = 1:numYears
    %simulate for numYears and store the  rev_temp with a shock each year
        a1_temp = a1 - 2 + 4 * rand; %adding a random variable ~ Unif[-2,2]
        a2_temp = a2 - 2 + 4 * rand;
        a3_temp = a3 - 2 + 4 * rand;
        [rev_temp,s_temp] = oneYearChoice_fs(firstyear_strat2,a1_temp,a2_temp,a3_temp,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,i,results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC);
        %store the revenue
        rev_store(j,4) = rev_temp(i,4);
        %store the HHI index
        rev_store(j,5) = rev_temp(i,5);
        %store the income index
        rev_store(j,6) = rev_temp(i,6);
    end
    firstyear_strat3 = fishery3_strats(i);
    for j = 1:numYears
    %simulate for numYears and store the  rev_temp with a shock each year
        a1_temp = a1 - 2 + 4 * rand; %adding a random variable ~ Unif[-2,2]
        a2_temp = a2 - 2 + 4 * rand;
        a3_temp = a3 - 2 + 4 * rand;
        [rev_temp,s_temp] = oneYearChoice_fs(firstyear_strat3,a1_temp,a2_temp,a3_temp,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,i,results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC);
        %store the revenue
        rev_store(j,7) = rev_temp(i,4);
        %store the hhi index
        rev_store(j,8) = rev_temp(i,5);
        %store the income index
        rev_store(j,9) = rev_temp(i,6);
    end
    mean_CV_simp_avgsimp(i,1) = mean(rev_store(:, 1));%mean net revenue 1 fishery
    mean_CV_simp_avgsimp(i,2) = var(rev_store(:, 1))/mean(rev_store(:, 1));% CV 1-fishery
    mean_CV_simp_avgsimp(i,3) = mean(rev_store(:,2));%mean HHI 1 fishery

    mean_CV_simp_avgsimp(i,4) = mean(rev_store(:, 4));%mean net revenue 2 fishery
    mean_CV_simp_avgsimp(i,5) = var(rev_store(:, 4))/mean(rev_store(:, 4));% CV 2-fishery
    mean_CV_simp_avgsimp(i,6) = mean(rev_store(:,5));%mean HHI 2 fishery

    mean_CV_simp_avgsimp(i,7) = mean(rev_store(:, 7));%mean Net revenue 3 fishery
    mean_CV_simp_avgsimp(i,8) = var(rev_store(:, 7))/mean(rev_store(:, 7));% CV 3-fishery
    mean_CV_simp_avgsimp(i,9) = mean(rev_store(:,8));%mean HHI 3 fishery

    mean_CV_simp_avgsimp(i,10) = var(rev_store(:, 3))/mean(rev_store(:, 3));% CV (income) 1-fishery
    mean_CV_simp_avgsimp(i,11) = var(rev_store(:, 6))/mean(rev_store(:, 6));% CV (income) 2-fishery
    mean_CV_simp_avgsimp(i,12) = var(rev_store(:, 9))/mean(rev_store(:, 9));% CV (income) 3-fishery
end
combinedData = num2cell(mean_CV_simp_avgsimp);
combinedData = cell2table(combinedData);

combinedData = renamevars(combinedData, ["combinedData1","combinedData2","combinedData3","combinedData4","combinedData5","combinedData6","combinedData7","combinedData8","combinedData9","combinedData10","combinedData11","combinedData12"],...
    ["strat1_MeanRev","strat1_CV","strat1_MeanHHI","strat2_MeanRev","strat2_CV","strat2_MeanHHI","strat3_MeanRev","strat3_CV","strat3_MeanHHI","strat1_IncomeCV","strat2_IncomeCV","strat3_IncomeCV"]);
combinedData.CV_Diff1 = combinedData.strat1_CV - combinedData.strat2_CV;%specialist versus the semi-generalist
combinedData.CV_Diff2 = combinedData.strat2_CV - combinedData.strat3_CV;%semi-generalist versus the generalist
combinedData.CV_Diff3 = combinedData.strat1_CV - combinedData.strat3_CV;%specialist versus the generalist

gscatter(combinedData.strat3_MeanRev, combinedData.CV_Diff3);
xlabel("Mean Revenue when diversified") 
ylabel("CV change (CV_{specialized} - CV_{diversified})")
gscatter(combinedData.strat1_MeanRev, combinedData.CV_Diff3);
xlabel("Mean Revenue under specialization")
ylabel("CV change (CV_{specialized} - CV_{diversified})")
title("CV change (allowing boats to set their strategy in the first year")

mycolumn = cell(numIterations*3,1);
mycolumn(1:numIterations) = {'1Fishery'}; mycolumn(1+numIterations:2*numIterations) = {'2Fishery'};mycolumn(1+2*numIterations:3*numIterations) = {'3Fishery'}; 
temp = table(vertcat(combinedData.strat1_CV,combinedData.strat2_CV,combinedData.strat3_CV), mycolumn,...
    vertcat(combinedData.strat1_MeanHHI,combinedData.strat2_MeanHHI,combinedData.strat3_MeanHHI),...
    vertcat(combinedData.strat1_MeanRev,combinedData.strat2_MeanRev,combinedData.strat3_MeanRev),...
    vertcat(combinedData.strat1_IncomeCV,combinedData.strat2_IncomeCV,combinedData.strat3_IncomeCV),...
    'VariableNames',["CV", "Strategy", "HHI", "MeanRevenue", "IncomeCV"]);




gscatter(temp.MeanRevenue, temp.MeanRevenue.*temp.CV, temp.Strategy)
xlabel("Mean Revenue")
ylabel("Revenue Variance")
title("Var/Mean (Allowing boats to set their strategy in the first year")

gscatter(temp.HHI, temp.CV, temp.Strategy)
xlabel("HHI")
ylabel("CV")
title("Var/Mean (Allowing boats to set their strategy in the first year")
line([min(temp.HHI), max(temp.HHI)], [median(combinedData.strat1_CV), median(combinedData.strat1_CV)], 'Color', 'b', 'LineStyle', '--');

%%
combinedData.IncomeCV_diff = combinedData.strat1_IncomeCV - combinedData.strat3_IncomeCV;
%CV difference graph (income CV) 
gscatter(combinedData.strat1_MeanRev, combinedData.IncomeCV_diff);
xlabel("Mean Revenue under specialization")
ylabel("\Delta CV of income (CV_{specialized} - CV_{diversified})")
title("Income CV change (Chosen strategy)")

%HHI versus CV
gscatter(temp.HHI, temp.IncomeCV, temp.Strategy)
xlabel("HHI")
ylabel("Income CV")
title("Income CV / HHI (Chosen strategy)")
line([min(temp.HHI), max(temp.HHI)], [median(combinedData.strat1_IncomeCV), median(combinedData.strat1_IncomeCV)], 'Color', 'b', 'LineStyle', '--');
