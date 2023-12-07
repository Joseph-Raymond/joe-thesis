numIterations = 330; %must choose a multiple of 3 here
numYears = 10;
% intialize the results matrices
results = zeros(numIterations, 11);
results_choice = zeros(numIterations, 11);
results_alt = zeros(numIterations, 11);
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
choice_results = zeros(1, 11);
alt_results = zeros(1, 11);
%%
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
    fc1 = 0.3 + 0.2*rand(n, 1);
    fc2 = 0.3 + 0.2*rand(n, 1);
    fc3 = 0.3 + 0.2*rand(n, 1);
    rev_store = zeros(numYears, 8);
    %reset the counters for years with diverse specialization
    noFish1 = 0;
    noFish2 = 0;
    %storing results from the four scenarios
    %specialist years
    for j = 1:numYears
        %simulate for numYears and store the  rev_temp with a shock each year
        a1_temp = a1 - 1 + 2 * rand; %adding a random variable ~ Unif[-1,1]
        a2_temp = a2 - 1 + 2 * rand;
        a3_temp = a3 - 1 + 2 * rand;
        [choice_results,choice_strat] = select_oneORthree_fishery(a1_temp,a2_temp,a3_temp,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,j,choice_results,results_A,results_B,results_C,results_ABC);
        %store the revenue
        if choice_results(j,1)<0
            disp(choice_results(j,1:3))
            disp(choice_strat)
        end
        rev_store(j,1) = choice_results(j,4);
        %store the hhi index
        rev_store(j,2) = choice_results(j,5);
        %store the income index
        rev_store(j,3) = choice_results(j,6);
        if choice_strat == "A" | choice_strat == "B" | choice_strat == "C"
            alternative = "ABC";
            diverse_index = 0;
            [alt_results,s_temp] = oneYearChoice_fs(alternative,a1_temp,a2_temp,a3_temp,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,j,alt_results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC);
            %store the revenue (alt)
            rev_store(j,4) = alt_results(j,4);
            %store the HHI index (alt)
            rev_store(j,5) = alt_results(j,5);
            %store the income index (alt)
            rev_store(j,6) = alt_results(j,6);
        elseif choice_strat == "ABC"
            %select which of the three specialist fisheries as "alt"
            [alt_results,alternative] = select_one_fishery(a1_temp,a2_temp,a3_temp,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,j,alt_results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC);
            diverse_index = 1;
            %get the numbr of fisheries that boat does not use
            numNofish = sum([choice_results(j,1),choice_results(j,2),choice_results(j,3)]==0);
            if numNofish==1
                noFish1 = noFish1+1;
            elseif numNofish==2
                noFish2 = noFish2+1;
            end
            %store the revenue (alt)
            rev_store(j,4) = alt_results(j,4);
            %store the HHI index (alt)
            rev_store(j,5) = alt_results(j,5);
            %store the income index (alt)
            rev_store(j,6) = alt_results(j,6);
        else
            msg = 'Error occurred';
            error(msg)
        end
    end
    results_choice((10*i)-9:10*i,1:11) = choice_results; %choice results
    results_alt((10*i)-9:10*i,1:11) = alt_results; %alt choice results

    %results_choice((10*i)-9:10*i,12) = diverse_index; %choice results add selection
    %results_alt((10*i)-9:10*i,12) = diverse_index; %alt choice results add selection

    mean_CV_simp_avgsimp(i,1) = mean(rev_store(:, 1));%mean net revenue fishery
    mean_CV_simp_avgsimp(i,2) = var(rev_store(:, 1))/mean(rev_store(:, 1));% CV fishery
    mean_CV_simp_avgsimp(i,3) = median(rev_store(:,2));%mean HHI fishery
    mean_CV_simp_avgsimp(i,4) = var(rev_store(:, 3))/mean(rev_store(:, 3));% CV (income) fishery

    mean_CV_simp_avgsimp(i,5) = mean(rev_store(:, 4));%mean net revenue fishery alt
    mean_CV_simp_avgsimp(i,6) = var(rev_store(:, 4))/mean(rev_store(:, 4));% CV fishery
    mean_CV_simp_avgsimp(i,7) = median(rev_store(:,5));%mean HHI fishery
    mean_CV_simp_avgsimp(i,8) = var(rev_store(:, 6))/mean(rev_store(:, 6));% CV (income) fishery

    mean_CV_simp_avgsimp(i,9) = diverse_index;%index for whether the boat was diversified (=1) or specialized to at the end (=0)
    mean_CV_simp_avgsimp(i,10) = noFish1;
    mean_CV_simp_avgsimp(i,11) = noFish2;
end
combinedData = num2cell(mean_CV_simp_avgsimp);
combinedData = cell2table(combinedData);
%combinedData = renamevars(combinedData, ["combinedData1","combinedData2","combinedData3","combinedData4","combinedData5","combinedData6","combinedData7","combinedData8","combinedData9","combinedData10","combinedData11"],...
%    ["choicestrat_MeanRev","choicestrat_CV","choicestrat_MedianHHI","choicestrat_IncomeCV","altstrat_MeanRev","altstrat_CV","altstrat_MedianHHI","altstrat_IncomeCV","diverse_index", "OneUnfished", "TwoUnfished"]);
oldVarNames = {'combinedData1', 'combinedData2', 'combinedData3', 'combinedData4', 'combinedData5', 'combinedData6', 'combinedData7', 'combinedData8', 'combinedData9', 'combinedData10', 'combinedData11'};
newVarNames = {'choicestrat_MeanRev', 'choicestrat_CV', 'choicestrat_MedianHHI', 'choicestrat_IncomeCV', 'altstrat_MeanRev', 'altstrat_CV', 'altstrat_MedianHHI', 'altstrat_IncomeCV', 'diverse_index', 'OneUnfished', 'TwoUnfished'};
combinedData.Properties.VariableNames = newVarNames;



treat_on_treated = zeros(1);
treat_on_nontreated = zeros(1);
nontreat_on_treated = zeros(1);
nontreat_on_nontreated = zeros(1);
treatment_means = table(treat_on_treated,treat_on_nontreated,nontreat_on_treated,nontreat_on_nontreated);
for i = 1:height(combinedData.choicestrat_IncomeCV)
    if combinedData.diverse_index(i)==1%chose to be a diversified; also means that the choice strat is diversified (3-fishery) and the altstrat is specialized (1-fishery)
        treatment_means.treat_on_treated(i) = combinedData.choicestrat_IncomeCV(i);
        treatment_means.nontreat_on_treated(i) = combinedData.altstrat_IncomeCV(i);
        treatment_means.treat_on_nontreated(i) = NaN;
        treatment_means.nontreat_on_nontreated(i) = NaN;
    elseif combinedData.diverse_index(i)==0%chose to be a specialist; also means that the choice strat is specialized (1-fishery) and the altstrat is diversified (3-fishery)
        treatment_means.treat_on_treated(i) = NaN;
        treatment_means.nontreat_on_treated(i) = NaN;
        treatment_means.treat_on_nontreated(i) = combinedData.altstrat_IncomeCV(i);
        treatment_means.nontreat_on_nontreated(i) = combinedData.choicestrat_IncomeCV(i);
    else
        msg = 'Error 2 occurred';
        error(msg)
    end    
end
effects = mean(treatment_means,1,"omitmissing")
df = [combinedData treatment_means];


namecolumn = cell(numIterations*4,1);
namecolumn(1:numIterations) = {'treat given treated'}; namecolumn(1+numIterations:2*numIterations) = {'treat given non-treated'};namecolumn(1+2*numIterations:3*numIterations) = {'non-treat given treated'};namecolumn(1+3*numIterations:4*numIterations) = {'non-treat given non-treated'}; 
plotdata = table(vertcat(df.treat_on_treated, df.treat_on_nontreated, df.nontreat_on_treated, df.nontreat_on_nontreated), namecolumn,...
    'VariableNames',["CV", "Average CV Group"]);

df = [combinedData];
scatter(df.choicestrat_MedianHHI, df.choicestrat_CV,20, df.choicestrat_MeanRev, 'filled')
xlabel("Median HHI")
ylabel("Income CV")
title("Income CV / Median HHI (treated given treated)")
cb = colorbar(); 
ylabel(cb,'Mean Annual Revenue (10yr)','FontSize',16,'Rotation',270)
%%
numIterations = 333; %must choose a multiple of 3 here
numYears = 10;
% intialize the results matrices
results = zeros(numIterations, 11);
results_choice = zeros(numIterations, 11);
results_alt = zeros(numIterations, 11);
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
choice_results = zeros(1, 11);
alt_results = zeros(1, 11);
%% Round two with HHI across all time
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
    fc1 = 0.3 + 0.2*rand(n, 1);
    fc2 = 0.3 + 0.2*rand(n, 1);
    fc3 = 0.3 + 0.2*rand(n, 1);
    rev_store = zeros(numYears, 8);
    %reset the counters for years with diverse specialization
    noFish1 = 0;
    noFish2 = 0;
    %storing results from the four scenarios
    %specialist years
    for j = 1:numYears
        %simulate for numYears and store the  rev_temp with a shock each year
        a1_temp = a1 - 1 + 2 * rand; %adding a random variable ~ Unif[-1,1]
        a2_temp = a2 - 1 + 2 * rand;
        a3_temp = a3 - 1 + 2 * rand;
        [choice_results,choice_strat] = select_oneORthree_fishery(a1_temp,a2_temp,a3_temp,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,j,choice_results,results_A,results_B,results_C,results_ABC);
        rev_store(j,1) = choice_results(j,4);
        %store the hhi index
        rev_store(j,2) = choice_results(j,5);
        %store the income index
        rev_store(j,3) = choice_results(j,6);
        if choice_strat == "A" | choice_strat == "B" | choice_strat == "C"
            alternative = "ABC";
            diverse_index = 0;
            [alt_results,s_temp] = oneYearChoice_fs(alternative,a1_temp,a2_temp,a3_temp,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,j,alt_results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC);
            %store the revenue (alt)
            rev_store(j,4) = alt_results(j,4);
            %store the HHI index (alt)
            rev_store(j,5) = alt_results(j,5);
            %store the income index (alt)
            rev_store(j,6) = alt_results(j,6);
        elseif choice_strat == "ABC"
            %select which of the three specialist fisheries as "alt"
            [alt_results,alternative] = select_one_fishery(a1_temp,a2_temp,a3_temp,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,j,alt_results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC);
            diverse_index = 1;
            %store the revenue (alt)
            rev_store(j,4) = alt_results(j,4);
            %store the HHI index (alt)
            rev_store(j,5) = alt_results(j,5);
            %store the income index (alt)
            rev_store(j,6) = alt_results(j,6);
        else
            msg = 'Error occurred';
            error(msg)
        end
    end
    results_choice((10*i)-9:10*i,1:11) = choice_results; %choice results
    results_alt((10*i)-9:10*i,1:11) = alt_results; %alt choice results
    
    hhi_temp = sum(results_choice((10*i)-9:10*i,9:11),1);%sum each column; left with row vector
    hhi_temp = (hhi_temp/sum(hhi_temp));

    mean_CV_simp_avgsimp(i,1) = mean(rev_store(:, 1));%mean net revenue fishery
    mean_CV_simp_avgsimp(i,2) = var(rev_store(:, 1))/mean(rev_store(:, 1));% CV fishery
    mean_CV_simp_avgsimp(i,3) = sqrt(sum(hhi_temp.^2));%10 yr HHI fishery
    mean_CV_simp_avgsimp(i,4) = var(rev_store(:, 3))/mean(rev_store(:, 3));% CV (income) fishery
    
    hhi_temp = sum(results_alt((10*i)-9:10*i,9:11),1);%sum each column; left with row vector
    hhi_temp = (hhi_temp/sum(hhi_temp));
    
    mean_CV_simp_avgsimp(i,5) = mean(rev_store(:, 4));%mean net revenue fishery alt
    mean_CV_simp_avgsimp(i,6) = var(rev_store(:, 4))/mean(rev_store(:, 4));% CV fishery alt
    mean_CV_simp_avgsimp(i,7) = sqrt(sum(hhi_temp.^2));%10yr HHI fishery alt
    mean_CV_simp_avgsimp(i,8) = var(rev_store(:, 6))/mean(rev_store(:, 6));% CV (income) fishery

    mean_CV_simp_avgsimp(i,9) = diverse_index;%index for whether the boat was diversified (=1) or specialized to at the end (=0)
    mean_CV_simp_avgsimp(i,10) = noFish1;
    mean_CV_simp_avgsimp(i,11) = noFish2;
end
combinedData = num2cell(mean_CV_simp_avgsimp);
combinedData = cell2table(combinedData);
%combinedData = renamevars(combinedData, ["combinedData1","combinedData2","combinedData3","combinedData4","combinedData5","combinedData6","combinedData7","combinedData8","combinedData9","combinedData10","combinedData11"],...
%    ["choicestrat_MeanRev","choicestrat_CV","choicestrat_MedianHHI","choicestrat_IncomeCV","altstrat_MeanRev","altstrat_CV","altstrat_MedianHHI","altstrat_IncomeCV","diverse_index", "OneUnfished", "TwoUnfished"]);
oldVarNames = {'combinedData1', 'combinedData2', 'combinedData3', 'combinedData4', 'combinedData5', 'combinedData6', 'combinedData7', 'combinedData8', 'combinedData9', 'combinedData10', 'combinedData11'};
newVarNames = {'choicestrat_MeanRev', 'choicestrat_CV', 'choicestrat_MedianHHI', 'choicestrat_IncomeCV', 'altstrat_MeanRev', 'altstrat_CV', 'altstrat_MedianHHI', 'altstrat_IncomeCV', 'diverse_index', 'OneUnfished', 'TwoUnfished'};
combinedData.Properties.VariableNames = newVarNames;
%%
scatter(results_choice(:,5),results_choice(:,6)-results_alt(:,6),20, results_choice(:,6), 'filled')
xlabel("HHI (1 year)")
ylabel("Income Difference from Alt strategy")
title("Income diff (choice - alt) / HHI [All Boat-Years]")
cb = colorbar(); 
ylabel(cb,'Income (not counting Fixed Costs)','FontSize',14,'Rotation',270)

rc_treat = results_choice(results_choice(:,5)<1,:);
rc_untreat = results_choice(results_choice(:,5)==1,:);
ac_treat = results_alt(results_choice(:,5)<1,:);
ac_untreat = results_alt(results_choice(:,5)==1,:);

scatter(rc_treat(:,5),rc_treat(:,6)-ac_treat(:,6),20, rc_treat(:,6), 'filled')
xlabel("HHI (1 year)")
ylabel("Income Difference from Alt strategy")
title("Income diff (choice - alt) / HHI [Treated/Diversified]")
cb = colorbar(); 
ylabel(cb,'Income (not counting Fixed Costs)','FontSize',14,'Rotation',270)

scatter(rc_untreat(:,5),rc_untreat(:,6)-ac_untreat(:,6),20, rc_untreat(:,6), 'filled')
xlabel("HHI (1 year)")
ylabel("Income Difference from Alt strategy")
title("Income diff (choice - alt) / HHI [Untreated/Specialized]")
cb = colorbar(); 
ylabel(cb,'Income (not counting Fixed Costs)','FontSize',14,'Rotation',270)

%%
results_choice(:,13) = 1;%ex-post diversified
results_choice((results_choice(:,1)<0.05 & results_choice(:,2)<0.05),13)=0;
results_choice((results_choice(:,1)<0.05 & results_choice(:,3)<0.05),13)=0;
results_choice((results_choice(:,2)<0.05 & results_choice(:,3)<0.05),13)=0;

mean(results_choice(results_choice(:,12)==1,6))%col 6 is revenue
mean(results_choice(results_choice(:,13)==1,6))%col 6 is revenue
%%
%HHI versus CV 
figure
hold on
fdf = df(df.diverse_index == 1, :);
scatter(fdf.choicestrat_MedianHHI, fdf.choicestrat_CV,20, fdf.choicestrat_MeanRev, 'filled')
xlabel("Median HHI")
ylabel("Income CV")
title("Income CV / Median HHI (treated given treated)")
cb = colorbar(); 
ylabel(cb,'Mean Annual Revenue (10yr)','FontSize',16,'Rotation',270)
clear fdf

fdf = df(df.diverse_index == 0, :);
figure
scatter(fdf.choicestrat_MedianHHI, fdf.choicestrat_CV,20, fdf.choicestrat_MeanRev, 'filled')
xlabel("Median HHI")
ylabel("Income CV")
title("Income CV / Median HHI (untreated given untreated)")
cb = colorbar(); 
ylabel(cb,'Mean Annual Revenue (10yr)','FontSize',16,'Rotation',270)
clear fdf

fdf = df(df.diverse_index == 1, :);
figure
scatter(fdf.altstrat_MedianHHI, fdf.altstrat_CV,20, fdf.altstrat_MeanRev, 'filled')
xlabel("Median HHI")
ylabel("Income CV")
title("Income CV / Median HHI (untreated given treated)")
cb = colorbar(); 
ylabel(cb,'Mean Annual Revenue (10yr)','FontSize',16,'Rotation',270)
clear fdf

fdf = df(df.diverse_index == 0, :);
figure
scatter(fdf.altstrat_MedianHHI, fdf.altstrat_CV,20, fdf.altstrat_MeanRev, 'filled')
xlabel("Median HHI")
ylabel("Income CV")
title("Income CV / Median HHI (treated given untreated)")
cb = colorbar(); 
ylabel(cb,'Mean Annual Revenue (10yr)','FontSize',16,'Rotation',270)
clear fdf
hold off


%%
figure
hold on
histogram(plotdata.CV(plotdata.("Average CV Group")=="treat given treated"),'FaceColor',"#0072BD")
xlim([0,0.70])
ylim([0,60])
lgd = legend({'treat given treated'});
fontsize(lgd,14,'points')
hold off

figure
hold on
histogram(plotdata.CV(plotdata.("Average CV Group")=="treat given non-treated"),'FaceColor',"#D95319")
xlim([0,0.70])
ylim([0,60])
lgd = legend({'treat given non-treated'});
fontsize(lgd,14,'points')
hold off

figure
hold on
histogram(plotdata.CV(plotdata.("Average CV Group")=="non-treat given treated"),'FaceColor',"#EDB120")
xlim([0,0.70])
ylim([0,60])
lgd = legend({'non-treat given treated'});
fontsize(lgd,14,'points')
hold off

figure
hold on
histogram(plotdata.CV(plotdata.("Average CV Group")=="non-treat given non-treated"),'FaceColor',"#7E2F8E")
xlim([0,0.70])
ylim([0,60])
lgd = legend({'non-treat given non-treated'});
fontsize(lgd,14,'points')
hold off




