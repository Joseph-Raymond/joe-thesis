numIterations = 333; %must choose a multiple of 3 here
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
    P = [0.1, 0.1, 0.1, 0.3];%[P_a, P_b,P_c,P_abc]
    b1 = 5 * rand(n, 1);
    b2 = 5 * rand(n, 1);
    b3 = 5 * rand(n, 1);
    fc1 = 0.3 + 0.2*rand(n, 1);
    fc2 = 0.3 + 0.2*rand(n, 1);
    fc3 = 0.3 + 0.2*rand(n, 1);
    rev_store = zeros(numYears, 6);
    %get the overall best strat from A,B,C,ABC. Don't need results from
    %this yet
    [~,choice_strat] = select_oneORthree_fishery(a1,a2,a3,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,(fc1+P(1)),(fc2+P(2)),(fc3+P(3)),i,results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC);
    [~,firstyear_strat1] = select_one_fishery(a1,a2,a3,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,(fc1+P(1)),(fc2+P(2)),(fc3+P(3)),i,results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC);
    %specialist years
    if choice_strat == "A" | choice_strat == "B" | choice_strat == "C"
        alternative = "ABC";
        diverse_index = 0;
        for j = 1:numYears
        %simulate for numYears and store the  rev_temp with a shock each year
        a1_temp = a1 - 2 + 4 * rand; %adding a random variable ~ Unif[-2,2]
        a2_temp = a2 - 2 + 4 * rand;
        a3_temp = a3 - 2 + 4 * rand;
        [rev_temp,~] = oneYearChoice_fs(choice_strat,a1_temp,a2_temp,a3_temp,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,P(1),P(2),P(3),i,results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC);
        %store the revenue
        rev_store(j,1) = rev_temp(i,4);
        %store the simpson index
        rev_store(j,2) = rev_temp(i,5);
        %store the income index
        rev_store(j,3) = rev_temp(i,6);

        [rev_temp_alt,~] = oneYearChoice_fs(alternative,a1_temp,a2_temp,a3_temp,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,P(1),P(2),P(3),i,results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC);
        %store the revenue
        rev_store(j,4) = rev_temp_alt(i,4);
        %store the HHI index
        rev_store(j,5) = rev_temp_alt(i,5);
        %store the income index
        rev_store(j,6) = rev_temp_alt(i,6);
        end
    elseif choice_strat == "ABC"
        alternative = firstyear_strat1;
        diverse_index = 1;
        for j = 1:numYears
        %simulate for numYears and store the  rev_temp with a shock each year
        a1_temp = a1 - 2 + 4 * rand; %adding a random variable ~ Unif[-2,2]
        a2_temp = a2 - 2 + 4 * rand;
        a3_temp = a3 - 2 + 4 * rand;
        [rev_temp,~] = oneYearChoice_fs(choice_strat,a1_temp,a2_temp,a3_temp,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,i,results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC);
        %store the revenue
        rev_store(j,1) = rev_temp(i,4);
        %store the simpson index
        rev_store(j,2) = rev_temp(i,5);
        %store the income index
        rev_store(j,3) = rev_temp(i,6);

        [rev_temp_alt,s_temp] = oneYearChoice_fs(alternative,a1_temp,a2_temp,a3_temp,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,i,results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC);
        %store the revenue
        rev_store(j,4) = rev_temp_alt(i,4);
        %store the HHI index
        rev_store(j,5) = rev_temp_alt(i,5);
        %store the income index
        rev_store(j,6) = rev_temp_alt(i,6);
        end
    else
        msg = 'Error occurred';
        error(msg)
    end
    
    
    mean_CV_simp_avgsimp(i,1) = mean(rev_store(:, 1));%mean net revenue fishery
    mean_CV_simp_avgsimp(i,2) = var(rev_store(:, 1))/mean(rev_store(:, 1));% CV fishery
    mean_CV_simp_avgsimp(i,3) = mean(rev_store(:,2));%mean HHI fishery
    mean_CV_simp_avgsimp(i,4) = var(rev_store(:, 3))/mean(rev_store(:, 3));% CV (income) fishery

    mean_CV_simp_avgsimp(i,5) = mean(rev_store(:, 4));%mean net revenue fishery alt
    mean_CV_simp_avgsimp(i,6) = var(rev_store(:, 4))/mean(rev_store(:, 4));% CV fishery
    mean_CV_simp_avgsimp(i,7) = mean(rev_store(:,5));%mean HHI fishery
    mean_CV_simp_avgsimp(i,8) = var(rev_store(:, 6))/mean(rev_store(:, 6));% CV (income) fishery

    mean_CV_simp_avgsimp(i,9) = diverse_index;%mean HHI 3 fishery
end
combinedData = num2cell(mean_CV_simp_avgsimp);
combinedData = cell2table(combinedData);

combinedData = renamevars(combinedData, ["combinedData1","combinedData2","combinedData3","combinedData4","combinedData5","combinedData6","combinedData7","combinedData8","combinedData9"],...
    ["choicestrat_MeanRev","choicestrat_CV","choicestrat_MeanHHI","choicestrat_IncomeCV","altstrat_MeanRev","altstrat_CV","altstrat_MeanHHI","altstrat_IncomeCV","diverse_index"]);

treat_on_treated = zeros(1);
treat_on_nontreated = zeros(1);
nontreat_on_treated = zeros(1);
nontreat_on_nontreated = zeros(1);
treatment_means = table(treat_on_treated,treat_on_nontreated,nontreat_on_treated,nontreat_on_nontreated);
for i = 1:length(combinedData.choicestrat_IncomeCV)
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

figure
hold on
histogram(plotdata.CV(plotdata.("Average CV Group")=="treat given treated"),'FaceColor',"#0072BD")
xlim([0,0.70])
ylim([0,60])
lgd = legend({'treat given treated'})
fontsize(lgd,14,'points')
hold off

figure
hold on
histogram(plotdata.CV(plotdata.("Average CV Group")=="treat given non-treated"),'FaceColor',"#D95319")
xlim([0,0.70])
ylim([0,60])
lgd = legend({'treat given non-treated'})
fontsize(lgd,14,'points')
hold off

figure
hold on
histogram(plotdata.CV(plotdata.("Average CV Group")=="non-treat given treated"),'FaceColor',"#EDB120")
xlim([0,0.70])
ylim([0,60])
lgd = legend({'non-treat given treated'})
fontsize(lgd,14,'points')
hold off

figure
hold on
histogram(plotdata.CV(plotdata.("Average CV Group")=="non-treat given non-treated"),'FaceColor',"#7E2F8E")
xlim([0,0.70])
ylim([0,60])
lgd = legend({'non-treat given non-treated'})
fontsize(lgd,14,'points')
hold off



%figure
%hold on
%histogram(plotdata.CV(plotdata.("Average CV Group")=="treat given treated"))
%histogram(plotdata.CV(plotdata.("Average CV Group")=="treat given non-treated"))
%histogram(plotdata.CV(plotdata.("Average CV Group")=="non-treat given treated"))
%histogram(plotdata.CV(plotdata.("Average CV Group")=="non-treat given non-treated"))
%legend({'treat given treated','treat given non-treated','non-treat given treated','non-treat given non-treated'})
%hold off



