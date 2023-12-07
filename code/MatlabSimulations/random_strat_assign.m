numIterations = 350;
numYears = 10;
% Initialize a cell array to store the results

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

% Define the strings you want to repeat
stringsToRepeat = {'A', 'B', 'C', 'BC', 'AC', 'AB', 'ABC'};
% Define the number of times you want to repeat the list
numRepeats = numIterations/7; % Change this value to adjust the number of repetitions
% Use the repelem function to repeat the cell array
repeatedList = repelem(stringsToRepeat, numRepeats);


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
    rev_store = zeros(numYears, 2);
    firstyear_strat = repeatedList(i);
    for j = 1:numYears
        %simulate for numYears and store the  rev_temp with a time-based shock
        a1_temp = a1 - 2 + 4 * rand;
        a2_temp = a2 - 2 + 4 * rand;
        a3_temp = a3 - 2 + 4 * rand;
        [rev_temp,s_temp] = oneYearChoice_fs(firstyear_strat,a1_temp,a2_temp,a3_temp,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,i,results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC);
        %store the revenue
        rev_store(j,1) = rev_temp(i,4);
        %store the simpson index
        rev_store(j,2) = rev_temp(i,5);
    end
    mean_CV_simp_avgsimp(i,1) = mean(rev_store(:, 1));
    mean_CV_simp_avgsimp(i,2) = var(rev_store(:, 1))/mean(rev_store(:, 1));
    mean_CV_simp_avgsimp(i,3) = rev_store(1,2);
    mean_CV_simp_avgsimp(i,4) = mean(rev_store(:,2));
    strategies(i) = {firstyear_strat};
end
combinedData = num2cell(mean_CV_simp_avgsimp);
combinedData(:,5) = strategies(:,1);
combinedData = cell2table(combinedData);



%results
%scatter(combinedData, "combinedData1", "combinedData3",'filled',"ColorVariable" , "combinedData5")
%scatter(combinedData, "combinedData1", "combinedData3",'filled')
%gscatter(combinedData.combinedData1,combinedData.combinedData4, combinedData.combinedData5)
gscatter(combinedData.combinedData1,combinedData.combinedData2, combinedData.combinedData5)
%gscatter(combinedData.combinedData1,combinedData.combinedData2, combinedData.combinedData5)
%scatter(mean_CV_simp_avgsimp(:,1), mean_CV_simp_avgsimp(:,3))
%%
function [results,strategy] = oneYearChoice_fs(fixed_strat,a1,a2,a3,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,i,results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC)
    strategy = fixed_strat;
    e1 = optimvar('e1', 'LowerBound', 0, 'UpperBound', 1);
    e2 = optimvar('e2', 'LowerBound', 0, 'UpperBound', 1);
    e3 = optimvar('e3', 'LowerBound', 0, 'UpperBound', 1);
    if fixed_strat == "ABC"
        %===============================
        %-------------ABC---------------
        %===============================
        prob_ABC = optimproblem('ObjectiveSense', 'maximize');
        prob_ABC.Constraints.cons1 = e1 + e2 + e3 <= 1;
        prob_ABC.Constraints.cons2 = (e1 >= 0);
        prob_ABC.Constraints.cons3 = (e2 >= 0);
        prob_ABC.Constraints.cons4 = (e3 >= 0);
        prob_ABC.Objective = (a1 * e1 - b1 * e1^c1) + (a2 * e2 - b2 * e2^c2) + (a3 * e3 - b3 * e3^c3);
        % Solve the problem and store the solution in the results cell array
        solution = solve(prob_ABC);
        results_ABC(i,1) = solution.e1;
        results_ABC(i,2) = solution.e2;
        results_ABC(i,3) = solution.e3;
        flag = ([(a1 * solution.e1 - b1 * solution.e1^c1) , (a2 * solution.e2 - b2 * solution.e2^c2) , (a3 * solution.e3 - b3 * solution.e3^c3)] < 0);
        %if (a1 * solution.e1 - b1 * solution.e1^c1) < 0
        if flag(1)
            prob_ABC.Constraints.cons2 = e1 == 0;
        end
        if flag(2)
            prob_ABC.Constraints.cons3 = e2 == 0;
        end
        if flag(3)
            prob_ABC.Constraints.cons4 = e3 == 0;
        end
        % Solve the problem and store the solution in the results cell array
        solution = solve(prob_ABC);
        results_ABC(i,1) = solution.e1;
        results_ABC(i,2) = solution.e2;
        results_ABC(i,3) = solution.e3;
        results_ABC(i,4) = evaluate(prob_ABC.Objective, solution);
        %store optimized objective function value
        results_ABC(i,4) = evaluate(prob_ABC.Objective, solution);
        results(i,1) = results_ABC(i,1);
        results(i,2) = results_ABC(i,2);
        results(i,3) = results_ABC(i,3);
        results(i,4) = results_ABC(i,4)+eps1+eps2+eps3-fc1-fc2-fc3;
    elseif fixed_strat == "A"
    %===============================
    %-------------A-----------------
    %===============================
        prob_A = optimproblem('ObjectiveSense', 'maximize');
        prob_A.Constraints.cons1 = e1 <= 1;
        prob_A.Constraints.cons2 = e1 >= 0;
        prob_A.Constraints.cons3 = e2 >= 0;
        prob_A.Constraints.cons4 = e3 >= 0;
        prob_A.Objective = (a1 * e1 - b1 * e1^c1 - fc1);
        % Solve the problem and store the solution in the results cell array
        solution = solve(prob_A);
        results_A(i,1) = solution.e1;
        results_A(i,2) = 0;
        results_A(i,3) = 0;
        %store optimized objective function value
        results_A(i,4) = evaluate(prob_A.Objective, solution);
        results(i,1) = results_A(i,1);
        results(i,2) = results_A(i,2);
        results(i,3) = results_A(i,3);
        results(i,4) = results_A(i,4)+eps1;
    elseif fixed_strat == "B"
        %===============================
        %-------------B-----------------
        %===============================
        prob_B = optimproblem('ObjectiveSense', 'maximize');
        prob_B.Constraints.cons1 = e2 <= 1;
        prob_B.Constraints.cons2 = e1 >= 0;
        prob_B.Constraints.cons3 = e2 >= 0;
        prob_B.Constraints.cons4 = e3 >= 0;
        prob_B.Objective = (a2 * e2 - b2 * e2^c2 - fc2);
        % Solve the problem and store the solution in the results cell array
        solution = solve(prob_B);
        results_B(i,1) = 0;
        results_B(i,2) = solution.e2;
        results_B(i,3) = 0;
        %store optimized objective function value
        results_B(i,4) = evaluate(prob_B.Objective, solution);
        results(i,1) = results_B(i,1);
        results(i,2) = results_B(i,2);
        results(i,3) = results_B(i,3);
        results(i,4) = results_B(i,4)+eps2;
    elseif fixed_strat == "C"
        %===============================
        %-------------C-----------------
        %===============================
        prob_C = optimproblem('ObjectiveSense', 'maximize');
        prob_C.Constraints.cons1 = e3 <= 1;
        prob_C.Constraints.cons2 = e1 >= 0;
        prob_C.Constraints.cons3 = e2 >= 0;
        prob_C.Constraints.cons4 = e3 >= 0;
        prob_C.Objective = (a3 * e3 - b3 * e3^c3 - fc3);
        % Solve the problem and store the solution in the results cell array
        solution = solve(prob_C);
        results_C(i,1) = 0;
        results_C(i,2) = 0;
        results_C(i,3) = solution.e3;
        %store optimized objective function value
        results_C(i,4) = evaluate(prob_C.Objective, solution);
        results(i,1) = results_C(i,1);
        results(i,2) = results_C(i,2);
        results(i,3) = results_C(i,3);
        results(i,4) = results_C(i,4)+eps3;
    elseif fixed_strat == "BC"
    %===============================
    %-------------BC-----------------
    %===============================
        prob_BC = optimproblem('ObjectiveSense', 'maximize');
        prob_BC.Constraints.cons1 = e2 + e3 <= 1;
        prob_BC.Constraints.cons2 = e1 >= 0;
        prob_BC.Constraints.cons3 = e2 >= 0;
        prob_BC.Constraints.cons4 = e3 >= 0;
        prob_BC.Objective = (a2 * e2 - b2 * e2^c2 - fc2) + (a3 * e3 - b3 * e3^c3 - fc3);
        % Solve the problem and store the solution in the results cell array
        solution = solve(prob_BC);
        results_BC(i,1) = 0;
        results_BC(i,2) = solution.e2;
        results_BC(i,3) = solution.e3;
        %store optimized objective function value
        results_BC(i,4) = evaluate(prob_BC.Objective, solution);
        results(i,1) = results_BC(i,1);
        results(i,2) = results_BC(i,2);
        results(i,3) = results_BC(i,3);
        results(i,4) = results_BC(i,4)+eps2+eps3;
    elseif fixed_strat == "AC"
        %===============================
        %-------------AC-----------------
        %===============================
        prob_AC = optimproblem('ObjectiveSense', 'maximize');
        prob_AC.Constraints.cons1 = e1 + e3 <= 1;
        prob_AC.Constraints.cons2 = e1 >= 0;
        prob_AC.Constraints.cons3 = e2 >= 0;
        prob_AC.Constraints.cons4 = e3 >= 0;
        prob_AC.Objective = (a1 * e1 - b1 * e1^c1 - fc1) + (a3 * e3 - b3 * e3^c3 - fc3);
        % Solve the problem and store the solution in the results cell array
        solution = solve(prob_AC);
        results_AC(i,1) = solution.e1;
        results_AC(i,2) = 0;
        results_AC(i,3) = solution.e3;
        %store optimized objective function value
        results_AC(i,4) = evaluate(prob_AC.Objective, solution);
        results(i,1) = results_AC(i,1);
        results(i,2) = results_AC(i,2);
        results(i,3) = results_AC(i,3);
        results(i,4) = results_AC(i,4)+eps1+eps3;
    elseif fixed_strat == "AB"
        %===============================
        %-------------AB-----------------
        %===============================
        prob_AB = optimproblem('ObjectiveSense', 'maximize');
        prob_AB.Constraints.cons1 = e1 + e2 <= 1;
        prob_AB.Constraints.cons2 = e1 >= 0;
        prob_AB.Constraints.cons3 = e2 >= 0;
        prob_AB.Constraints.cons4 = e3 >= 0;
        prob_AB.Objective = (a1 * e1 - b1 * e1^c1 - fc1) + (a2 * e2 - b2 * e2^c2 - fc2);
        % Solve the problem and store the solution in the results cell array
        solution = solve(prob_AB);
        results_AB(i,1) = solution.e1;
        results_AB(i,2) = solution.e2;
        results_AB(i,3) = 0;
        %store optimized objective function value
        results_AB(i,4) = evaluate(prob_AB.Objective, solution);
        results(i,1) = results_AB(i,1);
        results(i,2) = results_AB(i,2);
        results(i,3) = results_AB(i,3);
        results(i,4) = results_AB(i,4)+eps1+eps2;
    end
    data = [(a1 * results(i,1) - b1 * results(i,1)^c1),(a2 * results(i,2) - b2 * results(i,2)^c2),(a3 * results(i,3) - b3 * results(i,3)^c3)];
    %HHI of revenue concentration
    temp2 = (data/sum(data));
    results(i,5) = sqrt(sum(temp2.^2));
    if isnan(results(i,5))
        formatSpec = 'data1 is %8.8f , data2 is %8.8f , data3 is %8.8f';
        fprintf(formatSpec,data(1),data(2), data(3))
        formatSpec = 'e1 is %8.8f , e2 is %8.8f , e3 is %8.8f';
        fprintf(formatSpec,results_ABC(i,1),results_ABC(i,2), results_ABC(i,3))
    end
    results(i,6) = a1;
    results(i,7) = a2;
    results(i,8) = a3;
    results(i,9) = b1;
    results(i,10) = b2;
    results(i,11) = b3;
end