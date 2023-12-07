numIterations = 100;
numYears = 10;
% Initialize a cell array to store the results

% intialize the results matrices
results = zeros(numIterations, 14);
results_A = zeros(numIterations, 4);
results_B = zeros(numIterations, 4);
results_C = zeros(numIterations, 4);
results_AB = zeros(numIterations, 4);
results_AC = zeros(numIterations, 4);
results_BC = zeros(numIterations, 4);
results_ABC = zeros(numIterations, 4);
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
    
    e1 = optimvar('e1', 'LowerBound', 0, 'UpperBound', 1);
    e2 = optimvar('e2', 'LowerBound', 0, 'UpperBound', 1);
    e3 = optimvar('e3', 'LowerBound', 0, 'UpperBound', 1);
    
    fc1 = 0.2 + 0.1*rand(n, 1);
    fc2 = 0.2 + 0.1*rand(n, 1);
    fc3 = 0.2 + 0.1*rand(n, 1);

    prob_ABC = optimproblem('ObjectiveSense', 'maximize');
    prob_ABC.Constraints.cons1 = e1 + e2 + e3 <= 1;
    prob_ABC.Objective = (a1 * e1 - b1 * e1^c1 - fc1) + (a2 * e2 - b2 * e2^c2 - fc2) + (a3 * e3 - b3 * e3^c3 - fc3);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_ABC);
    results_ABC(i,1) = solution.e1;
    results_ABC(i,2) = solution.e2;
    results_ABC(i,3) = solution.e3;
    %store optimized objective function value
    results_ABC(i,4) = evaluate(prob_ABC.Objective, solution);
    results(i,1:4) = results_ABC(i,1:4)+eps1+eps2+eps3;
    temp = results_ABC(i,1:4);%temp is max expected revenue from any strategy
    results(i,12) = "ABC";

    %===============================
    %-------------A-----------------
    %===============================

    prob_A = optimproblem('ObjectiveSense', 'maximize');
    prob_A.Constraints.cons1 = e1 <= 1;
    prob_A.Objective = (a1 * e1 - b1 * e1^c1 - fc1);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_A);
    results_A(i,1) = solution.e1;
    results_A(i,2) = 0;
    results_A(i,3) = 0;
    %store optimized objective function value
    results_A(i,4) = evaluate(prob_A.Objective, solution);
    if results_A(i,4) > temp
        temp = results_A(i,4);
        results(i,1) = results_A(i,1);
        results(i,2) = results_A(i,2);
        results(i,3) = results_A(i,3);
        results(i,4) = results_A(i,4)+eps1;
        results(i,12) = "A";
    end
    %===============================
    %-------------B-----------------
    %===============================
    prob_B = optimproblem('ObjectiveSense', 'maximize');
    prob_B.Constraints.cons1 = e2 <= 1;
    prob_B.Objective = (a2 * e2 - b2 * e2^c2 - fc2);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_B);
    results_B(i,1) = 0;
    results_B(i,2) = solution.e2;
    results_B(i,3) = 0;
    %store optimized objective function value
    results_B(i,4) = evaluate(prob_B.Objective, solution);
    if results_B(i,4)> temp
        temp = results_B(i,4);
        results(i,1) = results_B(i,1);
        results(i,2) = results_B(i,2);
        results(i,3) = results_B(i,3);
        results(i,4) = results_B(i,4)+eps2;
        results(i,12) = "B";
    end
    %===============================
    %-------------C-----------------
    %===============================
    prob_C = optimproblem('ObjectiveSense', 'maximize');
    prob_C.Constraints.cons1 = e3 <= 1;
    prob_C.Objective = (a3 * e3 - b3 * e3^c3 - fc3);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_C);
    results_C(i,1) = 0;
    results_C(i,2) = 0;
    results_C(i,3) = solution.e3;
    %store optimized objective function value
    results_C(i,4) = evaluate(prob_C.Objective, solution);
    if results_C(i,4)> temp
        temp = results_C(i,4);
        results(i,1) = results_C(i,1);
        results(i,2) = results_C(i,2);
        results(i,3) = results_C(i,3);
        results(i,4) = results_C(i,4)+eps3;
        results(i,12) = "C";
    end
    %===============================
    %-------------BC-----------------
    %===============================
    prob_BC = optimproblem('ObjectiveSense', 'maximize');
    prob_BC.Constraints.cons1 = e2 + e3 <= 1;
    prob_BC.Objective = (a2 * e2 - b2 * e2^c2 - fc2) + (a3 * e3 - b3 * e3^c3 - fc3);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_BC);
    results_BC(i,1) = 0;
    results_BC(i,2) = solution.e2;
    results_BC(i,3) = solution.e3;
    %store optimized objective function value
    results_BC(i,4) = evaluate(prob_BC.Objective, solution);
    if results_BC(i,4)> temp
        temp = results_BC(i,4);
        results(i,1) = results_BC(i,1);
        results(i,2) = results_BC(i,2);
        results(i,3) = results_BC(i,3);
        results(i,4) = results_BC(i,4)+eps2+eps3;
        results(i,12) = "BC";
    end
    %===============================
    %-------------AC-----------------
    %===============================
    prob_AC = optimproblem('ObjectiveSense', 'maximize');
    prob_AC.Constraints.cons1 = e1 + e3 <= 1;
    prob_AC.Objective = (a1 * e1 - b1 * e1^c1 - fc1) + (a3 * e3 - b3 * e3^c3 - fc3);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_AC);
    results_AC(i,1) = solution.e1;
    results_AC(i,2) = 0;
    results_AC(i,3) = solution.e3;
    %store optimized objective function value
    results_AC(i,4) = evaluate(prob_AC.Objective, solution);
    if results_AC(i,4)> temp
        temp = results_AC(i,4);
        results(i,1) = results_AC(i,1);
        results(i,2) = results_AC(i,2);
        results(i,3) = results_AC(i,3);
        results(i,4) = results_AC(i,4)+eps1+eps3;
        results(i,12) = "AC";
    end
    %===============================
    %-------------AB-----------------
    %===============================
    prob_AB = optimproblem('ObjectiveSense', 'maximize');
    prob_AB.Constraints.cons1 = e1 + e2 <= 1;
    prob_AB.Objective = (a1 * e1 - b1 * e1^c1 - fc1) + (a2 * e2 - b2 * e2^c2 - fc2);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_AB);
    results_AB(i,1) = solution.e1;
    results_AB(i,2) = solution.e2;
    results_AB(i,3) = 0;
    %store optimized objective function value
    results_AB(i,4) = evaluate(prob_AB.Objective, solution);
    if results_AB(i,4)> temp
        temp = results_AB(i,4);
        results(i,1) = results_AB(i,1);
        results(i,2) = results_AB(i,2);
        results(i,3) = results_AB(i,3);
        results(i,4) = results_AB(i,4)+eps1+eps2;
        results(i,12) = "AB";
    end
    data = [(a1 * results(i,1) - b1 * results(i,1)^c1),(a2 * results(i,2) - b2 * results(i,2)^c2),(a3 * results(i,3) - b3 * results(i,3)^c3)];
    %HHI of revenue concentration
    results(i,5) = sqrt(sum((data./sum(data)).^2));
   
    results(i,6) = a1;
    results(i,7) = a2;
    results(i,8) = a3;
    results(i,9) = b1;
    results(i,10) = b2;
    results(i,11) = b3;
    %[M,I] = max(data); %leftover code but might use this info about the
    %max() function later
    %results(i,12) = I;
    %now simulate this fishermen repeating their decision each year with
    %productivity shocks
    for i = 1:numYears

    end
end


%results = cell2mat(results_ABC);
mean_effort = mean(results(:, 1:3), 'all');
mean_income = mean(results(:, 4), 'all');

%HHI of revenue concentration
hhi_index = sqrt(sum(results(:, 1:3).^2,2));


function results = oneYearChoice(a1,a2,a3,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,i,results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC)
    e1 = optimvar('e1', 'LowerBound', 0, 'UpperBound', 1);
    e2 = optimvar('e2', 'LowerBound', 0, 'UpperBound', 1);
    e3 = optimvar('e3', 'LowerBound', 0, 'UpperBound', 1);
    prob_ABC = optimproblem('ObjectiveSense', 'maximize');
    prob_ABC.Constraints.cons1 = e1 + e2 + e3 <= 1;
    prob_ABC.Objective = (a1 * e1 - b1 * e1^c1 - fc1) + (a2 * e2 - b2 * e2^c2 - fc2) + (a3 * e3 - b3 * e3^c3 - fc3);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_ABC);
    results_ABC(i,1) = solution.e1;
    results_ABC(i,2) = solution.e2;
    results_ABC(i,3) = solution.e3;
    %store optimized objective function value
    results_ABC(i,4) = evaluate(prob_ABC.Objective, solution);
    results(i,1:4) = results_ABC(i,1:4)+eps1+eps2+eps3;
    temp = results_ABC(i,1:4);%temp is max expected revenue from any strategy
    results(i,12) = "ABC";

    %===============================
    %-------------A-----------------
    %===============================

    prob_A = optimproblem('ObjectiveSense', 'maximize');
    prob_A.Constraints.cons1 = e1 <= 1;
    prob_A.Objective = (a1 * e1 - b1 * e1^c1 - fc1);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_A);
    results_A(i,1) = solution.e1;
    results_A(i,2) = 0;
    results_A(i,3) = 0;
    %store optimized objective function value
    results_A(i,4) = evaluate(prob_A.Objective, solution);
    if results_A(i,4) > temp
        temp = results_A(i,4);
        results(i,1) = results_A(i,1);
        results(i,2) = results_A(i,2);
        results(i,3) = results_A(i,3);
        results(i,4) = results_A(i,4)+eps1;
        results(i,12) = "A";
    end
    %===============================
    %-------------B-----------------
    %===============================
    prob_B = optimproblem('ObjectiveSense', 'maximize');
    prob_B.Constraints.cons1 = e2 <= 1;
    prob_B.Objective = (a2 * e2 - b2 * e2^c2 - fc2);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_B);
    results_B(i,1) = 0;
    results_B(i,2) = solution.e2;
    results_B(i,3) = 0;
    %store optimized objective function value
    results_B(i,4) = evaluate(prob_B.Objective, solution);
    if results_B(i,4)> temp
        temp = results_B(i,4);
        results(i,1) = results_B(i,1);
        results(i,2) = results_B(i,2);
        results(i,3) = results_B(i,3);
        results(i,4) = results_B(i,4)+eps2;
        results(i,12) = "B";
    end
    %===============================
    %-------------C-----------------
    %===============================
    prob_C = optimproblem('ObjectiveSense', 'maximize');
    prob_C.Constraints.cons1 = e3 <= 1;
    prob_C.Objective = (a3 * e3 - b3 * e3^c3 - fc3);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_C);
    results_C(i,1) = 0;
    results_C(i,2) = 0;
    results_C(i,3) = solution.e3;
    %store optimized objective function value
    results_C(i,4) = evaluate(prob_C.Objective, solution);
    if results_C(i,4)> temp
        temp = results_C(i,4);
        results(i,1) = results_C(i,1);
        results(i,2) = results_C(i,2);
        results(i,3) = results_C(i,3);
        results(i,4) = results_C(i,4)+eps3;
        results(i,12) = "C";
    end
    %===============================
    %-------------BC-----------------
    %===============================
    prob_BC = optimproblem('ObjectiveSense', 'maximize');
    prob_BC.Constraints.cons1 = e2 + e3 <= 1;
    prob_BC.Objective = (a2 * e2 - b2 * e2^c2 - fc2) + (a3 * e3 - b3 * e3^c3 - fc3);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_BC);
    results_BC(i,1) = 0;
    results_BC(i,2) = solution.e2;
    results_BC(i,3) = solution.e3;
    %store optimized objective function value
    results_BC(i,4) = evaluate(prob_BC.Objective, solution);
    if results_BC(i,4)> temp
        temp = results_BC(i,4);
        results(i,1) = results_BC(i,1);
        results(i,2) = results_BC(i,2);
        results(i,3) = results_BC(i,3);
        results(i,4) = results_BC(i,4)+eps2+eps3;
        results(i,12) = "BC";
    end
    %===============================
    %-------------AC-----------------
    %===============================
    prob_AC = optimproblem('ObjectiveSense', 'maximize');
    prob_AC.Constraints.cons1 = e1 + e3 <= 1;
    prob_AC.Objective = (a1 * e1 - b1 * e1^c1 - fc1) + (a3 * e3 - b3 * e3^c3 - fc3);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_AC);
    results_AC(i,1) = solution.e1;
    results_AC(i,2) = 0;
    results_AC(i,3) = solution.e3;
    %store optimized objective function value
    results_AC(i,4) = evaluate(prob_AC.Objective, solution);
    if results_AC(i,4)> temp
        temp = results_AC(i,4);
        results(i,1) = results_AC(i,1);
        results(i,2) = results_AC(i,2);
        results(i,3) = results_AC(i,3);
        results(i,4) = results_AC(i,4)+eps1+eps3;
        results(i,12) = "AC";
    end
    %===============================
    %-------------AB-----------------
    %===============================
    prob_AB = optimproblem('ObjectiveSense', 'maximize');
    prob_AB.Constraints.cons1 = e1 + e2 <= 1;
    prob_AB.Objective = (a1 * e1 - b1 * e1^c1 - fc1) + (a2 * e2 - b2 * e2^c2 - fc2);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_AB);
    results_AB(i,1) = solution.e1;
    results_AB(i,2) = solution.e2;
    results_AB(i,3) = 0;
    %store optimized objective function value
    results_AB(i,4) = evaluate(prob_AB.Objective, solution);
    if results_AB(i,4)> temp
        temp = results_AB(i,4);
        results(i,1) = results_AB(i,1);
        results(i,2) = results_AB(i,2);
        results(i,3) = results_AB(i,3);
        results(i,4) = results_AB(i,4)+eps1+eps2;
        results(i,12) = "AB";
    end
    data = [(a1 * results(i,1) - b1 * results(i,1)^c1),(a2 * results(i,2) - b2 * results(i,2)^c2),(a3 * results(i,3) - b3 * results(i,3)^c3)];
    %HHI of revenue concentration
    results(i,5) = sqrt(sum((data./sum(data)).^2));
   
    results(i,6) = a1;
    results(i,7) = a2;
    results(i,8) = a3;
    results(i,9) = b1;
    results(i,10) = b2;
    results(i,11) = b3;
end

%%
numIterations = 10;
numYears = 10;
% Initialize a cell array to store the results

% intialize the results matrices
results = zeros(numIterations, 14);
results_A = zeros(numIterations, 4);
results_B = zeros(numIterations, 4);
results_C = zeros(numIterations, 4);
results_AB = zeros(numIterations, 4);
results_AC = zeros(numIterations, 4);
results_BC = zeros(numIterations, 4);
results_ABC = zeros(numIterations, 4);
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
    results = oneYearChoice(a1,a2,a3,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,i,results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC)
end

%%
%results = cell2mat(results_ABC);
mean_effort = mean(results(:, 1:3), 'all');
mean_income = mean(results(:, 4), 'all');

%HHI of revenue concentration
hhi_index = sqrt(sum(results(:, 1:3).^2,2));





%% 
scatter(hhi_index(:,1),results(:, 4));
%%
scatter(results(:, 5),results(:, 4));

function results = oneYearChoice(a1,a2,a3,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,i,results,results_A,results_B,results_C,results_AB,results_AC,results_BC,results_ABC)
    e1 = optimvar('e1', 'LowerBound', 0, 'UpperBound', 1);
    e2 = optimvar('e2', 'LowerBound', 0, 'UpperBound', 1);
    e3 = optimvar('e3', 'LowerBound', 0, 'UpperBound', 1);
    prob_ABC = optimproblem('ObjectiveSense', 'maximize');
    prob_ABC.Constraints.cons1 = e1 + e2 + e3 <= 1;
    prob_ABC.Objective = (a1 * e1 - b1 * e1^c1 - fc1) + (a2 * e2 - b2 * e2^c2 - fc2) + (a3 * e3 - b3 * e3^c3 - fc3);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_ABC);
    results_ABC(i,1) = solution.e1;
    results_ABC(i,2) = solution.e2;
    results_ABC(i,3) = solution.e3;
    %store optimized objective function value
    results_ABC(i,4) = evaluate(prob_ABC.Objective, solution);
    results(i,1:4) = results_ABC(i,1:4)+eps1+eps2+eps3;
    temp = results_ABC(i,1:4);%temp is max expected revenue from any strategy
    results(i,12) = "ABC";

    %===============================
    %-------------A-----------------
    %===============================

    prob_A = optimproblem('ObjectiveSense', 'maximize');
    prob_A.Constraints.cons1 = e1 <= 1;
    prob_A.Objective = (a1 * e1 - b1 * e1^c1 - fc1);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_A);
    results_A(i,1) = solution.e1;
    results_A(i,2) = 0;
    results_A(i,3) = 0;
    %store optimized objective function value
    results_A(i,4) = evaluate(prob_A.Objective, solution);
    if results_A(i,4) > temp
        temp = results_A(i,4);
        results(i,1) = results_A(i,1);
        results(i,2) = results_A(i,2);
        results(i,3) = results_A(i,3);
        results(i,4) = results_A(i,4)+eps1;
        results(i,12) = "A";
    end
    %===============================
    %-------------B-----------------
    %===============================
    prob_B = optimproblem('ObjectiveSense', 'maximize');
    prob_B.Constraints.cons1 = e2 <= 1;
    prob_B.Objective = (a2 * e2 - b2 * e2^c2 - fc2);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_B);
    results_B(i,1) = 0;
    results_B(i,2) = solution.e2;
    results_B(i,3) = 0;
    %store optimized objective function value
    results_B(i,4) = evaluate(prob_B.Objective, solution);
    if results_B(i,4)> temp
        temp = results_B(i,4);
        results(i,1) = results_B(i,1);
        results(i,2) = results_B(i,2);
        results(i,3) = results_B(i,3);
        results(i,4) = results_B(i,4)+eps2;
        results(i,12) = "B";
    end
    %===============================
    %-------------C-----------------
    %===============================
    prob_C = optimproblem('ObjectiveSense', 'maximize');
    prob_C.Constraints.cons1 = e3 <= 1;
    prob_C.Objective = (a3 * e3 - b3 * e3^c3 - fc3);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_C);
    results_C(i,1) = 0;
    results_C(i,2) = 0;
    results_C(i,3) = solution.e3;
    %store optimized objective function value
    results_C(i,4) = evaluate(prob_C.Objective, solution);
    if results_C(i,4)> temp
        temp = results_C(i,4);
        results(i,1) = results_C(i,1);
        results(i,2) = results_C(i,2);
        results(i,3) = results_C(i,3);
        results(i,4) = results_C(i,4)+eps3;
        results(i,12) = "C";
    end
    %===============================
    %-------------BC-----------------
    %===============================
    prob_BC = optimproblem('ObjectiveSense', 'maximize');
    prob_BC.Constraints.cons1 = e2 + e3 <= 1;
    prob_BC.Objective = (a2 * e2 - b2 * e2^c2 - fc2) + (a3 * e3 - b3 * e3^c3 - fc3);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_BC);
    results_BC(i,1) = 0;
    results_BC(i,2) = solution.e2;
    results_BC(i,3) = solution.e3;
    %store optimized objective function value
    results_BC(i,4) = evaluate(prob_BC.Objective, solution);
    if results_BC(i,4)> temp
        temp = results_BC(i,4);
        results(i,1) = results_BC(i,1);
        results(i,2) = results_BC(i,2);
        results(i,3) = results_BC(i,3);
        results(i,4) = results_BC(i,4)+eps2+eps3;
        results(i,12) = "BC";
    end
    %===============================
    %-------------AC-----------------
    %===============================
    prob_AC = optimproblem('ObjectiveSense', 'maximize');
    prob_AC.Constraints.cons1 = e1 + e3 <= 1;
    prob_AC.Objective = (a1 * e1 - b1 * e1^c1 - fc1) + (a3 * e3 - b3 * e3^c3 - fc3);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_AC);
    results_AC(i,1) = solution.e1;
    results_AC(i,2) = 0;
    results_AC(i,3) = solution.e3;
    %store optimized objective function value
    results_AC(i,4) = evaluate(prob_AC.Objective, solution);
    if results_AC(i,4)> temp
        temp = results_AC(i,4);
        results(i,1) = results_AC(i,1);
        results(i,2) = results_AC(i,2);
        results(i,3) = results_AC(i,3);
        results(i,4) = results_AC(i,4)+eps1+eps3;
        results(i,12) = "AC";
    end
    %===============================
    %-------------AB-----------------
    %===============================
    prob_AB = optimproblem('ObjectiveSense', 'maximize');
    prob_AB.Constraints.cons1 = e1 + e2 <= 1;
    prob_AB.Objective = (a1 * e1 - b1 * e1^c1 - fc1) + (a2 * e2 - b2 * e2^c2 - fc2);
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob_AB);
    results_AB(i,1) = solution.e1;
    results_AB(i,2) = solution.e2;
    results_AB(i,3) = 0;
    %store optimized objective function value
    results_AB(i,4) = evaluate(prob_AB.Objective, solution);
    if results_AB(i,4)> temp
        temp = results_AB(i,4);
        results(i,1) = results_AB(i,1);
        results(i,2) = results_AB(i,2);
        results(i,3) = results_AB(i,3);
        results(i,4) = results_AB(i,4)+eps1+eps2;
        results(i,12) = "AB";
    end
    data = [(a1 * results(i,1) - b1 * results(i,1)^c1),(a2 * results(i,2) - b2 * results(i,2)^c2),(a3 * results(i,3) - b3 * results(i,3)^c3)];
    %HHI of revenue concentration
    results(i,5) = sqrt(sum((data./sum(data)).^2));
   
    results(i,6) = a1;
    results(i,7) = a2;
    results(i,8) = a3;
    results(i,9) = b1;
    results(i,10) = b2;
    results(i,11) = b3;
end


