numIterations = 500;
% Initialize a cell array to store the results

% intialize the results matrices
results = zeros(numIterations, 12);
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
    
    eps1 = 0.05 * randn(n, 1);
    eps3 = 0.05 * randn(n, 1);
    eps2 = 0.05 * randn(n, 1);
    
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
    results(i,1:4) = results_ABC(i,1:4);

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
    if results_A(i,4) > results(i,4)
        results(i,1) = results_A(i,1);
        results(i,2) = results_A(i,2);
        results(i,3) = results_A(i,3);
        results(i,4) = results_A(i,4);
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
    if results_B(i,4)>results(i,4)
        results(i,1) = results_B(i,1);
        results(i,2) = results_B(i,2);
        results(i,3) = results_B(i,3);
        results(i,4) = results_B(i,4);
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
    if results_C(i,4)>results(i,4)
        results(i,1) = results_C(i,1);
        results(i,2) = results_C(i,2);
        results(i,3) = results_C(i,3);
        results(i,4) = results_C(i,4);
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
    if results_BC(i,4)>results(i,4)
        results(i,1) = results_BC(i,1);
        results(i,2) = results_BC(i,2);
        results(i,3) = results_BC(i,3);
        results(i,4) = results_BC(i,4);
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
    if results_AC(i,4)>results(i,4)
        results(i,1) = results_AC(i,1);
        results(i,2) = results_AC(i,2);
        results(i,3) = results_AC(i,3);
        results(i,4) = results_AC(i,4);
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
    if results_AB(i,4)>results(i,4)
        results(i,1) = results_AB(i,1);
        results(i,2) = results_AB(i,2);
        results(i,3) = results_AB(i,3);
        results(i,4) = results_AB(i,4);
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
    [M,I] = max(data);
    results(i,12) = I;
end


%results = cell2mat(results_ABC);
mean_effort = mean(results(:, 1:3), 'all');
mean_income = mean(results(:, 4), 'all');

%HHI of revenue concentration
simp_index = sqrt(sum(results(:, 1:3).^2,2));
%shan_index2 = -sum(results(:, 1:3).*log(results(:, 1:3)),2, "omitmissing");
%shan_index2(:,2) = results(:, 4);
%scatter(shan_index2(:,1),shan_index2(:,2));
%scatter(shan_index(:,1),shan_index(:,2));

%% 
scatter(simp_index(:,1),results(:, 4));
%%
scatter(results(:, 5),results(:, 4));

%histogram(results(:, 1:3));
%var(results(:, 1:3),0,2)/mean_effort

