numIterations = 100;

% Initialize a cell array to store the results
results = cell(numIterations, 1);

for i = 1:numIterations
    n = 1;
    a1 = 5 + rand(n, 1);
    a2 = 5 + rand(n, 1);
    a3 = 5 + rand(n, 1);
    
    eps1 = 0.05 * randn(n, 1);
    eps3 = 0.05 * randn(n, 1);
    eps2 = 0.05 * randn(n, 1);
    
    c1 = 2;
    c2 = 2;
    c3 = 2;
    
    b1 = c1 + (4) * rand(n, 1);
    b2 = c2 + (4) * rand(n, 1);
    b3 = c3 + (4) * rand(n, 1);
    
    e1 = optimvar('e1', 'LowerBound', 0, 'UpperBound', 1);
    e2 = optimvar('e2', 'LowerBound', 0, 'UpperBound', 1);
    e3 = optimvar('e3', 'LowerBound', 0, 'UpperBound', 1);
    
    prob = optimproblem('ObjectiveSense', 'maximize');
    
    prob.Constraints.cons1 = e1 + e2 + e3 <= 1;
    %prob.Constraints.cons2 = e1 <= 1;
    %prob.Constraints.cons3 = e2 <= 1;
    %prob.Constraints.cons4 = e3 <= 1;
    prob.Objective = (a1 * e1 - b1 * e1^c1) + (a2 * e2 - b2 * e2^c2) + (a3 * e3 - b3 * e3^c3);
    
    % Solve the problem and store the solution in the results cell array
    solution = solve(prob);
    results{i,1} = solution.e1;
    results{i,2} = solution.e2;
    results{i,3} = solution.e3;
    %results{i,4} = var(solution.e1,solution.e1,solution.e1);
end
%calculated net revenue
%correlate a and b
%not CV, more like shanon diversity index
results = cell2mat(results);
mean_effort = mean(results(:));
var(results, 0, 2)/mean_effort
%+ fc1*(e1>0) + fc2*(e2>0) + fc3*(e3>0);
%fisherman = table((1:n)', a1, b1, a2, b2, a3, b3, 'VariableNames', {'fisher_id', 'a1', 'b1', 'a2', 'b2', 'a3', 'b3'});
%fc1 = 0.3 + 0.05*randn(n, 1);
%fc2 = 0.3 + 0.05*randn(n, 1);
%fc3 = 0.3 + 0.05*randn(n, 1);