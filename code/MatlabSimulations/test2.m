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
fc1 = 0.2 + 0.2*rand(n, 1);
fc2 = 0.2 + 0.2*rand(n, 1);
fc3 = 0.2 + 0.2*rand(n, 1);

e1 = optimvar('e1','UpperBound',1,'LowerBound',0);
e2 = optimvar('e2','UpperBound',1,'LowerBound',0);
e3 = optimvar('e3','UpperBound',1,'LowerBound',0);
prob_ABC = optimproblem('ObjectiveSense', 'max');
prob_ABC.Constraints.cons1 = (e1 + e2 + e3 <= 1);
prob_ABC.Objective = (a1 * e1 - b1 * e1^c1) + (a2 * e2 - b2 * e2^c2) + (a3 * e3 - b3 * e3^c3);
show(prob_ABC)
% Solve the problem and store the solution in the results cell array
solution = solve(prob_ABC)
solution.e1
i=1;

select_oneORthree_fishe(a1,a2,a3,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,i,choice_results,results_A,results_B,results_C,results_ABC)
    % e1 = optimvar('e1','UpperBound',1,'LowerBound',0);
    % e2 = optimvar('e2','UpperBound',1,'LowerBound',0);
    % e3 = optimvar('e3','UpperBound',1,'LowerBound',0);
    % prob_ABC = optimproblem('ObjectiveSense', 'maximize');
    % prob_ABC.Constraints.cons1 = (e1 + e2 + e3 <= 1);
    % prob_ABC.Objective = (a1 * e1 - b1 * e1^c1) + (a2 * e2 - b2 * e2^c2) + (a3 * e3 - b3 * e3^c3);
    % % Solve the problem and store the solution in the results cell array
    % solution = solve(prob_ABC);
    % %disp(solution)
    % out_results = choice_results;
    % out_results(i,1) = solution.e1
    % out_results(i,2) = solution.e2
    % out_results(i,3) = solution.e3
    % results_ABC(i,4) = evaluate(prob_ABC.Objective, solution);
    % out_results(i,1:4) = results_ABC(i,1:4)+eps1+eps2+eps3-fc1-fc2-fc3;%realized net revenue
    % temp = results_ABC(i,1:4)-fc1-fc2-fc3;%temp is max expected revenue from any strategy
    % strategy = "ABC";

    %%
    function [out_results,strategy] = select_oneORthree_fishe(a1,a2,a3,b1,b2,b3,c1,c2,c3,eps1,eps2,eps3,fc1,fc2,fc3,i,in_results,results_A,results_B,results_C,results_ABC)
        e1 = optimvar('e1','UpperBound',1,'LowerBound',0);
        e2 = optimvar('e2','UpperBound',1,'LowerBound',0);
        e3 = optimvar('e3','UpperBound',1,'LowerBound',0);
        prob_ABC = optimproblem('ObjectiveSense', 'maximize');
        prob_ABC.Constraints.cons1 = (e1 + e2 + e3 <= 1);
        prob_ABC.Objective = (a1 * e1 - b1 * e1^c1) + (a2 * e2 - b2 * e2^c2) + (a3 * e3 - b3 * e3^c3);
        % Solve the problem and store the solution in the results cell array
        solution = solve(prob_ABC);
        %disp(solution)
        out_results = in_results;
        out_results(i,1) = solution.e1;
        out_results(i,2) = solution.e2;
        out_results(i,3) = solution.e3;
        disp(solution)
        %store optimized objective function value
        results_ABC(i,4) = evaluate(prob_ABC.Objective, solution);
        out_results(i,4) = results_ABC(i,4)+eps1+eps2+eps3-fc1-fc2-fc3;%realized net revenue
        temp = results_ABC(i,4)-fc1-fc2-fc3;%temp is max expected revenue from any strategy
        strategy = "ABC";
    end
