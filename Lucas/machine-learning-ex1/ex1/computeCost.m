function J = computeCost(X, y, theta)
%COMPUTECOST Compute cost for linear regression
%   J = COMPUTECOST(X, y, theta) computes the cost of using theta as the
%   parameter for linear regression to fit the data points in X and y

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost of a particular choice of theta
%               You should set J to the cost.

h_out = X * theta;             % result of hypothesis fn
error = h_out - y;             % error vector between h and actual y
sqr_error = error .^ 2;        % error vector squared
total_error = sum(sqr_error);  % sum of error vector

% FINAL COST
cost = (1 / 2 * length(error)) * total_error;
J = cost;


% =========================================================================

end
