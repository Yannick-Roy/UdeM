
a = { rand(1,10) rand(1,10)+0.5 }; % pseudo 'paired' data vectors
[t df pvals] = statcond(a);        % perform paired t-test
%           pvals =                  
%              5.2807e-04 % standard t-test probability value
%         % Note: for different rand() outputs, results will differ.
%
%         [t df pvals surog] = statcond(a, 'method', 'perm', 'naccu', 2000); 
%           pvals =
%              0.0065 % nonparametric t-test using 2000 permuted data sets
%
%         a = { rand(2,11) rand(2,10) rand(2,12)+0.5 }; % pseudo 'unpaired' 
%         [F df pvals] = statcond(a); % perform an unpaired ANOVA 
%           pvals =
%              0.00025 % p-values for difference between columns 
%              0.00002 % for each data row
%
        a = { rand(3,4,10) rand(3,4,10) rand(3,4,10); ...
              rand(3,4,10) rand(3,4,10) rand(3,4,10)+0.5 }; 
%         % pseudo (2,3)-condition data array, each entry containing 
%         %                                    ten (3,4) data matrices
         [F df pvals] = statcond(a);  % perform a paired 2-way ANOVA 
%         % Output:
%           pvals{1} % a (3,4) matrix of p-values; effects across rows
%           pvals{2} % a (3,4) matrix of p-values; effects across colums 
%           pvals{3} % a (3,4) matrix of p-values; interaction effects
%                                      % across rows and columns
%





 std_plottf(t, f, projectedMeasureForDatasetWithConditionsSeparated)
 
 [F df pvals] = statcond(projectedMeasureForDatasetWithConditionsSeparated);