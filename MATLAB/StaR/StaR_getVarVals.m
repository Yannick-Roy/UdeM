% StaR_getVarVals(): StaR Protocol for 'decoding' the plots & titles
% received from R. This protocol helps to make sure the right data is
% generated from the righ variables / values.
%       
% It returns a cell array of unique variables and unique values. The value
% indexes correspond to the variable index.
% Exemple: 
% uniqueVars = {'conditions' 'groups'}
% uniqueVals = {'FOF' '3'}

% Author: Yannick Roy, UdeM
% 
% === HISTORY ===
% 06/05/2016 ver 0.01 by Yannick.
% === /HISTORY ===
%

function [uniqueVars, uniqueVals] = StaR_getVarVals(vars, vals)

    verbose = 0;

    if ~iscell(vars{1})
        for i = 1:length(vars)
            if ~isempty(vars{i})
                vars{i} = {vars{i}};
            end
        end
    end
    
    if ~iscell(vals{1})
        for i = 1:length(vals)
            if ~isempty(vals{i})
                vals{i} = {vals{i}};
            end
        end
    end
    
    if size(vars,1) > 1
        for i = 1:size(vars,1)
            vars(i,:) = vars(i, ~cellfun('isempty',vars(i,:)));
        end
    end
    if size(vals,1) > 1
        for i = 1:size(vals,1)
            vals(i,:) = vals(i, ~cellfun('isempty',vals(i,:)));
        end
    end
    
    tmpVarsL1 = [vars{1,:}]; % All other lines should have the same name (var) as L1. But different Val.
    uniqueVars = unique(tmpVarsL1);
    uniqueVals = {};
    combinedVals = {};
    for i = 1:length(uniqueVars)
        combinedVals{i} = {};
        for j = 1:size(vars, 2)
            if strcmp(uniqueVars(i), vars{1,j})
                %[vals{:,j}]
                if(length(combinedVals{i}) == 0)
                    combinedVals{i} = [vals{:,j}];
                else
                    combinedVals{i} = [combinedVals{i} vals{:,j}];
                end
            end
        end
    end
    
    for v = 1:length(combinedVals)
        uniqueVals{v} = unique(combinedVals{v});
    end
    
    if verbose > 0
        disp('=============================');
        for v = 1:length(combinedVals)
            disp(['Var: ' uniqueVars(v) ' : ' uniqueVals{v}])
        end
        disp('=============================');
    end
end
