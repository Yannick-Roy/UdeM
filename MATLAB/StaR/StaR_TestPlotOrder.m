%% Data Plots

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% GOOD ONE ! %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
b = reshape(d, length(timeData), 4, 2);
figure('name', 'B: nbPnts x 4 x 2');
for i=1:4
    for j=1:2
        if(j == 1)
            subplot(2,4,i);
        else
            subplot(2,4,i+4);
        end
        plot(b(:,i,j));
    end
end


% WRONG WAY OF DOING IT !!! 
% SEEMS TO WORK, BUT COMPARE WITH R PLOTS!
% a = reshape(means, length(timeData), 2, 4);
% figure('name', 'A: nbPnts x 2 x 4');
% for i=1:2
%     for j=1:4
%         if(i == 1)
%             subplot(2,4,j);
%         else
%             subplot(2,4,j+4);
%         end
%         plot(a(:,i,j));
%     end
% end
     
% WRONG WAY OF DOING IT !!! 
% ALL THE SAME PLOT !
% --> Subsampled by 8 basically! <--
% c = reshape(means, 4, 2, length(timeData));
% figure('name', 'C: 4 x 2 x nbPoints');
% for i=1:4
%     for j=1:2
%         if(j == 1)
%             subplot(2,4,i);
%         else
%             subplot(2,4,i+4);
%         end       
%         plot(squeeze(c(1,1,:)));
%     end
% end


%% Split Labels - Vals %%

load('Test.mat');

dr = reshape(d, 598, length(n));

starDF = {};
ns = {}; 
for i=1:length(n) 
    ns{i} = strsplit(n{i}, '|');
    starDF{i}.lbl = n{i};
    starDF{i}.var = {};
    starDF{i}.val = {};
    starDF{i}.plotVal = dr(:, i);
    
    for j=1:length(ns{i})
        nss = strsplit(ns{i}{j}, '=');
        if(length(nss) >= 2 && ~isempty(nss(1)) && ~isempty(nss(2)))
            starDF{i}.var{j} = strtrim(nss(1));
            starDF{i}.val{j} = strtrim(nss(2));
        end
    end
end

% Test Plot...
for i=1:length(starDF)
    figure('name', starDF{i}.lbl);
    plot(starDF{i}.plotVal);
    title(starDF{i}.lbl);
end


%% Split Labels - pVals %%
load('/Users/yannick/Documents/PhD/Stats Test/mTBI_SubClean_Measures/MPT_Export/Playground/Jul05_13h03/ERP/Domain_1/Stats_aov/Design_ERP_11/Workspace_Fullx.mat')

dr = reshape(d, 598, length(n));

starDF_pVals    = {};
pTitlesSub_s    = {}; 
pTitlesSub_ss   = {};
for i=1:length(pTitlesSub) % For each plot / title
    pTitlesSub_s{i} = strsplit(pTitlesSub{i}, '|');
    pTitlesSub_s{i} = pTitlesSub_s{i}(~cellfun('isempty', pTitlesSub_s{i})); 
    starDF_pVals{i}.lbl = pTitlesSub{i};
    starDF_pVals{i}.var = {};
    starDF_pVals{i}.val = {};
    starDF_pVals{i}.plotVal = dr(:, i);
    
    for j=1:length(pTitlesSub_s{i}) % For each 'sub data' that contributed...
        pTitlesSub_ss{j} = strsplit(pTitlesSub_s{i}{j}, ';');
        for k=1:length(pTitlesSub_ss{j}) % For each var/val
            tmp = strsplit(pTitlesSub_ss{j}{k}, '=')
            
            if(length(tmp) >= 2 && ~isempty(tmp) && ~isempty(tmp))
                starDF_pVals{i}.var{j,k} = tmp(1);
                starDF_pVals{i}.val{j,k} = tmp(2);
            end
        end
    end
end


%% Manipulate Labels
vars_g = {{'conditions'}, {'groups'}, {'groups'}};
vals_g = {{'FOF'}, {'3'}, {'4'}};

vars_g = {{'conditions'}, {'conditions'}, {'groups'}};
vals_g = {{'FOF'}, {'FOM'}, {'3'}};

vars_b = {{'sessions'}, {'groups'}};
vals_b = {{'1'}, {'3'}};

vars = vars_g;
vals = vals_g;

bVarCheck = zeros(1, length(vars));
bValCheck = zeros(length(vals), 1);
bTmpVarOther = 0;
bTmpValOther = 0;

nbPlots = 0;

disp('Searching...');
for i = 1:length(vars) % For each var
    for p = 1:length(starDF_pVals) % For each plot (title)
        
        % Sanity Check on Variables.
        otherVar = setdiff(vars, [starDF_pVals{1,p}.var{1,:}]);
        if ~isempty(otherVar)
            disp(['XX OTHER VAR - ' otherVar]);
            bVarOther = 1;
        end
        
        bTmpVarOther = 0;
        for v = 1:size(starDF_pVals{1,p}.var, 2) % For each variable in that plot
            if(strcmp(vars{i}, starDF_pVals{1,p}.var{1,v}))
                disp(['GOT IT - VAR - ' starDF_pVals{1,p}.var{1,v}]);
                bTmpValOther = 0;
                for vv = 1:size(starDF_pVals{1,p}.val, 1) % For each value of each variable
                    if(strcmp(vals{i}, starDF_pVals{1,p}.val{vv,v}))
                        disp(['GOT IT - VAL - ' starDF_pVals{1,p}.val{vv,v}]);
                    else
                        disp(['XX DIFF VAL - ' starDF_pVals{1,p}.val{vv,v}]); 
                        bTmpValOther = 1;
                    end
                end
                if bTmpValOther == 0
                    bValCheck(i) = 1;
                end
            else
                bTmpVarOther = 1;
            end
            if bTmpVarOther == 0
                bVarCheck(i) = 1;
            end
        end
        
        if(length(bVarCheck(bVarCheck == 0)) == 0 && length(bValCheck(bValCheck == 0)) == 0)
            disp('Got myself a plot!');
            nbPlots = nbPlots + 1;
        end
    end
end
disp(['nbPlots : ' num2str(nbPlots)]);
disp('Done!');


%% Add Some Tests to the previous section.
starDF_pVals{1, 1}.val{4,3} = {'4'};
starDF_pVals{1, 1}.val{3,3} = {'4'};
starDF_pVals{1, 1}.val{2,3} = {'4'};
starDF_pVals{1, 1}.val{1,3} = {'4'};

starDF_pVals{1, 1}.var{4,3} = {'groups'};
starDF_pVals{1, 1}.var{3,3} = {'groups'};
starDF_pVals{1, 1}.var{2,3} = {'groups'};
starDF_pVals{1, 1}.var{1,3} = {'groups'};


%% Bout dCode De La Mort Qui Tue!
% --------------------------------

%vars = starDF_pVals{1, 1}.var;
%vals = starDF_pVals{1, 1}.val;

vars = vars_g;
vals = vals_g;

%function [uniqueVars, uniqueVals] = getVarVals(vars, vals)
    tmpVarsL1 = [vars{1,:}]; % All other lines should have the same name (var) as L1. But different Val.
    uniqueVars = unique(tmpVarsL1);
    uniqueVals = {};
    combinedVals = {};
    for i = 1:length(uniqueVars)
        combinedVals{i} = {};
        for j = 1:size(vars, 2)
            if strcmp(uniqueVars(i), vars{1,j})
                [vals{:,j}]
                if(length(combinedVals{i}) == 0)
                    combinedVals{i} = [vals{:,j}];
                else
                    combinedVals{i} = [combinedVals{i} vals{:,j}];
                end
            end
        end
    end

    disp('=============================');
    for v = 1:length(combinedVals)
        uniqueVals{v} = unique(combinedVals{v});
        disp(['Var: ' uniqueVars(v) ' : ' uniqueVals{v}])
    end
    disp('=============================');
%end
% --------------------------------


%% Compare that shit!
vars_g = {{'conditions'}, {'groups'}, {'groups'}};
vals_g = {{'FOM'}, {'3'}, {'4'}};

goodPlot = [];
for p = 1:size(starDF_pVals, 2)
    [var_wanted, val_wanted] = getVarVals(vars_g, vals_g);
    [var_plot, val_plot] = getVarVals(starDF_pVals{1, p}.var, starDF_pVals{1, p}.val);

    same_var = union(var_wanted, var_plot);
    diff_var = union(setdiff(var_wanted, var_plot), setdiff(var_plot, var_wanted));
    for i = 1:length(var_wanted)
        same_val{i} = union([val_wanted{:,i}], val_plot{:,1});
        diff_val{i} = union(setdiff(val_wanted{:,i}, val_plot{:,i}), setdiff(val_plot{:,i}, val_wanted{:,i}));
    end

    diff = 0;
    if(~isempty(diff_var)) 
        diff = 1; 
    end
    for i = 1:length(diff_val)
        if(~isempty(diff_val{i})) 
            diff = diff + 1; 
        end
    end

    if diff == 0
        disp(['GOOD PLOT! #' num2str(p)]);
        goodPlot = [goodPlot p];
    end
end

if length(goodPlot) > 0
    disp('Good Plots:');
    disp(goodPlot);
else
    disp('Aucun Plot Correspondant...');
end
    