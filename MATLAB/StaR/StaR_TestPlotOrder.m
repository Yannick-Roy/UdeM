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

% nbPlots = 0;
% 
% disp('Searching...');
% for i = 1:length(vars) % For each var
%     for p = 1:length(starDF_pVals) % For each plot (title)
%         
%         % Sanity Check on Variables.
%         otherVar = setdiff(vars, [starDF_pVals{1,p}.var{1,:}]);
%         if ~isempty(otherVar)
%             disp(['XX OTHER VAR - ' otherVar]);
%             bVarOther = 1;
%         end
%         
%         bTmpVarOther = 0;
%         for v = 1:size(starDF_pVals{1,p}.var, 2) % For each variable in that plot
%             if(strcmp(vars{i}, starDF_pVals{1,p}.var{1,v}))
%                 disp(['GOT IT - VAR - ' starDF_pVals{1,p}.var{1,v}]);
%                 bTmpValOther = 0;
%                 for vv = 1:size(starDF_pVals{1,p}.val, 1) % For each value of each variable
%                     if(strcmp(vals{i}, starDF_pVals{1,p}.val{vv,v}))
%                         disp(['GOT IT - VAL - ' starDF_pVals{1,p}.val{vv,v}]);
%                     else
%                         disp(['XX DIFF VAL - ' starDF_pVals{1,p}.val{vv,v}]); 
%                         bTmpValOther = 1;
%                     end
%                 end
%                 if bTmpValOther == 0
%                     bValCheck(i) = 1;
%                 end
%             else
%                 bTmpVarOther = 1;
%             end
%             if bTmpVarOther == 0
%                 bVarCheck(i) = 1;
%             end
%         end
%         
%         if(length(bVarCheck(bVarCheck == 0)) == 0 && length(bValCheck(bValCheck == 0)) == 0)
%             disp('Got myself a plot!');
%             nbPlots = nbPlots + 1;
%         end
%     end
% end
% disp(['nbPlots : ' num2str(nbPlots)]);
% disp('Done!');


%% Add Some Tests to the previous section.
starDF_pVals{1, 1}.val{4,3} = {'4'};
starDF_pVals{1, 1}.val{3,3} = {'4'};
starDF_pVals{1, 1}.val{2,3} = {'4'};
starDF_pVals{1, 1}.val{1,3} = {'4'};

starDF_pVals{1, 1}.var{4,3} = {'groups'};
starDF_pVals{1, 1}.var{3,3} = {'groups'};
starDF_pVals{1, 1}.var{2,3} = {'groups'};
starDF_pVals{1, 1}.var{1,3} = {'groups'};


%% Compare that shit!
vars_g = {{'conditions'}, {'groups'}, {'groups'}};
vals_g = {{'FOM'}, {'3'}, {'4'}};

df = StaR_getVarValDF('');
plotIDs = StaR_getVarValPlots(vars_g, vals_g, df);

plot(df{plotIDs}.plotVal);
%plot = StaR_getVarValPlots([vars_g{:}], [vals_g{:}], df);
%[vars, vals] = StaR_getVarVals(df);


    