% StaR_getVarValPlots(): 

% Author: Yannick Roy, UdeM
% 
% === HISTORY ===
% 06/05/2016 ver 0.01 by Yannick.
% === /HISTORY ===
%

function plotIDs = StaR_getVarValPlots(vars, vals, starDF, plotType)

    if strcmp(plotType, 'data')
        plotsFromDF = starDF.data
    else
        plotsFromDF = starDF.pVals
    end
    
    goodPlotIDs = [];
    for p = 1:length(plotsFromDF)
        [var_wanted, val_wanted] = StaR_getVarVals(vars, vals);
        [var_plot, val_plot] = StaR_getVarVals(plotsFromDF{p}.var, plotsFromDF{p}.val);

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
            goodPlotIDs = [goodPlotIDs p];
        end
    end

    if length(goodPlotIDs) > 0
        disp('Good Plots:');
        disp(goodPlotIDs);
    else
        disp('Aucun Plot Correspondant...');
    end
    
    plotIDs = goodPlotIDs;
end