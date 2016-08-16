% StaR_getVarValPlots(): 

% Author: Yannick Roy, UdeM
% 
% === HISTORY ===
% 06/05/2016 ver 0.01 by Yannick.
% === /HISTORY ===
%

function starCustomDF = StaR_getCustomPlots(filename)
    disp(['Loading Custom Plots from: ' filename]);
    workspaceR = load(filename);
    
    starCustomDF.typeData = workspaceR.typeData;
    starCustomDF.timeData = workspaceR.timeData;
    starCustomDF.freqData = workspaceR.freqData;
    %starCustomDF.nbRows = workspaceR.nbRows;
    %starCustomDF.nbCols = workspaceR.nbCols;
    starCustomDF.plotValues = {};
    starCustomDF.plotTitles = {};
    starCustomDF.mainTitles = {};
    
    %% -- Reshape data -- 
    data_reshaped = [];
    
    if(strcmp(workspaceR.typeData, 'ERP'))
        dataTypeLength = length(workspaceR.timeData);
    end    
    
    
    if(strcmp(workspaceR.typeData, 'ERSP'))
        dataTypeLength = length(workspaceR.timeData) * length(workspaceR.freqData);
        
        data_reshaped = reshape(workspaceR.plotValues, dataTypeLength, length(workspaceR.plotTitles));
    
        for i = 1:size(data_reshaped, 2)
            plots{i} = reshape(data_reshaped(:, i), length(workspaceR.freqData), length(workspaceR.timeData))'; 
            plots{i} = {plots{i};plots{i}};
        end
        
        titles = workspaceR.plotTitles';
    
        std_plottf(workspaceR.timeData, workspaceR.freqData, plots, 'titles', titles);
    end
    

    
    % Set Title.
end