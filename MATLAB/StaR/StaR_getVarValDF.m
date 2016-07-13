% StaR_getVarValPlots(): 

% Author: Yannick Roy, UdeM
% 
% === HISTORY ===
% 06/05/2016 ver 0.01 by Yannick.
% === /HISTORY ===
%

function starDF = StaR_getVarValDF(filename)
    disp(['Loading Workspace from: ' filename]);
    workspaceR = load(filename);
    
    starDF.timeData = workspaceR.timeData;
    starDF.freqData = workspaceR.freqData;
    starDF.typeData = workspaceR.typeData;
    starDF.iDesign = workspaceR.iDesign;
    starDF.designName = workspaceR.designName;
    starDF.pVals = {};
    starDF.data = {};
    
    %% -- Reshape data & pVals -- 
    data_reshaped = [];
    pVals_reshaped = [];
    pValsCorrected_reshaped = [];
    
    if(strcmp(workspaceR.typeData, 'ERP'))
        dataTypeLength = length(workspaceR.timeData);
    end    
    if(strcmp(workspaceR.typeData, 'ERSP'))
        dataTypeLength = length(workspaceR.timeData) * length(workspaceR.freqData);
    end
    
    data_reshaped = reshape(workspaceR.dataSub, dataTypeLength, length(workspaceR.titlesSub));
    pVals_reshaped = reshape(workspaceR.pValsSub, dataTypeLength, length(workspaceR.pTitlesSub));
    pValsCorrected_reshaped = reshape(workspaceR.pValsSubCorrected, dataTypeLength, length(workspaceR.pTitlesSub));
    %data_reshaped = reshape(workspaceR.dataSub, length(workspaceR.titlesSub), dataTypeLength)';
    %pVals_reshaped = reshape(workspaceR.pValsSub, length(workspaceR.pTitlesSub), dataTypeLength)';
    
    %% -- Get pVals --
    pTitlesSub_s    = {}; 
    pTitlesSub_ss   = {};
    for i=1:length(workspaceR.pTitlesSub) % For each plot / title
        pTitlesSub_s{i} = strsplit(workspaceR.pTitlesSub{i}, '|');
        pTitlesSub_s{i} = pTitlesSub_s{i}(~cellfun('isempty', pTitlesSub_s{i})); 
        starDF.pVals{i}.lbl = workspaceR.pTitlesSub{i};
        starDF.pVals{i}.var = {};
        starDF.pVals{i}.val = {};
        starDF.pVals{i}.plotVal = pVals_reshaped(:, i);
        starDF.pVals{i}.plotValCorrected = pValsCorrected_reshaped(:, i);

        if strcmp(starDF.typeData, 'ERSP')
            starDF.pVals{i}.plotVal = reshape(starDF.pVals{i}.plotVal, length(starDF.freqData), length(starDF.timeData));
            starDF.pVals{i}.plotValCorrected = reshape(starDF.pVals{i}.plotValCorrected, length(starDF.freqData), length(starDF.timeData));
        end
        
        for j=1:length(pTitlesSub_s{i}) % For each 'sub data' that contributed...
            pTitlesSub_ss{j} = strsplit(pTitlesSub_s{i}{j}, ';');
            for k=1:length(pTitlesSub_ss{j}) % For each var/val
                tmp = strsplit(pTitlesSub_ss{j}{k}, '=');

                if(length(tmp) >= 2 && ~isempty(tmp) && ~isempty(tmp))
                    starDF.pVals{i}.var{j,k} = strtrim(tmp(1));
                    starDF.pVals{i}.val{j,k} = strtrim(tmp(2));
                end
            end
        end
    end
    
    %% -- Get data --
    titlesSub_s    = {}; 
    titlesSub_ss   = {};
    for i=1:length(workspaceR.titlesSub) % For each plot / title
        titlesSub_s{i} = strsplit(workspaceR.titlesSub{i}, '|');
        titlesSub_s{i} = titlesSub_s{i}(~cellfun('isempty', titlesSub_s{i})); 
        starDF.data{i}.lbl = workspaceR.titlesSub{i};
        starDF.data{i}.var = {};
        starDF.data{i}.val = {};
        starDF.data{i}.plotVal = data_reshaped(:, i);

        if strcmp(starDF.typeData, 'ERSP')
            starDF.data{i}.plotVal = reshape(starDF.data{i}.plotVal, length(starDF.freqData), length(starDF.timeData))';
        end
        
        for j=1:length(titlesSub_s{i}) % For each 'sub data' that contributed...
            titlesSub_ss{j} = strsplit(titlesSub_s{i}{j}, ';');
            for k=1:length(titlesSub_ss{j}) % For each var/val
                tmp = strsplit(titlesSub_ss{j}{k}, '=');

                if(length(tmp) >= 2 && ~isempty(tmp) && ~isempty(tmp))
                    starDF.data{i}.var{j,k} = strtrim(tmp(1));
                    starDF.data{i}.val{j,k} = strtrim(tmp(2));
                end
            end
        end
    end
end