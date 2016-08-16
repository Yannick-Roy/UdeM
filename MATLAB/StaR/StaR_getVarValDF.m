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
    %starDF.iDesign = workspaceR.iDesign;
    starDF.designName = workspaceR.designName;
    starDF.pValsSub = {};
    starDF.pValsFull = {};
    starDF.data = {};
    
    %% -- Reshape data & pVals -- 
    data_reshaped = [];
    pValsSub_reshaped = [];
    pValsSubCorrected_reshaped = [];
    pValsFull_reshaped = [];
    pValsFullCorrected_reshaped = [];
    
    workspaceR.pTitlesFull = cellstr(workspaceR.pTitlesFull);
    %workspaceR.pTitlesSub = cellstr(workspaceR.pTitlesSub);
    
    if(strcmp(workspaceR.typeData, 'ERP'))
        dataTypeLength = length(workspaceR.timeData);
    end    
    if(strcmp(workspaceR.typeData, 'ERSP'))
        dataTypeLength = length(workspaceR.timeData) * length(workspaceR.freqData);
    end
    
    data_reshaped = reshape(workspaceR.dataSub, dataTypeLength, length(workspaceR.titlesSub));
    
    pValsSub_reshaped = reshape(workspaceR.pValsSub, dataTypeLength, length(workspaceR.pTitlesSub));
    pValsSubCorrected_reshaped = reshape(workspaceR.pValsSubCorrected, dataTypeLength, length(workspaceR.pTitlesSub));
    
    pValsFull_reshaped = reshape(workspaceR.pValsFull, dataTypeLength, length(cellstr(workspaceR.pTitlesFull)));
    pValsFullCorrected_reshaped = reshape(workspaceR.pValsFullCorrected, dataTypeLength, length(cellstr(workspaceR.pTitlesFull)));
    
    %% -- Get pVals Sub --
    pTitlesSub_s    = {}; 
    pTitlesSub_ss   = {};
    for i=1:length(workspaceR.pTitlesSub) % For each plot / title
        pTitlesSub_s{i} = strsplit(workspaceR.pTitlesSub{i}, '|');
        pTitlesSub_s{i} = pTitlesSub_s{i}(~cellfun('isempty', pTitlesSub_s{i})); 
        starDF.pValsSub{i}.lbl = workspaceR.pTitlesSub{i};
        starDF.pValsSub{i}.var = {};
        starDF.pValsSub{i}.val = {};
        starDF.pValsSub{i}.plotVal = pValsSub_reshaped(:, i);
        starDF.pValsSub{i}.plotValCorrected = pValsSubCorrected_reshaped(:, i);

        if strcmp(starDF.typeData, 'ERSP')
            starDF.pValsSub{i}.plotVal = reshape(starDF.pValsSub{i}.plotVal, length(starDF.freqData), length(starDF.timeData))';
            starDF.pValsSub{i}.plotValCorrected = reshape(starDF.pValsSub{i}.plotValCorrected, length(starDF.freqData), length(starDF.timeData))';
        end
        
        for j=1:length(pTitlesSub_s{i}) % For each 'sub data' that contributed...
            pTitlesSub_ss{j} = strsplit(pTitlesSub_s{i}{j}, ';');
            for k=1:length(pTitlesSub_ss{j}) % For each var/val
                tmp = strsplit(pTitlesSub_ss{j}{k}, '=');

                if(length(tmp) >= 2 && ~isempty(tmp) && ~isempty(tmp))
                    starDF.pValsSub{i}.var{j,k} = strtrim(tmp(1));
                    starDF.pValsSub{i}.val{j,k} = strtrim(tmp(2));
                end
            end
        end
    end
    
    %% -- Get pVals Full --
    pTitlesFull_s    = {}; 
    pTitlesFull_ss   = {};
    for i=1:length(workspaceR.pTitlesFull) % For each plot / title
        if ~iscell(workspaceR.pTitlesFull)
            if length(workspaceR.pTitlesFull) > 0
                workspaceR.pTitlesFull = {workspaceR.pTitlesFull};
            end
        end
        pTitlesFull_s{i} = strsplit(workspaceR.pTitlesFull{i}, '|');
        pTitlesFull_s{i} = pTitlesFull_s{i}(~cellfun('isempty', pTitlesFull_s{i})); 
        starDF.pValsFull{i}.lbl = workspaceR.pTitlesFull{i};
        starDF.pValsFull{i}.var = {};
        starDF.pValsFull{i}.val = {};
        starDF.pValsFull{i}.plotVal = pValsFull_reshaped(:, i);
        starDF.pValsFull{i}.plotValCorrected = pValsFullCorrected_reshaped(:, i);

        if strcmp(starDF.typeData, 'ERSP')
            starDF.pValsFull{i}.plotVal = reshape(starDF.pValsFull{i}.plotVal, length(starDF.freqData), length(starDF.timeData))';
            starDF.pValsFull{i}.plotValCorrected = reshape(starDF.pValsFull{i}.plotValCorrected, length(starDF.freqData), length(starDF.timeData))';
        end
        
        for j=1:length(pTitlesFull_s{i}) % For each 'sub data' that contributed...
            pTitlesFull_ss{j} = strsplit(pTitlesFull_s{i}{j}, ';');
            for k=1:length(pTitlesFull_ss{j}) % For each var/val
                tmp = strsplit(pTitlesFull_ss{j}{k}, '=');

                if(length(tmp) >= 2 && ~isempty(tmp) && ~isempty(tmp))
                    starDF.pValsFull{i}.var{j,k} = strtrim(tmp(1));
                    starDF.pValsFull{i}.val{j,k} = strtrim(tmp(2));
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