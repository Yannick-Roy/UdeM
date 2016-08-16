function starDF = StaR_getVarValDF_PostHoc(filename)
    disp(['Loading Workspace from: ' filename]);
    workspaceR = load(filename);
    
    starDF.timeData = 1:1536;
    starDF.typeData = 'ERP';
    starDF.iDesign = 0;
    starDF.designName = 'PostHoc';
    starDF.pValsSub = {};
    
    %% -- Reshape data & pVals -- 
    pValsSub_reshaped = [];
    pValsSubCorrected_reshaped = [];
    
    if(strcmp(starDF.typeData, 'ERP'))
        dataTypeLength = length(starDF.timeData);
    end    
    if(strcmp(starDF.typeData, 'ERSP'))
        dataTypeLength = length(starDF.timeData) * length(starDF.freqData);
    end
    
    %data_reshaped = reshape(workspaceR.dataSub, dataTypeLength, length(workspaceR.titlesSub));
    
    pValsSub_reshaped = reshape(workspaceR.pValsSub, dataTypeLength, length(workspaceR.pTitlesSub));
    %pValsSubCorrected_reshaped = reshape(workspaceR.pValsSubCorrected, dataTypeLength, length(workspaceR.pTitlesSub));
    
    %pValsFull_reshaped = reshape(workspaceR.pValsFull, dataTypeLength, length(cellstr(workspaceR.pTitlesFull)));
    %pValsFullCorrected_reshaped = reshape(workspaceR.pValsFullCorrected, dataTypeLength, length(cellstr(workspaceR.pTitlesFull)));
    
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
        %starDF.pValsSub{i}.plotValCorrected = pValsSubCorrected_reshaped(:, i);

        if strcmp(starDF.typeData, 'ERSP')
            starDF.pValsSub{i}.plotVal = reshape(starDF.pValsSub{i}.plotVal, length(starDF.freqData), length(starDF.timeData))';
            %starDF.pValsSub{i}.plotValCorrected = reshape(starDF.pValsSub{i}.plotValCorrected, length(starDF.freqData), length(starDF.timeData))';
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
    
end