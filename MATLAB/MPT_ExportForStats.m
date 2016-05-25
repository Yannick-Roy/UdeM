% YR 2015 %
% Script to export MPT's data for stats in R (or else).

%% Manual Export

%=============================
% Nima's Code to export Data.
%=============================
domainNumber = 1;
dipoleAndMeasure = STUDY.measureProjection.ersp.object; % get the ERSP and dipole data (dataAndMeasure object) from the STUDY structure.
domain = STUDY.measureProjection.ersp.projection.domain(domainNumber); % get the domain in a separate variable
projection  = STUDY.measureProjection.ersp.projection;
headGrid = STUDY.measureProjection.ersp.headGrid;
[linearProjectedMeasure sessionConditionCell groupId uniqeDatasetId dipoleDensity] = dipoleAndMeasure.getMeanProjectedMeasureForEachSession(headGrid, domain.membershipCube, projection.projectionParameter);
%=============================
% Nima's Code to export Data.
%=============================          

% ERSPs Time/Freq Axes.
times = STUDY.measureProjection.ersp.object.time;
freqs = STUDY.measureProjection.ersp.object.frequency;

% Export data.
save('export_test_mpt', 'linearProjectedMeasure', 'sessionConditionCell', 'groupId', 'uniqeDatasetId', 'dipoleDensity', 'times', 'freqs');
%save('export_mpt', 'linearProjectedMeasure', 'sessionConditionCell', 'groupId', 'uniqeDatasetId', 'dipoleDensity', 'times', 'freqs');


%% Automatic Export
measures = {'erp', 'ersp'};

for m = measures
    curMeasure = m{1};
    
    disp(curMeasure);
    if strcmp(curMeasure, 'ersp')
        % ERSPs Time/Freq Axes.
        times = STUDY.measureProjection.ersp.object.time;
        freqs = STUDY.measureProjection.ersp.object.frequency;
    end
    
    for curDomain = 1:length(STUDY.measureProjection.(curMeasure).projection.domain)
        disp(curDomain);
        
        %=============================
        % Nima's Code to export Data.
        %=============================
        domainNumber = curDomain;
        dipoleAndMeasure = STUDY.measureProjection.(curMeasure).object; % get the ERSP and dipole data (dataAndMeasure object) from the STUDY structure.
        domain = STUDY.measureProjection.(curMeasure).projection.domain(domainNumber); % get the domain in a separate variable
        projection  = STUDY.measureProjection.(curMeasure).projection;
        headGrid = STUDY.measureProjection.(curMeasure).headGrid;
        [linearProjectedMeasure sessionConditionCell groupId uniqeDatasetId dipoleDensity] = dipoleAndMeasure.getMeanProjectedMeasureForEachSession(headGrid, domain.membershipCube, projection.projectionParameter);
        %=============================
        % Nima's Code to export Data.
        %=============================

        % Export data.
        if strcmp(curMeasure, 'ersp')
            save(['MPT_exp_' curMeasure '_D' num2str(curDomain)], 'linearProjectedMeasure', 'sessionConditionCell', 'groupId', 'uniqeDatasetId', 'dipoleDensity', 'times', 'freqs');
        else
            save(['MPT_exp_' curMeasure '_D' num2str(curDomain)], 'linearProjectedMeasure', 'sessionConditionCell', 'groupId', 'uniqeDatasetId', 'dipoleDensity');
        end   
    end
end