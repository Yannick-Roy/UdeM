% YR 2015 %
% Script to export MPT's data for stats in R (or else).

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
save('export_mpt', 'linearProjectedMeasure', 'sessionConditionCell', 'groupId', 'uniqeDatasetId', 'dipoleDensity', 'times', 'freqs');
