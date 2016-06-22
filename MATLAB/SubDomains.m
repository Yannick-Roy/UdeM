%% read the data (calculated measure, etc.) from STUDY
STUDY.measureProjection.ersp.object = pr.dipoleAndMeasureOfStudyErsp(STUDY, ALLEEG);

% define HeadGRID based on GUI options (you can change this in your script of course)
STUDY.measureProjection.ersp.headGrid = pr.headGrid(STUDY.measureProjection.option.headGridSpacing);

% do the actual projection 
STUDY.measureProjection.ersp.projection = pr.meanProjection(STUDY.measureProjection.ersp.object,...
STUDY.measureProjection.ersp.object.getPairwiseMutualInformationSimilarity, ...
STUDY.measureProjection.ersp.headGrid, 'numberOfPermutations', ...
STUDY.measureProjection.option.numberOfPermutations, 'stdOfDipoleGaussian',...
STUDY.measureProjection.option.standardDeviationOfEstimatedDipoleLocation,'numberOfStdsToTruncateGaussian',...
STUDY.measureProjection.option.numberOfStandardDeviationsToTruncatedGaussaian, 'normalizeInBrainDipoleDenisty', ...
fastif(STUDY.measureProjection.option.normalizeInBrainDipoleDenisty,'on', 'off'));

% visualize significant voxels individually (voxel p < 0.01)
STUDY.measureProjection.ersp.projection.plotVoxel(0.01);

% visualize significant voxles as a volume (voxel p < 0.01)
STUDY.measureProjection.ersp.projection.plotVolume(0.01);




%% create domains
 
% find out the significance level to use (e.g. corrected by FDR)
if STUDY.measureProjection.option.('erspFdrCorrection')
   significanceLevel = fdr(STUDY.measureProjection.ersp.projection.convergenceSignificance(...
STUDY.measureProjection.ersp.headGrid.insideBrainCube(:)), STUDY.measureProjection.option.(['erspSignificance']));
else
   significanceLevel = STUDY.measureProjection.option.('erspSignificance');
end;

maxDomainExemplarCorrelation = STUDY.measureProjection.option.('erspMaxCorrelation');

% the command below makes the domains using parameters significanceLevel and maxDomainExemplarCorrelation:
STUDY.measureProjection.(measureName).projection = ...
STUDY.measureProjection.(measureName).projection.createDomain(...
STUDY.measureProjection.(measureName).object, maxDomainExemplarCorrelation, significanceLevel);

% visualize domains (change 'voxle' to 'volume' for a different type of visualization) 
STUDY.measureProjection.ersp.projection.plotVoxelColoredByDomain;



%% get dipoles
measureName = 'spec';
domainNumber = 3;

dipoleAndMeasure = STUDY.measureProjection.(measureName).object; % get the ERSP and dipole data (dataAndMeasure object) from the STUDY structure.
domain = STUDY.measureProjection.(measureName).projection.domain(domainNumber); % get the domain in a separate variable
projection  = STUDY.measureProjection.(measureName).projection;
[dipoleId sortedDipoleDensity orderOfDipoles dipoleDenisty dipoleDenistyInRegion] = dipoleAndMeasure.getDipoleDensityContributionToRegionOfInterest(domain.membershipCube, projection, [1 0.05])% the last value, [1 0.05]) indicates that we want all the ICs that at least has a 0.05 chance of being in the domain. You may want to use 0.1 or even 0.5 to get fewer ICs.

STUDY.measureProjection.(measureName).projection.domain(domainNumber).plotDipole(STUDY.measureProjection.(measureName).object);

domainICs = dipoleAndMeasure.createSubsetForId(dipoleId, false); % here we create a new variable that contain information only for dipoles associates with domain ICs.



%% Color by Groups
%STUDY.measureProjection.(measureName).projection.domain(domainNumber).plotDipole(STUDY.measureProjection.(measureName).object);
groups = dipoleAndMeasure.groupNumber(dipoleId);
for i=1:length(groups)
    if(groups(i) == 1)
        colors(i,:) = [1 0 0];
    else
        colors(i,:) = [0 0 1];
    end
end

colorAsCell = {};
for i=1:size(colors, 1)-2
    colorAsCell{i} = colors(i,:);
end;

figure;
plot_dipplot_with_cortex(dipoleAndMeasure.location(dipoleId,:), true, 'coordformat', 'MNI', 'gui', 'off', 'spheres', 'on', 'color', colorAsCell);
           
%% Color by Sessions
% sessions <- datasetId(numberInDatasetId)
sessions = {ALLEEG([dipoleAndMeasure.datasetId(dipoleId)]).session};

%sessions = dipoleAndMeasure.session(dipoleId);
for i=1:length(sessions)
    if(sessions{i} == '1')
        colors(i,:) = [1 0 0];
    elseif(sessions{i} == '2')
        colors(i,:) = [0 1 0];
    else
        colors(i,:) = [0 0 1];
    end
end
            
colorAsCell = {};
for i=1:size(colors, 1)-2
    colorAsCell{i} = colors(i,:);
end;

figure;
plot_dipplot_with_cortex(dipoleAndMeasure.location(dipoleId,:), true, 'coordformat', 'MNI', 'gui', 'off', 'spheres', 'on', 'color', colorAsCell);
            
%% Projection
% do the actual projection 
testProjection = pr.meanProjection(domainICs,...
domainICs.getPairwiseMutualInformationSimilarity, ...
STUDY.measureProjection.ersp.headGrid, 'numberOfPermutations', ...
STUDY.measureProjection.option.numberOfPermutations, 'stdOfDipoleGaussian',...
STUDY.measureProjection.option.standardDeviationOfEstimatedDipoleLocation,'numberOfStdsToTruncateGaussian',...
STUDY.measureProjection.option.numberOfStandardDeviationsToTruncatedGaussaian, 'normalizeInBrainDipoleDenisty', ...
'off');
%fastif(STUDY.measureProjection.option.normalizeInBrainDipoleDenisty,'on', 'off'));

% visualize significant voxels individually (voxel p < 0.01)
testProjection.plotVoxel(0.05);

% visualize significant voxles as a volume (voxel p < 0.01)
testProjection.plotVolume(0.05);



%% extra

domainNumber = 3;
dipoleAndMeasure = STUDY.measureProjection.ersp.object; % get the ERSP and dipole data (dataAndMeasure object) from the STUDY structure.
domain = STUDY.measureProjection.ersp.projection.domain(domainNumber); % get the domain in a separate variable
projection  = STUDY.measureProjection.ersp.projection;
headGrid = STUDY.measureProjection.ersp.headGrid;
[linearProjectedMeasure sessionConditionCell groupId uniqeDatasetId dipoleDensity] = dipoleAndMeasure.getMeanProjectedMeasureForEachSession(headGrid, domain.membershipCube, projection.projectionParameter);


%% Bonus
roiProjection = pr.regionOfInterestProjection(STUDY.measureProjection.ersp.object, STUDY.measureProjection.ersp.object.getPairwiseFishersZSimilarity, pr.headGrid);
roiProjection.makeReport('TestMPTReport','.');


%% Test Me!
%dipoleAndMeasureForDataset = obj.createSubsetForId(obj.datasetId == datasetId, false); % do not re-normalize scalpmap polarities.
%[projectionMatrix dipoleDensityFromTheDataset]= pr.meanProjection.getProjectionMatrix(dipoleAndMeasureForDataset, headGrid, projectionParameter, regionOfInterestCube);
 
dipoleAndMeasureForDataset = dipoleAndMeasure.createSubsetForId(dipoleId, false); % do not re-normalize scalpmap polarities.
                
projectionParameter = STUDY.measureProjection.ersp.projection.projectionParameter;
headGrid = STUDY.measureProjection.ersp.projection.headGrid;
[projectionMatrix dipoleDensityFromTheDataset]= pr.meanProjection.getProjectionMatrix(dipoleAndMeasureForDataset, headGrid, projectionParameter, domain.membershipCube);

projectionFromTheDataset = dipoleAndMeasureForDataset.linearizedMeasure * projectionMatrix;
    
%normalizedDipoleDenisty = bsxfun(@times, dipoleDensityFromTheDataset, 1 ./ sum(dipoleDensityFromTheDataset));
%    linearProjectedMeasure(:,counter) = projectionFromTheDataset  * normalizedDipoleDenisty';
%    counter = counter + 1;


%% Plot Stats
for i = 1:20
    std_plottf(1:size(data{1},1), 1:size(data{1},2), {data{1}(:,:,i)});
end
