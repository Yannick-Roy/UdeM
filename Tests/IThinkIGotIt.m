measureName = 'ersp';

%% Create Project 

% read the data (calculated measure, etc.) from STUDY
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


%% Create Domains
 
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

%% Find ICs
domainNumber = 3;
dipoleAndMeasure = STUDY.measureProjection.ersp.object; % get the ERSP and dipole data (dataAndMeasure object) from the STUDY structure.
domain = STUDY.measureProjection.ersp.projection.domain(domainNumber); % get the domain in a separate variable
projection  = STUDY.measureProjection.ersp.projection;
[dipoleId sortedDipoleDensity orderOfDipoles dipoleDenisty dipoleDenistyInRegion] = dipoleAndMeasure.getDipoleDensityContributionToRegionOfInterest(domain.membershipCube, projection, [1 0.05]) %[1 0.5])% the last value, [1 0.05]) indicates that we want all the ICs that at least has a 0.05 chance of being in the domain. You may want to use 0.1 or even 0.5 to get fewer ICs.

figure;
plot_dipplot_with_cortex(dipoleAndMeasure.location(dipoleId,:), true, 'coordformat', 'MNI', 'gui', 'off', 'spheres', 'on');
            
domainICs = dipoleAndMeasure.createSubsetForId(dipoleId); % here we create a new variable that contain information only for dipoles associates with domain ICs.


%% Test
%STUDY.measureProjection.(measureName).headGrid = pr.headGrid(STUDY.measureProjection.option.headGridSpacing);
%testProjection = pr.meanProjection(dipoleAndMeasure, dipoleAndMeasure.getPairwiseMutualInformationSimilarity, STUDY.measureProjection.(measureName).headGrid, 'numberOfPermutations', STUDY.measureProjection.option.numberOfPermutations, 'stdOfDipoleGaussian', STUDY.measureProjection.option.standardDeviationOfEstimatedDipoleLocation,'numberOfStdsToTruncateGaussian', STUDY.measureProjection.option.numberOfStandardDeviationsToTruncatedGaussaian, 'normalizeInBrainDipoleDenisty', fastif(STUDY.measureProjection.option.normalizeInBrainDipoleDenisty,'on', 'off'));
testProjection = pr.meanProjection(domainICs, domainICs.getPairwiseMutualInformationSimilarity, STUDY.measureProjection.(measureName).headGrid, 'numberOfPermutations', STUDY.measureProjection.option.numberOfPermutations, 'stdOfDipoleGaussian', STUDY.measureProjection.option.standardDeviationOfEstimatedDipoleLocation,'numberOfStdsToTruncateGaussian', STUDY.measureProjection.option.numberOfStandardDeviationsToTruncatedGaussaian, 'normalizeInBrainDipoleDenisty', fastif(STUDY.measureProjection.option.normalizeInBrainDipoleDenisty,'on', 'off'));

%STUDY = place_components_for_projection_of_measure(STUDY, measureName);

% put the STUDY variable back from workspace
%assignin('base', 'STUDY', STUDY);


% show significant areas as volume.
%significanceLevel = getVoxelSignificance(STUDY, measureName);
%significanceLevel = fdr(STUDY.measureProjection.(measureName).projection.convergenceSignificance(STUDY.measureProjection.(measureName).headGrid.insideBrainCube(:)), STUDY.measureProjection.option.([measureName 'Significance']));
    
%significanceLevel = STUDY.measureProjection.option.([measureName 'Significance']);

testProjection.plotVoxel(0.01);
testProjection.plotVolume(0.01);