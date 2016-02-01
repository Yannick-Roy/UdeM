i = 1;
%%

% Most contributing dipole. (# in domain dipoles...)
[dipoleId sortedDipoleDensity orderOfDipoles dipoleDenisty dipoleDenistyInRegion] = STUDY.measureProjection.ersp.projection.domain(3).dipoleAndMeasure.getDipoleDensityContributionToRegionOfInterest(STUDY.measureProjection.ersp.projection.domain(3).membershipCube, STUDY.measureProjection.ersp.projection.domain(3), [1 0.05]);
contributingDipoles = dipoleId; %unique(STUDY.measureProjection.ersp.projection.domain(1,3).dipoleAndMeasure.icIndexForEachDipole(tmpDipoles))

names = STUDY.measureProjection.ersp.projection.domain(1,3).dipoleAndMeasure.subjectName(contributingDipoles);
ids = STUDY.measureProjection.ersp.projection.domain(1,3).dipoleAndMeasure.numberInDataset(contributingDipoles);









for dataSet = 1:90
    i = dataSet * 4;
    subjectName = STUDY.datasetinfo(1,i).subject;
    
    indice = find(cellfun('length',regexp(names,subjectName)));
    if ~isempty(indice)            
        disp(subjectName);
        disp(ids(indice));    
        
        dipolesD3(i).measures = ids(indice);
    end
end




locations = [];
for dataSet = 1:90
    i = dataSet * 4;
    
   %tmpLocations = STUDY.measureProjection.ersp.projection.domain(1,3).dipoleAndMeasure.location(dipolesD3(i).measures,:)
   if ~isempty(tmpLocations)
    locations = [locations; tmpLocations];   
   end
    
    %pause(1);
end
figure;
plot_dipplot_with_cortex(locations, false, 'coordformat',  'mni', 'spheres', 'on', 'gui','off');


















for dataSet = 1:90
    i = dataSet * 4;
    
    %dipolesD1(i).measures = SIFT_GetDipolesFromMPT(STUDY, EEG, STUDY.datasetinfo(1,i).index, STUDY.datasetinfo(1,i).subject, 1);

    %dipolesD2(i).measures = SIFT_GetDipolesFromMPT(STUDY, EEG, STUDY.datasetinfo(1,i).index, STUDY.datasetinfo(1,i).subject, 2);

    %dipolesD3(i).measures = SIFT_GetDipolesFromMPT(STUDY, EEG, STUDY.datasetinfo(1,i).index, STUDY.datasetinfo(1,i).subject, 3);
    
    tmp = [];
    subjectName = STUDY.datasetinfo(1,i).subject;
    
    for j = 1:length(contributingDipoles)
        %disp(contributingDipoles(j));
        %disp('...');
        if strcmp(STUDY.measureProjection.ersp.projection.domain(1,3).dipoleAndMeasure.subjectName(contributingDipoles(j)), subjectName)          
            subjectName
            STUDY.measureProjection.ersp.projection.domain(1,3).dipoleAndMeasure.numberInDataset(contributingDipoles(j))
        end        
    end
    dipolesD3(i).measures = tmp;
    
end;


%%
tmp = [];
subjectName = '2_3-07';

% Most contributing dipole. (# in domain dipoles...)
contributingDipoles = STUDY.measureProjection.ersp.projection.domain(3).dipoleAndMeasure.getDipoleDensityContributionToRegionOfInterest(STUDY.measureProjection.ersp.projection.domain(3).membershipCube, STUDY.measureProjection.ersp.projection.domain(3), [1 0.05]);

for i = 1:length(contributingDipoles)
    if strcmp(STUDY.measureProjection.ersp.projection.domain(1,3).dipoleAndMeasure.subjectName(contributingDipoles(i)), subjectName)
        tmp = [tmp STUDY.measureProjection.ersp.projection.domain(1,3).dipoleAndMeasure.numberInDataset(contributingDipoles(i))]
    end
end

% Need to find the # in the dataset!
names = STUDY.measureProjection.ersp.projection.domain(1,3).dipoleAndMeasure.subjectName(contributingDipoles);
tmp = STUDY.measureProjection.ersp.projection.domain(1,3).dipoleAndMeasure.numberInDataset((find(cellfun('length',regexp(names,subjectName)) == 1)));                


%figure;
%i = 1;
%plot_dipplot_with_cortex(STUDY.measureProjection.ersp.projection.domain(1,1).dipoleAndMeasure.location(dipolesD1(i).measures,:), true, 'coordformat',  'mni', 'spheres', 'on', 'gui', 'off');

locations = [];
for dataSet = 1:90
    i = dataSet * 4;
    
   tmpLocations = STUDY.measureProjection.ersp.projection.domain(1,3).dipoleAndMeasure.location(dipolesD3(i).measures,:)
   if ~isempty(tmpLocations)
    locations = [locations; tmpLocations];   
   end
    
    %pause(1);
end
figure;
plot_dipplot_with_cortex(locations, false, 'coordformat',  'mni', 'spheres', 'on', 'gui','off');

