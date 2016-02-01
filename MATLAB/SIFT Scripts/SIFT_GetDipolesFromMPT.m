function [dipoles] = SIFT_GetDipolesFromMPT(STUDY, EEG, id, subjectName, domainNo)

    % For all the measures (ERP, ERSPs, etc)
    dipoles = [];
    dipERP = [];
    dipERSP = [];
    
    disp('==== ERP Start =====');
    % For all the domains
    
    if 1
        for j = 1:size(STUDY.measureProjection.erp.projection.domain, 2)
            % Most contributing dipole. (# in domain dipoles...)
            [dipoleId sortedDipoleDensity orderOfDipoles dipoleDenisty dipoleDenistyInRegion] = STUDY.measureProjection.erp.projection.domain(j).dipoleAndMeasure.getDipoleDensityContributionToRegionOfInterest(STUDY.measureProjection.erp.projection.domain(j).membershipCube, STUDY.measureProjection.erp.projection.domain(j), [1 0.05]);
            contributingDipoles = dipoleId; %unique(STUDY.measureProjection.ersp.projection.domain(1,3).dipoleAndMeasure.icIndexForEachDipole(tmpDipoles))

            names = STUDY.measureProjection.erp.projection.domain(1,j).dipoleAndMeasure.subjectName(contributingDipoles);
            ids = STUDY.measureProjection.erp.projection.domain(1,j).dipoleAndMeasure.numberInDataset(contributingDipoles);

            indice = find(cellfun('length',regexp(names,subjectName)));
            if ~isempty(indice)            
                disp(subjectName);
                disp(ids(indice)); 
                %nbDipoles = nbDipoles + length(ids(indice));

                dipERP = [dipERP unique(ids(indice))];      
            end
        end
    end
    
%     if 0
%         for i = 1:size(STUDY.measureProjection.erp.projection.domain, 2)
%             domainName = STUDY.measureProjection.erp.projection.domain(1,i).label;
%             disp(['Doing ERP - ' domainName '...'])
% 
%             % Trouver avec le ID du dataset ca fail, pcq c'est group� par
%             % Subject ID il semblerait!
%             %condInd = find(~cellfun(@isempty, strfind(STUDY.measureProjection.erp.projection.domain(1,i).dipoleAndMeasure.conditionLabel, condition)))
%             %tmp = STUDY.measureProjection.erp.projection.domain(1,1).dipoleAndMeasure.numberInDataset(STUDY.measureProjection.erp.projection.domain(1,i).dipoleAndMeasure.subjectName == subjectName)
%             %tmp = STUDY.measureProjection.erp.projection.domain(1,1).dipoleAndMeasure.numberInDataset(find(cellfun('length',regexp(STUDY.measureProjection.erp.projection.domain(1,i).dipoleAndMeasure.subjectName,'1_4-02')) == 1))
% 
%             % Most contributing dipole. (# in domain dipoles...)
%             contributingDipoles = STUDY.measureProjection.erp.projection.domain(i).dipoleAndMeasure.getDipoleDensityContributionToRegionOfInterest(STUDY.measureProjection.erp.projection.domain(i).membershipCube, STUDY.measureProjection.erp.projection.domain(i), [1 0.05]);
% 
%             % Need to find the # in the dataset!
%             names = STUDY.measureProjection.erp.projection.domain(1,i).dipoleAndMeasure.subjectName(contributingDipoles);
%             tmp = STUDY.measureProjection.erp.projection.domain(1,i).dipoleAndMeasure.numberInDataset((find(cellfun('length',regexp(names,subjectName)) == 1)));
% 
%             if ~isempty(tmp)
%                 disp(['Dipoles found in ' domainName ' for : ' subjectName])
%                 disp(tmp);
% 
%                 dipERP = [dipERP tmp];
%             else
%                 disp(['No dipole found in ' domainName ' for : ' subjectName])
%             end
%             % ---- CONDITIONS ??!!
%             % STUDY.measureProjection.erp.projection.domain(1,i).dipoleAndMeasure.datasetIdAllConditions
%         end
%     end    
    dipERP = unique(dipERP)
    disp('==== ERP End =====');

    disp('==== ERSP Start =====');
    % For all the domains    
    
    if 0
        for j = 1:size(STUDY.measureProjection.ersp.projection.domain, 2)
            % Most contributing dipole. (# in domain dipoles...)
            [dipoleId sortedDipoleDensity orderOfDipoles dipoleDenisty dipoleDenistyInRegion] = STUDY.measureProjection.ersp.projection.domain(j).dipoleAndMeasure.getDipoleDensityContributionToRegionOfInterest(STUDY.measureProjection.ersp.projection.domain(j).membershipCube, STUDY.measureProjection.ersp.projection.domain(j), [1 0.05]);
            contributingDipoles = dipoleId; %unique(STUDY.measureProjection.ersp.projection.domain(1,3).dipoleAndMeasure.icIndexForEachDipole(tmpDipoles))

            names = STUDY.measureProjection.ersp.projection.domain(1,j).dipoleAndMeasure.subjectName(contributingDipoles);
            ids = STUDY.measureProjection.ersp.projection.domain(1,j).dipoleAndMeasure.numberInDataset(contributingDipoles);

            indice = find(cellfun('length',regexp(names,subjectName)));
            if ~isempty(indice)            
                disp(subjectName);
                disp(ids(indice)); 
                %nbDipoles = nbDipoles + length(ids(indice));

                dipERSP = [dipERSP unique(ids(indice))];      
            end
        end
    end
    
%     %for i = 1:size(STUDY.measureProjection.ersp.projection.domain, 2)
%     i = domainNo;
%         domainName = STUDY.measureProjection.ersp.projection.domain(1,i).label;
%         disp(['Doing ERSP - ' STUDY.measureProjection.ersp.projection.domain(1,i).label '...'])
%         
%         %condInd = find(~cellfun(@isempty, strfind(STUDY.measureProjection.ersp.projection.domain(1,i).dipoleAndMeasure.conditionLabel, condition)));
%         %tmp = STUDY.measureProjection.ersp.projection.domain(1,1).dipoleAndMeasure.numberInDataset(STUDY.measureProjection.ersp.projection.domain(1,i).dipoleAndMeasure.subjectName == subjectName);
%         %tmp = STUDY.measureProjection.ersp.projection.domain(1,1).dipoleAndMeasure.numberInDataset(find(cellfun('length',regexp(STUDY.measureProjection.ersp.projection.domain(1,i).dipoleAndMeasure.subjectName,'1_4-02')) == 1))
%         %tmp = STUDY.measureProjection.ersp.projection.domain(i).dipoleAndMeasure.getDipoleDensityContributionToRegionOfInterest(STUDY.measureProjection.erp.projection.domain(i).membershipCube, STUDY.measureProjection.ersp.projection.domain(i), [1 0.05]);
%         
%         % Most contributing dipole. (# in domain dipoles...)
%         contributingDipoles = STUDY.measureProjection.ersp.projection.domain(i).dipoleAndMeasure.getDipoleDensityContributionToRegionOfInterest(STUDY.measureProjection.ersp.projection.domain(i).membershipCube, STUDY.measureProjection.ersp.projection.domain(i), [1 0.05]);
%         
%         % Need to find the # in the dataset!
%         names = STUDY.measureProjection.ersp.projection.domain(1,i).dipoleAndMeasure.subjectName(contributingDipoles);
%         tmp = STUDY.measureProjection.ersp.projection.domain(1,i).dipoleAndMeasure.numberInDataset((find(cellfun('length',regexp(names,subjectName)) == 1)));                
%         
%         if ~isempty(tmp)
%             disp(['Dipoles found in ' domainName ' for : ' subjectName])
%             disp(tmp);
%             
%             dipERSP = [dipERSP tmp];
%         else
%             disp(['No dipole found in ' domainName ' for : ' subjectName])
%         end
%     %end
    disp('==== ERSP End =====');
    dipERSP = unique(dipERSP)


    % Associated dipoles? (for this dataset)
    dipoles = [dipERP dipERSP];
end



%%
% Debug stuff
% Plot dipole #5 in domain #3...
% figure;
% plot_dipplot_with_cortex(STUDY.measureProjection.ersp.projection.domain(1,3).dipoleAndMeasure.location(5,:), true, 'coordformat',  'mni', 'spheres', 'on', 'gui', 'off');
