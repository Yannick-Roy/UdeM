        function plotConditionDifference(obj, linearProjectedMeasureForCombinedCondition, headGrid, regionOfInterestCube, projectionParameter, twoConditionLabelsForComparison, significanceLevelForConditionDifference, usePositionProjections, statisticsParameter, plottingParameter)
            % plotConditionDifference(linearProjectedMeasureForCombinedCondition, headGrid, regionOfInterestCube, projectionParameter, twoConditionLabelsForComparison, significanceLevelForConditionDifference, statisticsParameter, plottingParameter)
            %
            % plot condition difference between first and second specified conditions (A-B) and
            % mask areas with non-significent differences.
            
            % if no condition label is provided, use the first two (if they exist)
            if nargin < 5 || isempty(twoConditionLabelsForComparison)
                if length(obj.conditionLabel) > 1
                    twoConditionIdsForComparison = [1 2];
                    twoConditionLabelsForComparison = obj.conditionLabel;
                    
                    % when there are more than two conditions present, give a warning
                    if length(obj.conditionLabel) > 2
                        fprintf('Warning: there are more than two conditons present, by default only the difference between the first two is displayed. You can assign another condition pair in the input.\n');
                    end;
                else
                    error('There is only one condition present, at least two conditions are needed for a comparison\n');
                end;
            else % if condition labels for comparison are provided, then find condition ids in obj.conditionLabel that are associated with them.                
                if length(twoConditionLabelsForComparison) < 2
                    error('Number of provided condition labels is less than required two (2).\n');
                end;
                
                % find condition ids in obj.conditionLabel that are associated with provided conditon labels.
                for i=1:2
                    twoConditionIdsForComparison(i) = find(strcmp(obj.conditionLabel, twoConditionLabelsForComparison{i}));
                end;
            end
            
            if nargin < 6
                significanceLevelForConditionDifference = 0.03;                
            end;
            
            if nargin < 7
                usePositionProjections = 'mean'; % default is mean since we currently do not have a good way to incorporate session dipole denisties when 'each' statistics is used.
            end;
            
            if nargin < 8               
                statisticsParameter = {};
            end;
            
            if nargin < 9               
                plottingParameter = {};
            end;
            
            % ================================================
            % YR : Load R Stats.
            loadRStats = 'N';
            loadRStats = input('Do you want to load R Values? Y/N [N]:','s');
            if loadRStats == 'Y' || loadRStats == 'y'
                disp('YR : Loading R Values...');
                load('RVals.mat');
                size(R_Pvals)
                R_Pvals = reshape(R_Pvals, 100, 200)';
            
                R_Pvals(R_Pvals > 0.5) = 0.5;
            
                Pvals = {R_Pvals};  
                title = {'R P Values'};
                obj.plotMeasureAsArrayInCell(Pvals, title, plottingParameter{:}); 
            end
            % !!! DEBUG !!!
            % ================================================
            
            % separate conditions from linearized form
            projectedMeasure = obj.getSeparatedConditionsForLinearizedMeasure(linearProjectedMeasureForCombinedCondition);
            
            % only keep the two conditions to be compared
            projectedMeasure = projectedMeasure(twoConditionIdsForComparison);
            
            % calculate difference statistics for two conditions
            [differenceBetweenConditions significanceValue] = obj.getMeasureDifferenceAcrossConditions(headGrid, regionOfInterestCube, projectionParameter, twoConditionIdsForComparison, usePositionProjections,statisticsParameter{:});
            
            % calculate difference between the two conditions and add as the third measure for
            % plotting
            projectedMeasure{3} = projectedMeasure{1} - projectedMeasure{2};
                        
            % mask insignificent differences
            % -- YR added --
            if loadRStats == 'Y' || loadRStats == 'y'
                disp('YR : Masking with P Values...');
                projectedMeasure{4} = projectedMeasure{3};
                projectedMeasure{4}(R_Pvals > significanceLevelForConditionDifference) = 0;
            end
            % -- /YR added --
            projectedMeasure{3}(significanceValue > significanceLevelForConditionDifference) = 0;
            
            % since when plotting, the mean spectra is automatically added for spectra, we should
            % add the negative of it so it is canccled out.
            if strcmp(obj.measureLabel, 'Spec')
                projectedMeasure{3} = projectedMeasure{3} - obj.specMeanOverAllIcAndCondition;
            end;
            
            % set the titles
            conditionTitle = [obj.conditionLabel{twoConditionIdsForComparison(1)} ' - ' obj.conditionLabel{twoConditionIdsForComparison(2)} ' (p<' num2str(significanceLevelForConditionDifference) ')'];
            
            title = [obj.conditionLabel(twoConditionIdsForComparison), conditionTitle];
            % -- YR added --
            if loadRStats == 'Y' || loadRStats == 'y'
                title = [title, 'R Stats'];
            end
            % -- /YR added --

            % plot
            obj.plotMeasureAsArrayInCell(projectedMeasure, title, plottingParameter{:});
            
            % -- YR added --
            % !!! DEBUG !!!
            if loadRStats == 'Y' || loadRStats == 'y'
                Pvals = {significanceValue significanceValue};  
                title = {'MPT P Values', 'MPT P Values'};
                obj.plotMeasureAsArrayInCell(Pvals, title, plottingParameter{:});               
            end
            % !!! DEBUG !!!
            % -- /YR added --
        end