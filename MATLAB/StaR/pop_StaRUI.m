% pop_StarUI(): UI to help getting the stats from MPT -> R -> MPT.
%       

% Author: Yannick Roy, UdeM
% 
% === HISTORY ===
% 06/20/2016 ver 0.01 by Yannick. Creation of the Dialog Box (UI).
% === /HISTORY ===
%
% === KNOW ISSUES & TODO ===
% Fixed designs!
% Would work with different conditions.
% === /KNOW ISSUES ===
%
% === NOTES ===
%
% The following code must be added to create_mpt_submenu.m  
% (~ line 300 in 14.4.4b)
%
% % % function command_measure_show_domain_star(callerHandle, evnt, measureName)
% % %     tagForHandle = get(gcbo, 'tag');
% % %     if ischar(tagForHandle(end)) && ~isempty(tagForHandle(end))
% % %         domainIndex = str2num(tagForHandle(end));
% % %         STUDY = evalin('base', 'STUDY;');
% % %         if isfield(STUDY, 'measureProjection') && isfield(STUDY.measureProjection, 'option') && isfield(STUDY.measureProjection.option, 'groupDifferenceOption') && ~isempty(STUDY.measureProjection.option.groupDifferenceOption)
% % %             STUDY.measureProjection.option.groupDifferenceOption = STUDY.measureProjection.(measureName).projection.domain(domainIndex).plotGroupDifferenceGui(STUDY.measureProjection.(measureName).object, STUDY.measureProjection.option.groupDifferenceOption);
% % %         else
% % %             STUDY.measureProjection.option.groupDifferenceOption = STUDY.measureProjection.(measureName).projection.domain(domainIndex).plotGroupDifferenceGui(STUDY.measureProjection.(measureName).object);
% % %         end;
% % %         
% % %         % put the STUDY variable back from workspace
% % %         assignin('base', 'STUDY', STUDY);
% % %     else
% % %         return;
% % %     end   
% % % end
%
% The following code must also be added to create_mpt_submenu.m
%
% % % uimenu(domain_menu,'Label', 'Show StaR','Callback',{@command_measure_show_domain_star, measureString},'tag',[measureString 'StaRForDomain ' num2str(j)],'separator','on', 'userdata', 'study:on');


%function STUDY = pop_StaRUI(STUDY, ALLEEG, obj, curDomain)
function STUDY = pop_StaRUI(varargin)
com = '';

if ~isstr(varargin{1}) %intial settings
    if length(varargin) < 2
        help pop_StaRUI;
        return;
    end
    STUDY  = varargin{1};
    ALLEEG = varargin{2};

    disp(' -- Entering: pop_StaRUI() --');

    % Callbacks
    generate_plot_cb = ['pop_StaRUI(''btn_generate'',gcf);']; 
    check_compute_cb = ['pop_StaRUI(''check_compute'',gcf);']; 
    cbo_fixvar1_cb = ['pop_StaRUI(''cbo_fixvar1'',gcf);']; 
    cbo_fixvar2_cb = ['pop_StaRUI(''cbo_fixvar2'',gcf);']; 
    cbo_fixvar1val_cb = ['pop_StaRUI(''cbo_fixvar1val'',gcf);']; 
    cbo_fixvar2val_cb = ['pop_StaRUI(''cbo_fixvar2val'',gcf);']; 

    % Design Names...
    designs = {};
    designs{1}.No = 1;
    designs{1}.Name  = ['Design #' num2str(designs{1}.No) ': (conditions)'];
    designs{2}.No = 2;
    designs{2}.Name  = ['Design #' num2str(designs{2}.No) ': (sessions)'];
    designs{3}.No = 3;
    designs{3}.Name  = ['Design #' num2str(designs{3}.No) ': (motions)'];
    designs{4}.No = 4;
    designs{4}.Name  = ['Design #' num2str(designs{4}.No) ': (orders)'];
    designs{5}.No = 5;
    designs{5}.Name  = ['Design #' num2str(designs{5}.No) ': (groups)'];
    designs{6}.No = 11;
    designs{6}.Name  = ['Design #' num2str(designs{6}.No) ': (groups * conditions)'];
    designs{7}.No = 12;
    designs{7}.Name  = ['Design #' num2str(designs{7}.No) ': (groups * sessions)'];
    designs{8}.No = 13;
    designs{8}.Name  = ['Design #' num2str(designs{8}.No) ': (groups * motions)'];
    designs{9}.No = 14;
    designs{9}.Name  = ['Design #' num2str(designs{9}.No) ': (groups * orders)'];
    designs{10}.No = 15;
    designs{10}.Name  = ['Design #' num2str(designs{10}.No) ': (sessions * motions)'];
    designs{11}.No = 16;
    designs{11}.Name  = ['Design #' num2str(designs{11}.No) ': (sessions * orders)'];
    designs{12}.No = 17;
    designs{12}.Name  = ['Design #' num2str(designs{12}.No) ': (groups * sessions * motions)'];
    designs{13}.No = 18;
    designs{13}.Name  = ['Design #' num2str(designs{13}.No) ': (groups * sessions * orders)'];
    designs{14}.No = 19;
    designs{14}.Name  = ['Design #' num2str(designs{14}.No) ': (groups * sessions * motions * orders)'];

    % Stats Type.
    stats = {};
    stats{1}.function = 'aov';
    stats{1}.details = '(anova)';
    stats{2}.function = 'lme';
    stats{2}.details = '(mixed models)';

    % Correction Type.
    corrections = {};
    corrections{1} = 'N/A';
    corrections{2} = 'FDR';
    corrections{3} = 'Bonferroni';
    
    variables = {};
    variables{1}.name = 'N/A';
    variables{1}.values = {};
    variables{1}.values{1} = 'N/A';
    variables{2}.name = 'group';
    variables{2}.values = {};
    variables{2}.values{1} = '1';
    variables{2}.values{2} = '2';
    variables{3}.name = 'session';
    variables{3}.values = {};
    variables{3}.values{1} = '1';
    variables{3}.values{2} = '2';
    variables{3}.values{3} = '3';
    variables{4}.name = 'order';
    variables{4}.values = {};
    variables{4}.values{1} = 'FO';
    variables{4}.values{2} = 'SO';
    variables{5}.name = 'motion';
    variables{5}.values = {};
    variables{5}.values{1} = 'F';
    variables{5}.values{2} = 'M';
    
    % create domains string
    designString = '';
    if length(designs)>1
        for n = 1:length(designs)
            if isempty(designString)
                designString = designs{n}.Name;
            else
                designString = [designString '|'  designs{n}.Name];
            end
        end
    else
        designString = 'N/A';
    end

    % groups in the STUDY (not checking the current design...)
    statsString = '';
    if length(stats)>1
        for n = 1:length(stats)
            if isempty(statsString)
                statsString = [stats{n}.function ' ' stats{n}.details];
            else
                statsString = [statsString '|'  [stats{n}.function ' ' stats{n}.details]];
            end
        end
    else
        statsString = 'N/A';
    end

    correctionString = '';
    if length(corrections)>1
        for n = 1:length(corrections)
            if isempty(correctionString)
                correctionString = corrections{n};
            else
                correctionString = [correctionString '|'  corrections{n}];
            end
        end
    else
        correctionString = 'N/A';
    end
    
    variablesString = '';
    if length(variables)>1
        for n = 1:length(variables)
            if isempty(variablesString)
                variablesString = variables{n}.name;
            else
                variablesString = [variablesString '|'  variables{n}.name];
            end
        end
    else
        variablesString = 'N/A';
    end
    
    vals = {'Val1', 'Val2', 'Val3'};
    variablesValString = 'All';
    if length(vals)>1
        for n = 1:length(vals)
            if isempty(variablesValString)
                variablesValString = vals{n};
            else
                variablesValString = [variablesValString '|'  vals{n}];
            end
        end
    else
        variablesValString = 'N/A';
    end
    
    nbPlots = 6;
    nbPlotsValid = 1;
    nbSessions = length(STUDY.session);
    nbGroups = length(STUDY.group);

    statsThreshold = 0.05;
    statsCompute = false;

    % collect user input
    try
        nRows = 13;
        nCols = 5;
       userInput = inputgui('title', 'pop_StaRUI()', 'geom', ...
           {{nCols nRows [0 0] [1 1]} {nCols nRows [1 0] [4 1]} ...
            {nCols nRows [0 1] [1 1]} ... % Empty Line
            {nCols nRows [0 2] [1 1]} ... % Title Line
            {nCols nRows [0 3] [1 1]} {nCols nRows [1 3] [2 1]} {nCols nRows [3 3] [1 1]} {nCols nRows [4 3] [1 1]} ...
            {nCols nRows [0 4] [1 1]} {nCols nRows [1 4] [1 1]} ...
            {nCols nRows [0 5] [1 1]} {nCols nRows [1 5] [1 1]} ...
            {nCols nRows [0 6] [1 1]} ... % Empty Line
            {nCols nRows [0 7] [1 1]} {nCols nRows [2 7] [1 1]} {nCols nRows [3 7] [1 1]} ... % Title Line
            {nCols nRows [1 8] [1 1]} {nCols nRows [2 8] [1 1]} ... % Var 1
            {nCols nRows [1 9] [1 1]} {nCols nRows [2 9] [1 1]} ... % Var 2
            {nCols nRows [1 10] [1 1]} {nCols nRows [2 10] [1 1]} {nCols nRows [3 10] [1 1]} ... % Fix Var 1
            {nCols nRows [1 11] [1 1]} {nCols nRows [2 11] [1 1]} {nCols nRows [3 11] [1 1]} ... % Fix Var 2
            {nCols nRows [0 12] [1 1]} ... % Empty Line
            {nCols nRows [2 13] [1 2]} ... % Generate button
            {nCols nRows [0 14] [1 1]} ... % Empty Line
            }, ... 
        'uilist',...
           {{'style' 'text' 'string' ' Design : '}  {'style' 'popupmenu' 'string' designString 'tag' 'design' 'value' 1} ...
            {} ... 
            {'style' 'text' 'string' ' Stats' 'FontWeight' 'Bold'} ...
            {'style' 'text' 'string' ' Stats : '}  {'style' 'popupmenu' 'string' statsString 'tag' 'stats' 'value' 1} {'style' 'text' 'string' ' Signif. Threshold : '}  {'style' 'edit' 'string' statsThreshold 'tag' 'threshold' 'userdata' 'eeglab' } ...
            {'style' 'text' 'string' ' Correction : '}  {'style' 'popupmenu' 'string' correctionString 'tag' 'correction' 'value' 1} ...
            {'style' 'text' 'string' ' Compute Stats : '} {'style' 'checkbox' 'string' '' 'tag' 'compute' 'value' statsCompute 'callback' check_compute_cb} , ... ...
            {} ...
            {'style' 'text' 'string' ' Plot & Variables' 'FontWeight' 'Bold'} {'style' 'text' 'string' ' Name'} {'style' 'text' 'string' ' Value'} ...
            {'style' 'text' 'string' ' Variable 1 : '}  {'style' 'popupmenu' 'string' variablesString 'tag' 'var1' 'value' 2} ...
            {'style' 'text' 'string' ' Variable 2 : '}  {'style' 'popupmenu' 'string' variablesString 'tag' 'var2' 'value' 3} ...
            {'style' 'text' 'string' ' [FIXED] Variable 1 : '}  {'style' 'popupmenu' 'string' variablesString 'tag' 'fixvar1' 'value' 1 'callback' cbo_fixvar1_cb} {'style' 'popupmenu' 'string' variablesValString 'tag' 'fixvar1val' 'value' 1 'callback' cbo_fixvar1val_cb} ...
            {'style' 'text' 'string' ' [FIXED] Variable 2 : '}  {'style' 'popupmenu' 'string' variablesString 'tag' 'fixvar2' 'value' 1 'callback' cbo_fixvar2_cb} {'style' 'popupmenu' 'string' variablesValString 'tag' 'fixvar2val' 'value' 1 'callback' cbo_fixvar2val_cb} ...
            {} ...
            {'style' 'pushbutton' 'string' 'Generate' 'tag' 'generate' 'callback' generate_plot_cb} ...
            {} ...
            }, 'userdata', nbPlots);
    catch
        disp('## ERROR ## pop_StaRUI() cant load...');
    end

    % canceled
    if isempty(userInput)
        return
    end

    % Store userInput to STUDY (MPT.StaR)
    STUDY.measureProjection.StaR = userInput;

    % Get User Inputs.
    StaRParams.design           = userInput{1,1};
    StaRParams.stats            = userInput{1,2};
    StaRParams.threshold        = userInput{1,3};
    StaRParams.correction       = userInput{1,4};
    StaRParams.compute          = userInput{1,5};
    
    StaRParams.vars{1}.name_ind         = userInput{1,6};
    StaRParams.vars{2}.name_ind         = userInput{1,7};
    StaRParams.fixvars{1}.name_ind      = userInput{1,8};
    StaRParams.fixvars{1}.val_ind       = userInput{1,9};
    StaRParams.fixvars{2}.name_ind      = userInput{1,10};
    StaRParams.fixvars{2}.val_ind       = userInput{1,11};
    
    StaRParams.vars{1}.name = variables{StaRParams.vars{1}.name_ind}.name;
    StaRParams.vars{2}.name = variables{StaRParams.vars{2}.name_ind}.name;
    
    StaRParams.fixvars{1}.name = variables{StaRParams.fixvars{1}.name_ind}.name;
    StaRParams.fixvars{1}.value = vals{StaRParams.fixvars{1}.val_ind};
    StaRParams.fixvars{2}.name = variables{StaRParams.fixvars{2}.name_ind}.name;
    StaRParams.fixvars{2}.value = vals{StaRParams.fixvars{2}.val_ind};
    
    % Get params and send them over to R in a 'StaR' protocol!
    disp('Saving... ');
    fileTestName = 'testParamsR.mat';
    disp(fileTestName);
    
    starDesign = StaRParams.design;
    starStats = StaRParams.stats;
    starThreshold = StaRParams.threshold;
    starCorrection = StaRParams.correction;
    starVars = StaRParams.vars;
    starFixVars = StaRParams.fixvars;
    save(fileTestName, 'starDesign', 'starStats', 'starThreshold', 'starCorrection', 'starVars', 'starFixVars');
    
    disp('Saved!');
    
    % -- Debug Disp --
    disp(['Design: ' designs{StaRParams.design}.Name]);
    disp(['Stats: ' stats{StaRParams.stats}.function stats{StaRParams.stats}.details]);
    disp(['Correction: ' corrections{StaRParams.correction}]);
    disp(['Signif. Threshold: ' StaRParams.threshold]);
    disp(['Compute? ' num2str(StaRParams.compute)]);
    disp('--------------');
    disp(['Var1 - ' StaRParams.vars{1}.name]);
    disp(['Var2 - ' StaRParams.vars{2}.name]);
    disp(['Fix Var1 - ' StaRParams.fixvars{1}.name]);
    disp(['Fix Var1 Val - ' StaRParams.fixvars{1}.value]);
    disp(['Fix Var2 - ' StaRParams.fixvars{2}.name]);
    disp(['Fix Var2 Val - ' StaRParams.fixvars{2}.value]);
    % -- Debug Disp --

    if StaRParams.compute

        % Launch R with Params.

        % ToDo : In Progress Wait Mechanism!!!

        disp('Do Me ! ... Stats in R ...');

    end

    % Load from Files.
    disp(['Doing: ' obj.object.measureLabel ' Domain #' num2str(curDomain)]); 


    % Show Stats! ---- The Good Stuff Happens Here! ---- 
    strStatsPath = ['/Users/yannick/Documents/PhD/Stats Test/mTBI_SubClean_Measures/MPT_Export/Star Images/Latest/' obj.object.measureLabel '/Domain_' num2str(curDomain) '/Stats_' stats{StaRParams.stats}.function '/Design_' obj.object.measureLabel '_' num2str(designs{StaRParams.design}.No)];
    disp(strStatsPath);
    if exist(strStatsPath, 'dir')
        disp('Valid Path!');
    else
        disp('INVALID PATH!');
    end

    %domainObj = obj.projection.domain(curDomain);
    %plotStaR(domainObj.dipoleAndMeasure, domainObj.meanLinearizedProjectedMeasure, domainObj.headGrid, domainObj.membershipCube, domainObj.projectionParameter, {'FOF', 'FOM'}, 0.05, 'mean'); % inputOptions.statisticsParameter, inputOptions.plottingParameter);

    disp(' -- Leaving: pop_StaRUI() --');            
        
else
    % varargin 1 - The string you pick (for the switch)
    % varargin 2 - userdata sent to inputui() (multiple variables)
    userdat = get(varargin{2}, 'userdata');    
        
    switch  varargin{1}
        case 'btn_generate'
            %if statsCompute
            if userdat == 5
                disp('YES!');
            else
                disp('NO :(');
            end
        case 'check_compute'
            checkcompute = get(findobj('parent', hdl, 'tag', 'compute'), 'value')
        case 'cbo_fixvar1'
            disp('Fix Var 1');
        case 'cbo_fixvar2'
            disp('Fix Var 2');
        case 'cbo_fixvar1val'
            disp('Fix Var 1 Val');
        case 'cbo_fixvar2val'
            disp('Fix Var 2 Val');
    end
                
end

function plotStaR(obj, linearProjectedMeasureForCombinedCondition, headGrid, regionOfInterestCube, projectionParameter, twoConditionLabelsForComparison, significanceLevelForConditionDifference, usePositionProjections, statisticsParameter, plottingParameter)
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

    %if nargin < 8               
        statisticsParameter = {};
    %end;

    %if nargin < 9               
        plottingParameter = {};
    %end;

    % ================================================
    % YR : Load R Stats.
    loadRStats = 'N';
    %loadRStats = input('Do you want to load R Values? Y/N [N]:','s');
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
end
% Show Stats! ---- The Good Stuff Happens Here! ---- 
