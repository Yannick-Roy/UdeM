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
% % %         ALLEEG = evalin('base', 'ALLEEG;');
% % %         
% % %         if isfield(STUDY, 'measureProjection')
% % %             %STUDY.measureProjection.option.groupDifferenceOption = STUDY.measureProjection.(measureName).projection.domain(domainIndex).plotGroupDifferenceGui(STUDY.measureProjection.(measureName).object, STUDY.measureProjection.option.groupDifferenceOption);
% % %             % Send domain obj. You can retrieve the Dipole object from the domain.
% % %             STUDY.measureProjection.(measureName).projection.domain(domainIndex)
% % %             study_local = pop_StaRUI(STUDY, ALLEEG, STUDY.measureProjection.(measureName), domainIndex); 
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
    obj = varargin{3};
    domainID = varargin{4};

    disp(' -- Entering: pop_StaRUI() --');

    % Callbacks
    cbo_design_cb = ['pop_StaRUI(''cbo_design'',gcf);']; 
    generatesub_plot_cb = ['pop_StaRUI(''btn_generatesub'',gcf);'];
    generatefull_plot_cb = ['pop_StaRUI(''btn_generatefull'',gcf);'];
    check_compute_cb = ['pop_StaRUI(''check_compute'',gcf);']; 
    cbo_var1_cb = ['pop_StaRUI(''cbo_var1'',gcf);']; 
    cbo_var2_cb = ['pop_StaRUI(''cbo_var2'',gcf);']; 
    cbo_var3_cb = ['pop_StaRUI(''cbo_var3'',gcf);']; 
    cbo_var4_cb = ['pop_StaRUI(''cbo_var4'',gcf);']; 
    cbo_var1val_cb = ['pop_StaRUI(''cbo_var1val'',gcf);']; 
    cbo_var2val_cb = ['pop_StaRUI(''cbo_var2val'',gcf);']; 
    cbo_var3val_cb = ['pop_StaRUI(''cbo_var3val'',gcf);']; 
    cbo_var4val_cb = ['pop_StaRUI(''cbo_var4val'',gcf);']; 

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
    variables{2}.name = 'groups';
    variables{2}.values = {};
    variables{2}.values{1} = '3';
    variables{2}.values{2} = '4';
    variables{2}.values{3} = 'All';
    variables{3}.name = 'sessions';
    variables{3}.values = {};
    variables{3}.values{1} = '1';
    variables{3}.values{2} = '2';
    variables{3}.values{3} = '3';
    variables{3}.values{4} = 'All';
    variables{4}.name = 'orders';
    variables{4}.values = {};
    variables{4}.values{1} = 'FO';
    variables{4}.values{2} = 'SO';
    variables{4}.values{3} = 'All';
    variables{5}.name = 'motions';
    variables{5}.values = {};
    variables{5}.values{1} = 'F';
    variables{5}.values{2} = 'M';
    variables{5}.values{3} = 'All';
    variables{6}.name = 'conditions';
    variables{6}.values = {};
    variables{6}.values{1} = 'FOF';
    variables{6}.values{2} = 'FOM';
    variables{6}.values{3} = 'SOF';
    variables{6}.values{4} = 'SOM';
    variables{6}.values{5} = 'All';
    
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
    
    variablesString = 'N/A';
    variablesValString = 'N/A';
    
    nbPlots = 6;
    nbPlotsValid = 1;
    nbSessions = length(STUDY.session);
    nbGroups = length(STUDY.group);

    statsThreshold = 0.05;
    statsCompute = false;
    
    uDataVariables = {obj domainID variables designs stats}
    
    % collect user input
    try
        nRows = 15;
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
            {nCols nRows [1 8] [1 1]} {nCols nRows [2 8] [1 1]} {nCols nRows [3 8] [1 1]} ... % Var 1
            {nCols nRows [1 9] [1 1]} {nCols nRows [2 9] [1 1]} {nCols nRows [3 9] [1 1]} ... % Var 2
            {nCols nRows [1 10] [1 1]} {nCols nRows [2 10] [1 1]} {nCols nRows [3 10] [1 1]} ... % Var 3
            {nCols nRows [1 11] [1 1]} {nCols nRows [2 11] [1 1]} {nCols nRows [3 11] [1 1]} ... % Var 4
            {nCols nRows [1 12] [1 1]} {nCols nRows [2 12] [1 1]} ... % Plot Correction Check Box
            {nCols nRows [0 13] [1 1]} ... % Empty Line
            {nCols nRows [2 14] [1 2]} {nCols nRows [3 14] [1 2]} ... % Generate button
            {nCols nRows [0 15] [1 1]} ... % Empty Line
            }, ... 
        'uilist',...
           {{'style' 'text' 'string' ' Design : '}  {'style' 'popupmenu' 'string' designString 'tag' 'design' 'value' 1 'callback' cbo_design_cb} ...
            {} ... 
            {'style' 'text' 'string' ' Stats' 'FontWeight' 'Bold'} ...
            {'style' 'text' 'string' ' Stats : '}  {'style' 'popupmenu' 'string' statsString 'tag' 'stats' 'value' 1} {'style' 'text' 'string' ' Signif. Threshold : '}  {'style' 'edit' 'string' statsThreshold 'tag' 'threshold' 'userdata' 'eeglab' } ...
            {'style' 'text' 'string' ' Correction : '}  {'style' 'popupmenu' 'string' correctionString 'tag' 'correction' 'value' 1} ...
            {'style' 'text' 'string' ' Compute Stats : '} {'style' 'checkbox' 'string' '' 'tag' 'compute' 'value' statsCompute 'callback' check_compute_cb} , ... ...
            {} ...
            {'style' 'text' 'string' ' Plot & Variables' 'FontWeight' 'Bold'} {'style' 'text' 'string' ' Name'} {'style' 'text' 'string' ' Value'} ...
            {'style' 'text' 'string' ' Variable 1 : '}  {'style' 'popupmenu' 'string' variablesString 'tag' 'var1' 'value' 1 'callback' cbo_var1_cb} {'style' 'popupmenu' 'string' variablesValString 'tag' 'var1val' 'value' 1 'callback' cbo_var1val_cb} ...
            {'style' 'text' 'string' ' Variable 2 : '}  {'style' 'popupmenu' 'string' variablesString 'tag' 'var2' 'value' 1 'callback' cbo_var2_cb} {'style' 'popupmenu' 'string' variablesValString 'tag' 'var2val' 'value' 1 'callback' cbo_var2val_cb} ...
            {'style' 'text' 'string' ' Variable 3* : '}  {'style' 'popupmenu' 'string' variablesString 'tag' 'var3' 'value' 1 'callback' cbo_var3_cb} {'style' 'popupmenu' 'string' variablesValString 'tag' 'var3val' 'value' 1 'callback' cbo_var3val_cb} ...
            {'style' 'text' 'string' ' Variable 4* : '}  {'style' 'popupmenu' 'string' variablesString 'tag' 'var4' 'value' 1 'callback' cbo_var4_cb} {'style' 'popupmenu' 'string' variablesValString 'tag' 'var4val' 'value' 1 'callback' cbo_var4val_cb} ...
            {'style' 'text' 'string' ' Use Corrected pVals : '} {'style' 'checkbox' 'string' '' 'tag' 'plot_correction' 'value' true} , ... 
            {} ...
            {'style' 'pushbutton' 'string' 'Generate Sub' 'tag' 'generatesub' 'callback' generatesub_plot_cb} {'style' 'pushbutton' 'string' 'Generate Full' 'tag' 'generatefull' 'callback' generatefull_plot_cb} ...
            {} ...
            }, 'userdata', uDataVariables);
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
    
    StaRParams.vars{1}.name_ind      = userInput{1,6};
    StaRParams.vars{1}.val_ind       = userInput{1,7};
    StaRParams.vars{2}.name_ind      = userInput{1,8};
    StaRParams.vars{2}.val_ind       = userInput{1,9};
    StaRParams.vars{3}.name_ind      = userInput{1,10};
    StaRParams.vars{3}.val_ind       = userInput{1,11};
    StaRParams.vars{4}.name_ind      = userInput{1,12};
    StaRParams.vars{4}.val_ind       = userInput{1,13};
    
    StaRParams.vars{1}.name = variables{StaRParams.vars{1}.name_ind}.name;
    StaRParams.vars{1}.value = variables{StaRParams.vars{1}.name_ind}.values{StaRParams.vars{1}.val_ind};
    StaRParams.vars{2}.name = variables{StaRParams.vars{2}.name_ind}.name;
    StaRParams.vars{2}.value = variables{StaRParams.vars{2}.name_ind}.values{StaRParams.vars{2}.val_ind};
    StaRParams.vars{3}.name = variables{StaRParams.vars{3}.name_ind}.name;
    StaRParams.vars{3}.value = variables{StaRParams.vars{3}.name_ind}.values{StaRParams.vars{3}.val_ind};
    StaRParams.vars{4}.name = variables{StaRParams.vars{4}.name_ind}.name;
    StaRParams.vars{4}.value = variables{StaRParams.vars{4}.name_ind}.values{StaRParams.vars{4}.val_ind};
    
    % Get params and send them over to R in a 'StaR' protocol!
    disp('Saving... ');
    fileTestName = 'testParamsR.mat';
    disp(fileTestName);
    
    starDesign = StaRParams.design;
    starStats = StaRParams.stats;
    starThreshold = StaRParams.threshold;
    starCorrection = StaRParams.correction;
    starVars = StaRParams.vars;
    save(fileTestName, 'starDesign', 'starStats', 'starThreshold', 'starCorrection', 'starVars');
    
    disp('Saved!');
    
    % -- Debug Disp --
    disp(['Design: ' designs{StaRParams.design}.Name]);
    disp(['Stats: ' stats{StaRParams.stats}.function stats{StaRParams.stats}.details]);
    disp(['Correction: ' corrections{StaRParams.correction}]);
    disp(['Signif. Threshold: ' StaRParams.threshold]);
    disp(['Compute? ' num2str(StaRParams.compute)]);
    disp('--------------');
    disp(['Var1 - ' StaRParams.vars{1}.name]);
    disp(['Var1 Val - ' StaRParams.vars{1}.value]);
    disp(['Var2 - ' StaRParams.vars{2}.name]);
    disp(['Var2 Val - ' StaRParams.vars{2}.value]);
    disp(['Var3 - ' StaRParams.vars{3}.name]);
    disp(['Var3 Val - ' StaRParams.vars{3}.value]);
    disp(['Var4 - ' StaRParams.vars{4}.name]);
    disp(['Var4 Val - ' StaRParams.vars{4}.value]);
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
    hdl = varargin{2}; %figure handle
    userdat = get(varargin{2}, 'userdata');
    obj = userdat{1};
    domainID = userdat{2};
    variables = userdat{3};
    designs = userdat{4};
    stats = userdat{5};
    
    updateVals = [];
	
    defaultFolder = '/Users/yannick/Documents/PhD/Stats Test/mTBI_SubClean_Measures/MPT_Export/Star Images/Latest/';
    
    switch  varargin{1}
        case 'cbo_design'
            var_designID = get(findobj('parent', hdl, 'tag', 'design'), 'value');
            var_statsID = get(findobj('parent', hdl, 'tag', 'stats'), 'value');
            
            %---------------------------------------------------
            % Update the Var Combobox Based on Design Variables.
            %---------------------------------------------------
            [nbVars, varString] = getVarString(variables, designs{var_designID}.Name, true);
            if nbVars > 0 
                set(findobj('parent', hdl,'tag', 'var1'), 'string', varString, 'value', 1);
            else
                set(findobj('parent', hdl,'tag', 'var1'), 'string', 'N/A', 'value', 1);
            end
            if nbVars > 1
                set(findobj('parent', hdl,'tag', 'var2'), 'string', varString, 'value', 1);
            else
                set(findobj('parent', hdl,'tag', 'var2'), 'string', 'N/A', 'value', 1);
            end
            if nbVars > 2                
                set(findobj('parent', hdl,'tag', 'var3'), 'string', varString, 'value', 1);
            else
                set(findobj('parent', hdl,'tag', 'var3'), 'string', 'N/A', 'value', 1);
            end
            if nbVars > 3
                set(findobj('parent', hdl,'tag', 'var4'), 'string', varString, 'value', 1);
            else
                set(findobj('parent', hdl,'tag', 'var4'), 'string', 'N/A', 'value', 1);
            end
            
            updateVals = [1,2,3,4];
            % Don't forget to update their Values! (putting the string and
            % value in the UI, doesn't trigger the callback.
            %---------------------------------------------------
            
            disp(designs{var_designID}.Name);
            designFolderPath = [defaultFolder obj.object.measureLabel '/Domain_' num2str(domainID) '/Stats_' stats{var_statsID}.function '/Design_' obj.object.measureLabel '_' num2str(designs{var_designID}.No)];

            disp(['Checking for files in: ' designFolderPath]);
            if exist(designFolderPath, 'dir')
                disp(['Design found in ' designFolderPath]);
            else
                disp(['Design NOT found in ' designFolderPath]);
                disp('Disabling Controls...'); % TODO ...
            end
            
        case 'btn_generatesub'
            var_designID = get(findobj('parent', hdl, 'tag', 'design'), 'value');
            var_statsID = get(findobj('parent', hdl, 'tag', 'stats'), 'value');
            
            disp(designs{var_designID}.Name);

            designFolderPath = [defaultFolder obj.object.measureLabel '/Domain_' num2str(domainID) '/Stats_' stats{var_statsID}.function '/Design_' obj.object.measureLabel '_' num2str(designs{var_designID}.No)];
            designFilePath = [designFolderPath '/Workspace_Fullx.mat'];
                
            generateStaRPlots(obj.object, domainID, designFilePath);
            
        case 'btn_generatefull'
            disp('Work in Progress!'); 
            
        case 'check_compute'
            checkcompute = get(findobj('parent', hdl, 'tag', 'compute'), 'value')
            
        case 'cbo_var1'
            updateVals = 1;
        case 'cbo_var2'           
            updateVals = 2;
        case 'cbo_var3'           
            updateVals = 3;
        case 'cbo_var4'        
            updateVals = 4;
            
        case 'cbo_var1val'
            %disp('Fix Var 1 Val');
        case 'cbo_var2val'
            %disp('Fix Var 2 Val');
    end             
    
    for i = updateVals
        var_selID = get(findobj('parent', hdl, 'tag', ['var' num2str(i)]), 'value');
        valString = getValString(variables{var_selID}.values);
        set(findobj('parent', hdl,'tag', ['var' num2str(i) 'val']), 'string', valString, 'value', length(variables{var_selID}.values));  
    end
end

function figureHandle = generateStaRPlots(obj, domainID, designFileName) % TODO: Replace with Params{:}
    
    bSanityCheck = 1;
    
    %if statsCompute
    bCompStats = get(findobj('parent', hdl, 'tag', 'compute'), 'value');
    if bCompStats
        disp('Compute the stats...');
    else
        disp('Do not compute the stats...');
    end
    
    % TODO: Add CheckBox to be able to plot regular pValues.
    bPSignificants = 1;
    
    pSignifThreshold = str2num(get(findobj('parent', hdl, 'tag', 'threshold'), 'string'));
    bPCorrected = get(findobj('parent', hdl, 'tag', 'plot_correction'), 'value');
    
    Var1_selID = get(findobj('parent', hdl, 'tag', 'var1'), 'value');
    Val1_selID = get(findobj('parent', hdl, 'tag', 'var1val'), 'value');
    Var2_selID = get(findobj('parent', hdl, 'tag', 'var2'), 'value');
    Val2_selID = get(findobj('parent', hdl, 'tag', 'var2val'), 'value');
    Var3_selID = get(findobj('parent', hdl, 'tag', 'var3'), 'value');
    Val3_selID = get(findobj('parent', hdl, 'tag', 'var3val'), 'value');
    Var4_selID = get(findobj('parent', hdl, 'tag', 'var4'), 'value');
    Val4_selID = get(findobj('parent', hdl, 'tag', 'var4val'), 'value');

    % Prep Var1 & check if 'All'
    if(strcmp(variables{Var1_selID}.values{Val1_selID}, 'All'))
        for i = 1:length(variables{Var1_selID}.values) - 1
            Val1{i} = variables{Var1_selID}.values{i};
            Var1{i} = variables{Var1_selID}.name;
        end 
    else
        Var1{1} = variables{Var1_selID}.name;
        Val1{1} = variables{Var1_selID}.values{Val1_selID};
    end

    % Prep Var2 & check if 'All'
    if(strcmp(variables{Var2_selID}.values{Val2_selID}, 'All'))
        for i = 1:length(variables{Var2_selID}.values) - 1
            Val2{i} = variables{Var2_selID}.values{i};
            Var2{i} = variables{Var2_selID}.name;
        end 
    else
        Var2{1} = variables{Var2_selID}.name;
        Val2{1} = variables{Var2_selID}.values{Val2_selID};
    end
    
    % Prep Var3 & check if 'All'
    if(strcmp(variables{Var3_selID}.values{Val3_selID}, 'All'))
        %for i = 1:length(variables{Var3_selID}.values) - 1
        %    Val3{i} = variables{Var3_selID}.values{i};
        %    Var3{i} = variables{Var3_selID}.name;
        %end 
        disp('## NOT SUPPORTED! Please select 1 value for your variable 4');
        return;
    else
        Var3{1} = variables{Var3_selID}.name;
        Val3{1} = variables{Var3_selID}.values{Val3_selID};
    end

    % Prep Var4 & check if 'All'
    if(strcmp(variables{Var4_selID}.values{Val4_selID}, 'All'))
        %for i = 1:length(variables{Var4_selID}.values) - 1
        %    Val4{i} = variables{Var4_selID}.values{i};
        %    Var4{i} = variables{Var4_selID}.name;
        %end 
        disp('## NOT SUPPORTED! Please select 1 value for your variable 4');
        return;
    else
        Var4{1} = variables{Var4_selID}.name;
        Val4{1} = variables{Var4_selID}.values{Val4_selID};
    end
    
    disp('-- Var & Vals --');
    disp(Var1);
    disp(Val1);
    disp(Var2);
    disp(Val2);
    disp(Var3);
    disp(Val3);
    disp(Var4);
    disp(Val4);
    disp('----------------');

    % Plot Based on Selection.
    vars_test = {{'conditions'}, {'groups'}, {'groups'}};
    vals_test = {{'FOM'}, {'3'}, {'4'}};

    [Var1, Val1] = cleanVarVals(Var1, Val1);
    [Var2, Val2] = cleanVarVals(Var2, Val2);
    [Var3, Val3] = cleanVarVals(Var3, Val3);
    [Var4, Val4] = cleanVarVals(Var4, Val4);
    
    vars_UI = cat(2, Var1, Var2, Var3, Var4);
    vals_UI = cat(2, Val1, Val2, Val3, Val4);

    vars = vars_UI;
    vals = vals_UI;

    cleanVarVals(vars, vals)

    % Load existing file. (for now)
    if strcmp(obj.measureLabel, 'ERP') && strcmp(designFileName, '')
        %filename = '/Users/yannick/Documents/PhD/Stats Test/mTBI_SubClean_Measures/MPT_Export/Playground/Jul05_13h03/ERP/Domain_1/Stats_aov/Design_ERP_11/Workspace_Fullx.mat';
        designFileName = '/Users/yannick/Documents/PhD/Stats Test/mTBI_SubClean_Measures/MPT_Export/Playground/Jul10_00h02/ERP/Domain_1/Stats_aov/Design_ERP_11/Workspace_Fullx.mat';
    end
    if strcmp(obj.measureLabel, 'ERSP') && strcmp(designFileName, '')
        %filename = '/Users/yannick/Documents/PhD/Stats Test/mTBI_SubClean_Measures/MPT_Export/Star Images/Jul09_23h08/ERSP/Domain_1/Stats_aov/Design_ERSP_11/Workspace_Fullx.mat';
        designFileName = '/Users/yannick/Documents/PhD/Stats Test/mTBI_SubClean_Measures/MPT_Export/Star Images/Jul10_19h13/ERSP/Domain_1/Stats_aov/Design_ERSP_11/Workspace_Fullx.mat';
    end
    
    if ~exist(designFileName, 'file')
        disp(['## Invalid file: ' designFileName]);
        return;
    end
    
    % Get DF & VarVals. (only once)
    df = StaR_getVarValDF(designFileName);
    
    bShortTitles = 1;
    bPValsTitles = 0;
    
    mixPlots = {};
    mixTitles = {};
    
    % Rows (i, Var1) | Cols (j, Var2) - with other vars.
    for i = 1:length(Val1)
        if ~isempty(Val2)
            for j = 1:length(Val2)
                curDataVars = cat(2, Var1{i}, Var2{j}, Var3, Var4);
                curDataVals = cat(2, Val1{i}, Val2{j}, Val3, Val4);
                dataPlotIDs{i,j} = StaR_getVarValPlots(curDataVars, curDataVals, df, 'data');
                if(~isempty(dataPlotIDs{i,j}))
                    mixPlots{i,j} =  df.data{dataPlotIDs{i,j}}.plotVal;
                    mixTitles{i,j} = df.data{dataPlotIDs{i,j}}.lbl;
                end
                
                curPValsVars = cat(2, Var1{:}, Var2{j}, Var3, Var4);
                curDataVals = cat(2, Val1{:}, Val2{j}, Val3, Val4);
                pValsPlotIDs.cols{j} = StaR_getVarValPlots(curPValsVars, curDataVals, df, 'pValsSub');
                
                if ~isempty(pValsPlotIDs.cols{j})
                    if bPCorrected
                        mixPlots{length(Val1) + 1, j} =  df.pValsSub{pValsPlotIDs.cols{j}}.plotValCorrected;
                    else
                        mixPlots{length(Val1) + 1, j} =  df.pValsSub{pValsPlotIDs.cols{j}}.plotVal;
                    end
                    
                    if bPSignificants && ~isempty(mixPlots{length(Val1) + 1, j})
                        mixPlots{length(Val1) + 1, j}(mixPlots{length(Val1) + 1, j} < pSignifThreshold) = 2; % 2 for a better color contrast.
                        mixPlots{length(Val1) + 1, j}(mixPlots{length(Val1) + 1, j} ~= 2) = 0;
                    end
                        
                    if bPValsTitles
                        mixTitles{length(Val1) + 1, j} = df.pValsSub{pValsPlotIDs.cols{j}}.lbl;
                    else
                        mixTitles{length(Val1) + 1, j} = 'pVals';
                    end
                end
            end
        else
            dataPlotIDs{i, 1} = StaR_getVarValPlots(vars, vals, df, 'data');
            
            if ~isempty(dataPlotIDs{i, 1})
                mixPlots{i, 1} =  df.data{dataPlotIDs{i, 1}}.plotVal;
                mixTitles{i, 1} = df.data{dataPlotIDs{i, 1}}.lbl;
            end
        end
        
        % Get pVals Plots Combining Vars / Vals.
        curPValsVars = cat(2, Var1{i}, Var2{:}, Var3, Var4);
        curDataVals = cat(2, Val1{i}, Val2{:}, Val3, Val4);
        pValsPlotIDs.rows{i} = StaR_getVarValPlots(curPValsVars, curDataVals, df, 'pValsSub');
        
        if ~isempty(pValsPlotIDs.rows{i})
            if bPCorrected
                mixPlots{i, length(Val2) + 1} =  df.pValsSub{pValsPlotIDs.rows{i}}.plotValCorrected;
            else
                mixPlots{i, length(Val2) + 1} =  df.pValsSub{pValsPlotIDs.rows{i}}.plotVal;
            end
            
            if bPSignificants && ~isempty(mixPlots{i, length(Val2) + 1})
                mixPlots{i, length(Val2) + 1}(mixPlots{i, length(Val2) + 1} < pSignifThreshold) = 2; % 2 for a better color contrast.
                mixPlots{i, length(Val2) + 1}(mixPlots{i, length(Val2) + 1} ~= 2) = 0;
            end
                    
            if bPValsTitles
                mixTitles{i, length(Val2) + 1} = df.pValsSub{pValsPlotIDs.rows{i}}.lbl;
            else
                mixTitles{i, length(Val2) + 1} = 'pVals';
            end
        end
    end
    
    % pValsFull 
    for p = 1:length(df.pValsFull)
        if bPCorrected
            pValsFullPlots{p} = df.pValsFull{p}.plotValCorrected;
        else
            pValsFullPlots{p} = df.pValsFull{p}.plotVal;
        end
        
        if bPSignificants
            pValsFullPlots{p}(pValsFullPlots{p} < pSignifThreshold) = 2;
            pValsFullPlots{p}(pValsFullPlots{p} ~= 2) = 0;
        end
        
        pValsFullTitles{p} = df.pValsFull{p}.lbl;
    end
    
    
    % Titles can become quite long and difficult to read - you can shorten
    % them, keeping only first 2 letters of the variable name.
    if bShortTitles
        mixTitles = getShortTitles(mixTitles);
    end
    
    % ------------------------------------
    % Plot Sub Data + pVals from Sub Data.
    % ------------------------------------
    if length(mixPlots) == 0
        disp('There is nothing to plot... Please select something else.');
        return;
    else
        if (strcmp(obj.measureLabel, 'ERSP'))
            disp(['Plotting ERSP Domain #' num2str(domainID)]);
            std_plottf(df.timeData, df.freqData, mixPlots,  'titles', mixTitles);%, varargin{:});
            
            std_plottf(df.timeData, df.freqData, pValsFullPlots,  'titles', pValsFullTitles);
        end

        if (strcmp(obj.measureLabel, 'ERP'))
            disp(['ERP! Domain #' num2str(domainID)]);
            %if length(measureAsArrayInCell) <= numberOfConditionsInEachFigure
                % put ERP traces together inone plot;


                %pr.std_plotcurve(df.timeData, {df.pVals{plotIDs(i)}.plotVal}, 'titles', {df.pVals{plotIDs(i)}.lbl}, 'datatype','erp', 'plotconditions', 'together','plotgroups', 'apart', 'figure', 'off');%, varargin{:});
                pr.std_plotcurve(df.timeData, mixPlots, 'titles', mixTitles, 'datatype','erp', 'plotconditions', 'together','plotgroups', 'apart', 'figure', 'off');%, varargin{:});

                % put legend for conditions
            %    legend(title);

                % make lines thicker.
            %   set(findobj(gcf, 'type', 'Line'), 'linewidth', 2);
            %end
        end
        %plot(df{plotIDs(i)}.plotVal);

        %figureHandle = gfc;
    end
    
    % ------------------------------------
    
    % ------------------------------------
end

function [nbVars varString] = getVarString(variables, strDesign, bAll)
    variablesString = '';
    nbVars = 0;
    
    if length(variables)>1
        for n = 1:length(variables)
            if (bAll == 1) || ~isempty(strfind(strDesign, variables{n}.name))
                if isempty(variablesString)
                    variablesString = variables{n}.name;
                else
                    variablesString = [variablesString '|'  variables{n}.name];
                end
                nbVars = nbVars + 1;
            end
        end
    end
    
    if isempty(variablesString)
        variablesString = 'N/A';
    end
    
    varString = variablesString;
end

function valString = getValString(vals)
    variablesValString = '';
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

    valString = variablesValString;
end

function [vars, vals] = cleanVarVals(vars, vals)
    for i = 1:length(vars)
        if (strcmp(vars{i}, 'N/A') == 1) || (strcmp(vals{i}, 'N/A') == 1)
            vars{i} = [];
            vals{i} = [];
        end
    end
end

%% ShortTitles
% Shitty Code to Shorten the Titles with only the First 2 Letters. (e.g. Groups=3 -> Gr=3)
% TODO : Handle the potential problems to avoid crashes.
function shortTitles = getShortTitles(titles)
    
    shortTitles = {};
    
    for i = 1:size(titles,1)
        for j = 1:size(titles, 2)                
             comb1 = strsplit(titles{i,j}, '|');
             shortTitle1 = '';
             for k = 1:length(comb1)
                 comb2 = strsplit(comb1{k}, ';');
                 shortTitle2 = '';
                 for l = 1:length(comb2)
                     tmpComb = strsplit(comb2{l}, '=')
                     if length(tmpComb) == 2
                         comb2{l} = [tmpComb{1}(1:2) '=' tmpComb{2}];
                         if isempty(shortTitle2)
                             shortTitle2 = comb2{l};
                         else
                             shortTitle2 = [shortTitle2 ';' comb2{l}];
                         end
                     else
                         disp('## tmpComb in shortTitles is not 2');
                     end
                 end
                 if isempty(shortTitle1)
                     shortTitle1 = shortTitle2
                 else
                     shortTitle1 = [shortTitle1 '|' shortTitle2]
                 end
             end
             shortTitles{i,j} = shortTitle1;
        end    
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
