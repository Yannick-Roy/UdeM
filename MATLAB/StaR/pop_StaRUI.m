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
    btn_generatesub_cb = ['pop_StaRUI(''btn_generatesub'',gcf);'];
    btn_generatefull_cb = ['pop_StaRUI(''btn_generatefull'',gcf);'];
    btn_generatedata_cb = ['pop_StaRUI(''btn_generatedata'',gcf);'];
    btn_custom_cb = ['pop_StaRUI(''btn_custom'',gcf);'];
    btn_export_cb = ['pop_StaRUI(''btn_export'',gcf);'];
    btn_runRstats_cb = ['pop_StaRUI(''btn_runRstats'',gcf);'];
    check_thisdomain_cb = ['pop_StaRUI(''check_thisdomain'',gcf);']; 
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
    designs{1}.Name  = ['Design #' num2str(designs{1}.No) ': (values ~ domains * groups * sessions * orders * motions)'];
    designs{2}.No = 2;
    designs{2}.Name  = ['Design #' num2str(designs{2}.No) ': (values ~ groups * sessions * orders * motions)'];
    

    % Stats Type.
    stats = {};
    stats{1}.function = 'lme';
    stats{1}.details = '(mixed models)';
    stats{2}.function = 'custom';
    stats{2}.details = '(custom)';
    
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
    
    [nbVars, varString] = getVarString(variables, designs{1}.Name, true);
    
    variablesString = varString; %'N/A';
    variablesValString = 'N/A';
    
    nbPlots = 6;
    nbPlotsValid = 1;
    nbSessions = length(STUDY.session);
    nbGroups = length(STUDY.group);

    statsThreshold = 0.05;
    checkThisDomain = true;
    plot_diffmask = true;
    pSignificant = true;
    
    uDataVariables = {obj domainID variables designs stats STUDY}
    
    % collect user input
    try
        nRows = 18;
        nCols = 5;
       userInput = inputgui('title', 'pop_StaRUI()', 'geom', ...
           {{nCols nRows [0 0] [1 1]} {nCols nRows [1 0] [4 1]} ...
            {nCols nRows [0 2] [1 1]} ... % Title Line
            {nCols nRows [0 3] [1 1]} {nCols nRows [1 3] [2 1]} {nCols nRows [3 3] [1 1]} {nCols nRows [4 3] [1 1]} ...
            {nCols nRows [0 4] [1 1]} {nCols nRows [1 4] [1 1]} ...
            {nCols nRows [0 5] [1 1]} ... % Empty Line
            {nCols nRows [0 6] [1 1]} {nCols nRows [2 6] [1 1]} {nCols nRows [3 6] [1 1]} ... % Title Line
            {nCols nRows [1 7] [1 1]} {nCols nRows [2 7] [1 1]} {nCols nRows [3 7] [1 1]} ... % Var 1
            {nCols nRows [1 8] [1 1]} {nCols nRows [2 8] [1 1]} {nCols nRows [3 8] [1 1]} ... % Var 2
            {nCols nRows [1 9] [1 1]} {nCols nRows [2 9] [1 1]} {nCols nRows [3 9] [1 1]} ... % Var 3
            {nCols nRows [1 10] [1 1]} {nCols nRows [2 10] [1 1]} {nCols nRows [3 10] [1 1]} ... % Var 4
            {nCols nRows [1 11] [1 1]} {nCols nRows [2 11] [1 1]} {nCols nRows [3 11] [1 1]} {nCols nRows [4 11] [1 1]} ... % Plot Correction Check Box
            {nCols nRows [1 12] [1 1]} {nCols nRows [2 12] [1 1]} ... % Plot Only pVals Chec Box
            {nCols nRows [1 13] [1 1]} {nCols nRows [2 13] [1 1]} ... % This Domain Only Check Box
            {nCols nRows [0 14] [1 1]} ... % Empty Line
            {nCols nRows [1 15] [1 2]} {nCols nRows [2 15] [1 2]} {nCols nRows [3 15] [1 2]} ... % Generate button
            {nCols nRows [0 17] [1 1]} ... % Empty Line
            {nCols nRows [0 18] [1 1]} {nCols nRows [1 18] [1 1]} {nCols nRows [2 18] [1 1]} {nCols nRows [3 18] [1 1]}... % Export & Run R Stats & Custom
            }, ... 
        'uilist',...
           {{'style' 'text' 'string' ' Design : '}  {'style' 'popupmenu' 'string' designString 'tag' 'design' 'value' 1 'callback' cbo_design_cb} ...            
            {'style' 'text' 'string' ' Stats' 'FontWeight' 'Bold'} ...
            {'style' 'text' 'string' ' Stats : '}  {'style' 'popupmenu' 'string' statsString 'tag' 'stats' 'value' 1} {'style' 'text' 'string' ' Signif. Threshold : '}  {'style' 'edit' 'string' statsThreshold 'tag' 'threshold' 'userdata' 'eeglab' } ...
            {'style' 'text' 'string' ' Correction : '}  {'style' 'popupmenu' 'string' correctionString 'tag' 'correction' 'value' 2} ...           
            {} ... 
            {'style' 'text' 'string' ' Plot & Variables' 'FontWeight' 'Bold'} {'style' 'text' 'string' ' Name'} {'style' 'text' 'string' ' Value'} ...
            {'style' 'text' 'string' ' Variable 1 : '}  {'style' 'popupmenu' 'string' variablesString 'tag' 'var1' 'value' 1 'callback' cbo_var1_cb} {'style' 'popupmenu' 'string' variablesValString 'tag' 'var1val' 'value' 1 'callback' cbo_var1val_cb} ...
            {'style' 'text' 'string' ' Variable 2 : '}  {'style' 'popupmenu' 'string' variablesString 'tag' 'var2' 'value' 1 'callback' cbo_var2_cb} {'style' 'popupmenu' 'string' variablesValString 'tag' 'var2val' 'value' 1 'callback' cbo_var2val_cb} ...
            {'style' 'text' 'string' ' Variable 3* : '}  {'style' 'popupmenu' 'string' variablesString 'tag' 'var3' 'value' 1 'callback' cbo_var3_cb} {'style' 'popupmenu' 'string' variablesValString 'tag' 'var3val' 'value' 1 'callback' cbo_var3val_cb} ...
            {'style' 'text' 'string' ' Variable 4* : '}  {'style' 'popupmenu' 'string' variablesString 'tag' 'var4' 'value' 1 'callback' cbo_var4_cb} {'style' 'popupmenu' 'string' variablesValString 'tag' 'var4val' 'value' 1 'callback' cbo_var4val_cb} ...
            {'style' 'text' 'string' ' Use Corrected pVals : '} {'style' 'checkbox' 'string' '' 'tag' 'plot_correction' 'value' true} {'style' 'text' 'string' ' Use Significant pVals : '} {'style' 'checkbox' 'string' '' 'tag' 'pSignificant' 'value' true} ...
            {'style' 'text' 'string' ' Plot Diff Mask : '} {'style' 'checkbox' 'string' '' 'tag' 'plot_diffmask' 'value' true} ...
            {'style' 'text' 'string' ' This Domain Only : '} {'style' 'checkbox' 'string' '' 'tag' 'check_thisdomain' 'value' checkThisDomain 'callback' check_thisdomain_cb} ...
            {} ...
            {'style' 'pushbutton' 'string' 'Data Only' 'tag' 'generatedata' 'callback' btn_generatedata_cb} {'style' 'pushbutton' 'string' 'Post Hoc' 'tag' 'generatesub' 'callback' btn_generatesub_cb} {'style' 'pushbutton' 'string' 'Effects' 'tag' 'generatefull' 'callback' btn_generatefull_cb} ...
            {} ...
            {'style' 'text' 'string' ' Other Tools : ' 'FontWeight' 'Bold'} {'style' 'pushbutton' 'string' 'Export MPT' 'tag' 'btn_export' 'callback' btn_export_cb} {'style' 'pushbutton' 'string' 'Run R Stats' 'tag' 'btn_runRstats' 'callback' btn_runRstats_cb} {'style' 'pushbutton' 'string' 'Custom StaR' 'tag' 'btn_custom' 'callback' btn_custom_cb}...
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
%     strStatsPath = ['/Users/yannick/Documents/PhD/Stats Test/mTBI_SubClean_ShortMeasures/MPT_Export/Star Images/Latest/' obj.object.measureLabel '/Domain_' num2str(curDomain) '/Stats_' stats{StaRParams.stats}.function '/Design_' obj.object.measureLabel '_' num2str(designs{StaRParams.design}.No)];
%     disp(strStatsPath);
%     if exist(strStatsPath, 'dir')
%         disp('Valid Path!');
%     else
%         disp('INVALID PATH!');
%     end

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
    STUDY = userdat{6};
    
    updateVals = [];
	
    defaultFolder = '/Users/yannick/Documents/PhD/Stats Test/mTBI_SubClean_ShortMeasures/MPT_Export/StaR Images/Latest/';
    %defaultFolder = '/media/user/BrainData/Study_mTBI_x/MPT_Export/StaR Images/Latest/';
    %defaultFolder = 'E:\Study_mTBI_x\MPT_Export\StaR Images\Latest\';
    
    switch  varargin{1}
        case 'cbo_design'
            var_designID = get(findobj('parent', hdl, 'tag', 'design'), 'value');
            var_statsID = get(findobj('parent', hdl, 'tag', 'stats'), 'value');
            
            if var_designID == 2
                disp('## DESIGN NOT SUPPORTED YET ##');
            end
            
        case 'btn_generatesub'
            disp('Generating Sub...');
        case 'btn_generatefull'
            disp('Generating Full...');
        case 'btn_custom'
            % pop up window
            % -------------
            [inputname, inputpath] = uigetfile2('*.mat', 'Load Custom StaR Plot(s) -- pop_StaRUI()', 'multiselect', 'off');
            drawnow;
            if isequal(inputname, 0) return; end;
            options = { 'filename' inputname 'filepath' inputpath };
            
            StaR_getCustomPlots([inputpath '/' inputname]);
            
        case 'btn_export'
            exportMPT(STUDY);
            
        case 'btn_runRstats'
            system('/Library/Frameworks/R.framework/Resources/bin/R --version');
            
        case 'check_thisdomain'
            checkThisDomain = get(findobj('parent', hdl, 'tag', 'checkThisDomain'), 'value')
            
            disp(['This Domain Only: ' num2str(checkThisDomain)]);
            
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
    
    if strcmp(varargin{1}, 'btn_generatesub') || strcmp(varargin{1}, 'btn_generatefull') || strcmp(varargin{1}, 'btn_generatedata')
        var_designID = get(findobj('parent', hdl, 'tag', 'design'), 'value');
        var_statsID = get(findobj('parent', hdl, 'tag', 'stats'), 'value');

        disp(designs{var_designID}.Name);

        % They both use the function, bu the Full only display the full
        % pVals, not the sub data.
        if strcmp(varargin{1}, 'btn_generatefull')
            bFull = true;
        else
            bFull = false;
        end
        if strcmp(varargin{1}, 'btn_generatedata')
            bPlotDataOnly = true;
        else
            bPlotDataOnly = false;
        end
        
        % Folder Path. Then Filename will be "constructed" in the function.
        designFolderPath = [defaultFolder obj.object.measureLabel];
        
        generateStaRPlots(obj.object, domainID, designFolderPath, bFull, bPlotDataOnly);
    end
    
    for i = updateVals
        var_selID = get(findobj('parent', hdl, 'tag', ['var' num2str(i)]), 'value');
        valString = getValString(variables{var_selID}.values);
        set(findobj('parent', hdl,'tag', ['var' num2str(i) 'val']), 'string', valString, 'value', length(variables{var_selID}.values));  
    end
end

%% generateStaRPlots()
%
% Inputs:
%   - obj:
%   - domainID:
%   - designFileName:
%
% Outputs:
%   - figureHandle:
%
% Description:
%
%
function figureHandle = generateStaRPlots(obj, domainID, designFolderName, bFull, bPlotDataOnly) % TODO: Replace with Params{:}
    
    bSanityCheck = 1;
    
    if nargin < 4
        bFull = false;
    end
    if nargin < 5
        bPlotDataOnly = true;
    end
    
    if ~exist(designFolderName, 'dir')
        disp(['## Invalid Folder: ' designFolderName]);
        return;
    end
    
    bPSignificants = get(findobj('parent', hdl, 'tag', 'pSignificant'), 'value');
    pSignifThreshold = str2num(get(findobj('parent', hdl, 'tag', 'threshold'), 'string'));
    bPCorrected = get(findobj('parent', hdl, 'tag', 'plot_correction'), 'value');
    bThisDomainOnly = get(findobj('parent', hdl, 'tag', 'check_thisdomain'), 'value');
    bPlotDiffMask = get(findobj('parent', hdl, 'tag', 'plot_diffmask'), 'value');
    
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
    [Var1, Val1] = cleanVarVals(Var1, Val1);
    [Var2, Val2] = cleanVarVals(Var2, Val2);
    [Var3, Val3] = cleanVarVals(Var3, Val3);
    [Var4, Val4] = cleanVarVals(Var4, Val4);
    
    vars_UI = cat(2, Var1, Var2, Var3, Var4);
    vals_UI = cat(2, Val1, Val2, Val3, Val4);

    vars = vars_UI;
    vals = vals_UI;

    cleanVarVals(vars, vals)
    
    if bFull
        designFileName = [designFolderPath '/Workspace_Fullx.mat'];
    else    
        curVarString = '';
        if ~isempty(Var1{1})
            if isempty(curVarString)
                curVarString = Var1{1};
            else
                curVarString = strcat(curVarString, '_', Var1{1});
            end
        end
        if ~isempty(Var2{1})
            if isempty(curVarString)
                curVarString = Var2{1};
            else
                curVarString = strcat(curVarString, '_', Var2{1});
            end
        end
        if ~isempty(Var3{1})
            if isempty(curVarString)
                curVarString = Var3{1};
            else
                curVarString = strcat(curVarString, '_', Var3{1});
            end
        end
        if ~isempty(Var4{1})
            if isempty(curVarString)
                curVarString = Var4{1};
            else
                curVarString = strcat(curVarString, '_', Var4{1});
            end
        end
        
        fileNameValid = '';
        curVars = {Var1{1}, Var2{1}, Var3{1}, Var4{1}};
        curVars = curVars(~cellfun('isempty',curVars))
        curVarsCombs = perms(curVars);
        for c = 1:size(curVarsCombs, 1)
            curVarString = '';
            for cs = 1:size(curVarsCombs, 2)
                if(isempty(curVarString))
                    curVarString = char(curVarsCombs{c, cs});
                else
                    curVarString = strcat(curVarString, '_', char(curVarsCombs{c, cs}));
                end
            end
            tmpDesignFileName = [designFolderPath '/Workspace_PH_' curVarString '.mat']
            if exist(tmpDesignFileName, 'file')
                fileNameValid = tmpDesignFileName;
            end
        end
        
        if ~isempty(fileNameValid)
            designFileName = fileNameValid;
        else
            designFileName = [designFolderPath '/Workspace_PH_X.mat'];
        end
    end
       
    if ~exist(designFileName, 'file')
        disp(['## Invalid File: ' designFileName]);
        return;
    end
    
    % Get DF & VarVals. (only once)
    df = StaR_getVarValDF(designFileName);
    
    bShortTitles = 1;
    bPValsTitles = 1;
    bPlotpValOnly = 0;
    
    mixPlots = {};
    mixTitles = {};
    dataPlots = {};
    dataTitles = {};
    pValsPlots = {};
    pValsTitles = {};
    
    if(bThisDomainOnly)
        VarD = {'domains'};
        ValD = {num2str(domainID)};
    else
        VarD = {};
        ValD = {};
    end
    
    if(~bFull)
        %===================================================
        % Cols (c, Var1) | Rows (r, Var2) - with other vars.
        %===================================================
        c = 1; r = 1;
        %-------------------------
        %-- Plot Data
        %-------------------------
        if(~bPlotpValOnly)
            for c = 1:length(Val1)
                if ~isempty(Val2) && length(Val2) > 1
                    for r = 1:length(Val2)                    
                        curDataVars = cat(2, VarD, Var1{c}, Var2{r}, Var3, Var4);
                        curDataVals = cat(2, ValD, Val1{c}, Val2{r}, Val3, Val4);
                        dataPlotIDs{r,c} = StaR_getVarValPlots(curDataVars, curDataVals, df, 'data');
                        if(~isempty(dataPlotIDs{r,c}))
                            dataPlots{r,c} =  df.data{dataPlotIDs{r,c}}.plotVal;
                            dataTitles{r,c} = df.data{dataPlotIDs{r,c}}.lbl;
                        end
                    end
                else
                    curDataVars = cat(2, VarD, Var1{c}, Var2{r}, Var3, Var4);
                    curDataVals = cat(2, ValD, Val1{c}, Val2{r}, Val3, Val4);
                    dataPlotIDs{r, c} = StaR_getVarValPlots(curDataVars, curDataVals, df, 'data');

                    if ~isempty(dataPlotIDs{1, c})
                        dataPlots{1, c} =  df.data{dataPlotIDs{1, c}}.plotVal;
                        dataTitles{1, c} = df.data{dataPlotIDs{1, c}}.lbl;
                    end     
                end
            end
        end
        
        %-------------------------
        %-- Plot pVals
        %-------------------------
        if(~bPlotDataOnly)
            r = 1; c = 1;
            if ~isempty(Val1)
                %--------------
                % pVals - Cols
                %--------------
                if (isempty(Val2) || isempty(Val2{1}) || length(Val2) <= 1)
                    %==============
                    % 1 Var...
                    %==============
                    
                    if length(Val1) == 2                                                
                        %--------------
                        % 2 Factors...
                        %--------------
                        disp('1 Variable, 2 Factors');
                        
                        % Get pVals Plots Combining Vars / Vals.
                        curPValsVars = cat(2, VarD, {Var1{1} Var1{2}}, Var2, Var3, Var4);
                        curDataVals = cat(2, ValD, {Val1{1}, Val1{2}}, Val2, Val3, Val4);
                        pValsPlotIDs{r, c} = StaR_getVarValPlots(curPValsVars, curDataVals, df, 'pValsSub');

                        % Prep Polts & Titles.
                        if ~isempty(pValsPlotIDs{r, c})
                            [pValsPlots, pValsTitles] = prepPValsPlots(dataPlots, dataTitles, pValsPlots, pValsTitles, 1, 1, df, pValsPlotIDs{r, c}, bPCorrected, bPSignificants, bPValsTitles, pSignifThreshold, bPlotDiffMask);     
                        end
                    end
                    
                    if length(Val1) == 3
                        %--------------
                        % 3 Factors...
                        %--------------
                        disp('1 Variable, 3 Factors');
                        
                        c = 1;
                        % Get pVals Plots Combining Vars / Vals.
                        curPValsVars{c} = cat(2, VarD, {Var1{1} Var1{2}}, Var2, Var3, Var4);
                        curDataVals{c} = cat(2, ValD, {Val1{1}, Val1{2}}, Val2, Val3, Val4);
                        
                        c = 2;
                        % Get pVals Plots Combining Vars / Vals.
                        curPValsVars{c} = cat(2, VarD, {Var1{2} Var1{3}}, Var2, Var3, Var4);
                        curDataVals{c} = cat(2, ValD, {Val1{2}, Val1{3}}, Val2, Val3, Val4);
                        
                        c = 3;
                        % Get pVals Plots Combining Vars / Vals.
                        curPValsVars{c} = cat(2, VarD, {Var1{1} Var1{3}}, Var2, Var3, Var4);
                        curDataVals{c} = cat(2, ValD, {Val1{1}, Val1{3}}, Val2, Val3, Val4);
                        
                        for c = 1:3
                            pValsPlotIDs{r,c} = StaR_getVarValPlots(curPValsVars{c}, curDataVals{c}, df, 'pValsSub');

                            % Prep Polts & Titles.
                            if ~isempty(pValsPlotIDs{r,c})
                                [pValsPlots, pValsTitles] = prepPValsPlots(dataPlots, dataTitles, pValsPlots, pValsTitles, 2, c, df, pValsPlotIDs{r,c}, bPCorrected, bPSignificants, bPValsTitles, pSignifThreshold, bPlotDiffMask);     
                            end
                        end
                    end
                end 
            end
            
            
            if ~isempty(Val2) && ~isempty(Val2{1}) && length(Val2) > 1
                %==============
                % 2 Vars...
                %==============
                for c = 1:length(Val1)
                    % Get pVals Plots Combining Vars / Vals.
                    curPValsVars = cat(2, VarD, Var1{c}, Var2{:}, Var3, Var4);
                    curDataVals = cat(2, ValD, Val1{c}, Val2{:}, Val3, Val4);
                    pValsPlotIDs{2, c} = StaR_getVarValPlots(curPValsVars, curDataVals, df, 'pValsSub');

                    % Prep Polts & Titles.
                    if ~isempty(pValsPlotIDs{2, c})
                        [pValsPlots, pValsTitles] = prepPValsPlots(dataPlots, dataTitles, pValsPlots, pValsTitles, 2, c, df, pValsPlotIDs{2, c}, bPCorrected, bPSignificants, bPValsTitles, pSignifThreshold, bPlotDiffMask);     
                    end
                end
                    
                for r = 1:length(Val2) 
                    % Get pVals Plots Combining Vars / Vals.
                    curPValsVars = cat(2, VarD, Var1{:}, Var2{r}, Var3, Var4);
                    curPValsVals = cat(2, ValD, Val1{:}, Val2{r}, Val3, Val4);
                    pValsPlotIDs{1,r} = StaR_getVarValPlots(curPValsVars, curPValsVals, df, 'pValsSub');

                    % Prep Polts & Titles.
                    if ~isempty(pValsPlotIDs{1,r})
                        [pValsPlots, pValsTitles] = prepPValsPlots(dataPlots, dataTitles, pValsPlots, pValsTitles, 1, r, df, pValsPlotIDs{1,r}, bPCorrected, bPSignificants, bPValsTitles, pSignifThreshold, bPlotDiffMask); 
                    end
                end
            end
        end
    end
          
    % pValsFull 
    if(bFull)
        for p = 1:length(df.pValsFull)

            nbEffects = length(find(char(df.pValsFull{p}.lbl) == ':'))
            nbEffects = nbEffects + 1;

            pValsFullTitles{nbEffects}{p} = df.pValsFull{p}.lbl;

            if bPCorrected
                pValsFullPlots{nbEffects}{p} = df.pValsFull{p}.plotValCorrected;
            else
                pValsFullPlots{nbEffects}{p} = df.pValsFull{p}.plotVal;
            end

            if bPSignificants
                pValsFullPlots{nbEffects}{p}(pValsFullPlots{nbEffects}{p} < pSignifThreshold) = 2;
                pValsFullPlots{nbEffects}{p}(pValsFullPlots{nbEffects}{p} ~= 2) = 0;
            end       
        end
    end
    
    
    if(~bFull)
        % Titles can become quite long and difficult to read - you can shorten
        % them, keeping only first 2 letters of the variable name.
        if bShortTitles
            dataTitles = getShortTitles(dataTitles);
            pValsTitles = getShortTitles(pValsTitles);
        end
    end
    
    mixPlots = dataPlots;
    mixTitles = dataTitles;
    if(size(mixTitles) ~= size(mixPlots))
        disp('## ERROR ## Sizes Mismatch between plots and titles'); 
    end
        
    if(~bFull && ~bPlotDataOnly)
        % ------------------------------------
        % Plot Sub Data + pVals from Sub Data.
        % ONLY if pFull == false
        % The same function is being used!
        % ------------------------------------
        % Combine plots.
        for d1 = 1:length(pValsPlots(1,:))
            mixPlots{d1, size(dataPlots, 2) + 1} = pValsPlots{1, d1};
            mixTitles{d1, size(dataTitles, 2) + 1} = pValsTitles{1, d1};
        end

        if(size(pValsPlots, 1) > 1)
            for d2 = 1:length(pValsPlots(2,:))
                mixPlots{size(dataPlots, 1) + 1, d2} = pValsPlots{2, d2};
                mixTitles{size(dataTitles, 1) + 1, d2} = pValsTitles{2, d2};
            end
        end
    end

    if (length(dataPlots) == 0 && ~bFull) || (bFull && length(pValsFullPlots) == 0)
        disp('There is nothing to plot... Please select something else.');
        return;
    else
        if (strcmp(obj.measureLabel, 'ERSP'))
            disp(['Plotting ERSP Domain #' num2str(domainID)]);
            
            if bFull == false
                std_plottf(df.timeData, df.freqData, mixPlots, 'titles', mixTitles);%, varargin{:});
            else
                for nbEffects = 1:length(pValsFullPlots)
% TODO: Split the Effects when > 4
%                     if(length(pValsFullPlots{nbEffects}) > 4)
%                         nbPlotsPerRow = 4;
%                         if (mod(length(pValsFullPlots{nbEffects}), 3) == 0), nbPlotsPerRow = 3; end
%                         for r = 1:ceil(length(pValsFullPlots{nbEffects})/nbPlotsPerRow)
%                             firstId = (r - 1) * nbPlotsPerRow + 1; lastId = min((r - 1) * nbPlotsPerRow + nbPlotsPerRow, length(pValsFullPlots{nbEffects}));
%                             tmpEffectsPlots{r , :} = pValsFullPlots{nbEffects}{firstId:lastId}{:};
%                             tmpEffectsTitles{r , :} = pValsFullTitles{nbEffects}{firstId:lastId}{:};
%                         end
%                         
%                     else
                    tmpEffectsPlots = pValsFullPlots{nbEffects};
                    tmpEffectsTitles = pValsFullTitles{nbEffects};
%                     end

                    std_plottf(df.timeData, df.freqData, tmpEffectsPlots, 'titles', tmpEffectsTitles);
                end
            end
        end

        if (strcmp(obj.measureLabel, 'ERP'))
            disp(['ERP! Domain #' num2str(domainID)]);
            %if length(measureAsArrayInCell) <= numberOfConditionsInEachFigure
                % put ERP traces together inone plot;
                signifStyles = {'ok', 'sk', 'dk', '.k'};
                 
                % Find Min Value.
                minValue = 0;
                maxValue = 0;
                for c = 1:size(dataPlots,1)
                    for r = 1:size(dataPlots,2)
                        minValue = min(minValue, min(min(dataPlots{c,r})));
                        maxValue = max(maxValue, max(max(dataPlots{c,r})));
                    end
                end
                spaceBetweenLines = (maxValue - minValue) / 20;
                
                nbPLinesTotal = size(pValsPlots,1) + size(pValsPlots,2);
                
                disp('ERP Style #1');
                pr.std_plotcurve(df.timeData, mixPlots, 'titles', mixTitles, 'datatype', 'erp', 'figure', 'on');
                
                %disp('ERP Style #2');
                %pr.std_plotcurve(df.timeData, dataPlots, 'titles', dataTitles, 'datatype','erp', 'plotconditions', 'apart','plotgroups', 'together', 'figure', 'on');%, varargin{:});
                %disp('ERP Style #3');
                %pr.std_plotcurve(df.timeData, dataPlots, 'titles', dataTitles, 'datatype','erp', 'plotconditions', 'together','plotgroups', 'apart', 'figure', 'on');%, varargin{:});
                
                disp('ERP Style #4');
                pr.std_plotcurve(df.timeData, dataPlots, 'titles', dataTitles, 'datatype','erp', 'plotconditions', 'together','plotgroups', 'together', 'figure', 'on', 'ylim', [minValue - ((nbPLinesTotal + 2) * spaceBetweenLines), maxValue + (2 * spaceBetweenLines)]);%, varargin{:});                
                
                nbPLines = 1;
                for i = 1:size(pValsPlots,1)
                    for j = 1:size(pValsPlots,2)     
                        tmpPVals = pValsPlots{i,j};
                        tmpPVals(tmpPVals == 0) = NaN;
                        tmpPVals(~isnan(tmpPVals)) = minValue - (nbPLines * spaceBetweenLines);
                        
                        plot(tmpPVals, signifStyles{nbPLines});
                        
                        nbPLines = nbPLines + 1;
                    end
                end
                
                % put legend for conditions
                legend({dataTitles{:}, pValsTitles{:}});

                % make lines thicker.
                set(findobj(gcf, 'type', 'Line'), 'linewidth', 2);
        end
        %plot(df{plotIDs(i)}.plotVal);

        %figureHandle = gfc;
    end
    
    % ------------------------------------
    
    % ------------------------------------
end


function [pValsPlots, pValsTitles] = prepPValsPlots(dataPlots, dataTitles, pValsPlots, pValsTitles, row, col, df, plotID, bPCorrected, bPSignificants, bPValsTitles, pSignifThreshold, pDiffMask)
    if bPCorrected
        pValsPlots{row, col} =  df.pValsSub{plotID}.plotValCorrected;
    else
        pValsPlots{row, col} =  df.pValsSub{plotID}.plotVal;
    end

    if bPSignificants && ~isempty(pValsPlots{row, col})
        pValsPlots{row, col}(pValsPlots{row, col} < pSignifThreshold) = 1;
        pValsPlots{row, col}(pValsPlots{row, col} ~= 1) = 0;
    end
    
    if ~bPSignificants 
        pValsPlots{row, col} = 1 - pValsPlots{row, col};            % Inverse for Significance to be colored.
                
        if ~isempty(dataPlots)
            maxValue = 1;

            % Find Max Value.
            for c = 1:size(dataPlots,1)
                for r = 1:size(dataPlots,2)
                    maxValue = max(maxValue, max(max(dataPlots{c,r})))                
                end
            end

            disp(['Adjusting Scaling : Multiplying pVals by ' num2str(maxValue)]);

            % Adjust Values.
            pValsPlots{row, col} = pValsPlots{row, col} .* maxValue;    % Adjust Scale According to other graph.
        end
    end
    
    % Use the Diff Between Plot 1 & 2 and Mask it.
    if pDiffMask && ~isempty(dataPlots) && row <= size(dataPlots, 1) && col <= size(dataPlots, 2) && ~isempty(dataPlots{row, col})
        
        if row == 1 % Rows
            if size(dataPlots, 2) ~= 2, disp('### ERROR : Using Diff Mask for pVals, but with size > 2'); end
            diffPlot = dataPlots{col, 1} - dataPlots{col, 2};
        end
        if row == 2 % Cols
            if size(dataPlots, 1) ~= 2, disp('### ERROR : Using Diff Mask for pVals, but with size > 2'); end
            diffPlot = dataPlots{1, col} - dataPlots{2, col};
        end
        
        diffPlot(pValsPlots{row, col} == 0) = 0;
        pValsPlots{row, col} = diffPlot;
    end

    if bPValsTitles
        pValsTitles{row, col} = df.pValsSub{plotID}.lbl;
    else
        pValsTitles{row, col} = 'pVals';
    end
end


%% getVarString()
%
% Inputs:
%   - variables:
%   - strDesign:
%   - bAll:
%
% Outputs:
%   - nbVars:
%   - varString:
%
% Description:
%
%
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

%% getValString()
%
% Inputs:
%   - vals:
%
% Outputs:
%   - valString:
%
% Description:
%
%
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

%% cleanVarVals()
%
% Inputs:
%   - vars:
%   - vals:
%
% Outputs:
%   - vars:
%   - vals:
%
% Description:
%
%
function [vars, vals] = cleanVarVals(vars, vals)
    for i = 1:length(vars)
        if (strcmp(vars{i}, 'N/A') == 1) || (strcmp(vals{i}, 'N/A') == 1)
            vars{i} = [];
            vals{i} = [];
        end
    end
end


%% getShortTitles()
%
% --- Inputs ---
%   - titles:
%
% --- Outputs ---
%   - shortTitles:
%
% --- Description ---
% Shitty Code to Shorten the Titles with only the First 2 Letters. (e.g. Groups=3 -> Gr=3)
% TODO : Handle the potential problems to avoid crashes.
function shortTitles = getShortTitles(titles)
    
    shortTitles = {};
    sepChar = '|';
    
    for i = 1:size(titles,1)
        for j = 1:size(titles, 2)
            if (strfind(titles{i,j}, '-') < 1)
                comb1 = strsplit(titles{i,j}, '|');
                sepChar = '|';
            else
                comb1 = strsplit(titles{i,j}, '-');
                sepChar = '-';
            end
             shortTitle1 = '';
             for k = 1:length(comb1)
                 comb2 = strsplit(comb1{k}, ';');
                 shortTitle2 = '';
                 for l = 1:length(comb2)
                     tmpComb = strsplit(comb2{l}, '=');
                     if length(tmpComb) == 2
                         tmpComb{1} = strtrim(tmpComb{1});
                         tmpComb{2} = strtrim(tmpComb{2});
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
                     shortTitle1 = shortTitle2;
                 else
                     shortTitle1 = [shortTitle1 sepChar shortTitle2];
                 end
             end
             if isempty(shortTitle1)
                 shortTitles{i,j} = titles{i,j}; % put back the title if you could find separation!
             else
                shortTitles{i,j} = shortTitle1; % put the new 'short' title.
             end
        end    
    end
    
    % Shorten the titles by grouping similar term together.
    % The 'hard part' is to deal with the ';'
    for i = 1:size(shortTitles,1)
        for j = 1:size(shortTitles, 2)
            strRemovedVarVals = '';
            strSplits = strsplit(shortTitles{i,j}, '-');
            if length(strSplits) == 2
                strSubs{1} = strsplit(strSplits{1}, ';');
                strSubs{2} = strsplit(strSplits{2}, ';');
                if(size(strSubs{1},1) == size(strSubs{2},1))
                    for k = 1:length(strSubs{1})
                        disp([strSubs{1}(k) ' vs ' strSubs{2}(k)]);
                        
                        if(strcmp(strtrim(strSubs{1}(k)), strtrim(strSubs{2}(k))))
                            strRem = strtrim(strSubs{1}(k));
                            
                            %disp(['Removing: ' strRem]);
                            
                            strSubs{1}(k) = strtrim(strSubs{1}(k));
                            strSubs{2}(k) = strtrim(strSubs{2}(k));
                            
                            strSubs{1}(k) = strrep(strSubs{1}(k), strRem, ''); 
                            strSubs{2}(k) = strrep(strSubs{2}(k), strRem, '');
                              
                            if isempty(strRemovedVarVals)
                                strRemovedVarVals = strRem;
                            else
                                strRemovedVarVals = strcat(strRemovedVarVals, ';', strRem);
                            end
                        end
                    end
                end
                
                curStrSub{1} = '';
                curStrSub{2} = '';
                for s = 1:2
                    for curSubInd = 1:length(strSubs{s})
                        if ~isempty(char(strSubs{s}(curSubInd)))
                            if isempty(curStrSub{s})
                                curStrSub{s} = char(strSubs{s}(curSubInd));
                            else
                                strcat(curStrSub{s}, char(strSubs{s}(curSubInd)));
                            end
                        end
                    end           
                end

                if ~isempty(strRemovedVarVals)
                    shortTitles{i,j} = strcat(curStrSub{1}, ' -  ', curStrSub{2}, ' (', strRemovedVarVals, ')');
                end
            
            shortTitles{i,j} = char(shortTitles{i,j});
            end
        end
    end
end

function exportMPT(STUDY)
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
end

% % function plotStaR(obj, linearProjectedMeasureForCombinedCondition, headGrid, regionOfInterestCube, projectionParameter, twoConditionLabelsForComparison, significanceLevelForConditionDifference, usePositionProjections, statisticsParameter, plottingParameter)
% %     % plotConditionDifference(linearProjectedMeasureForCombinedCondition, headGrid, regionOfInterestCube, projectionParameter, twoConditionLabelsForComparison, significanceLevelForConditionDifference, statisticsParameter, plottingParameter)
% %     %
% %     % plot condition difference between first and second specified conditions (A-B) and
% %     % mask areas with non-significent differences.
% % 
% %     % if no condition label is provided, use the first two (if they exist)
% %     if nargin < 5 || isempty(twoConditionLabelsForComparison)
% %         if length(obj.conditionLabel) > 1
% %             twoConditionIdsForComparison = [1 2];
% %             twoConditionLabelsForComparison = obj.conditionLabel;
% % 
% %             % when there are more than two conditions present, give a warning
% %             if length(obj.conditionLabel) > 2
% %                 fprintf('Warning: there are more than two conditons present, by default only the difference between the first two is displayed. You can assign another condition pair in the input.\n');
% %             end;
% %         else
% %             error('There is only one condition present, at least two conditions are needed for a comparison\n');
% %         end;
% %     else % if condition labels for comparison are provided, then find condition ids in obj.conditionLabel that are associated with them.                
% %         if length(twoConditionLabelsForComparison) < 2
% %             error('Number of provided condition labels is less than required two (2).\n');
% %         end;
% % 
% %         % find condition ids in obj.conditionLabel that are associated with provided conditon labels.
% %         for i=1:2
% %             twoConditionIdsForComparison(i) = find(strcmp(obj.conditionLabel, twoConditionLabelsForComparison{i}));
% %         end;
% %     end
% % 
% %     if nargin < 6
% %         significanceLevelForConditionDifference = 0.03;                
% %     end;
% % 
% %     if nargin < 7
% %         usePositionProjections = 'mean'; % default is mean since we currently do not have a good way to incorporate session dipole denisties when 'each' statistics is used.
% %     end;
% % 
% %     %if nargin < 8               
% %         statisticsParameter = {};
% %     %end;
% % 
% %     %if nargin < 9               
% %         plottingParameter = {};
% %     %end;
% % 
% %     % ================================================
% %     % YR : Load R Stats.
% %     loadRStats = 'N';
% %     %loadRStats = input('Do you want to load R Values? Y/N [N]:','s');
% %     if loadRStats == 'Y' || loadRStats == 'y'
% %         disp('YR : Loading R Values...');
% %         load('RVals.mat');
% %         size(R_Pvals)
% %         R_Pvals = reshape(R_Pvals, 100, 200)';
% % 
% %         R_Pvals(R_Pvals > 0.5) = 0.5;
% % 
% %         Pvals = {R_Pvals};  
% %         title = {'R P Values'};
% %         obj.plotMeasureAsArrayInCell(Pvals, title, plottingParameter{:}); 
% %     end
% %     % !!! DEBUG !!!
% %     % ================================================
% % 
% %     % separate conditions from linearized form
% %     projectedMeasure = obj.getSeparatedConditionsForLinearizedMeasure(linearProjectedMeasureForCombinedCondition);
% % 
% %     % only keep the two conditions to be compared
% %     projectedMeasure = projectedMeasure(twoConditionIdsForComparison);
% % 
% %     % calculate difference statistics for two conditions
% %     [differenceBetweenConditions significanceValue] = obj.getMeasureDifferenceAcrossConditions(headGrid, regionOfInterestCube, projectionParameter, twoConditionIdsForComparison, usePositionProjections,statisticsParameter{:});
% % 
% %     % calculate difference between the two conditions and add as the third measure for
% %     % plotting
% %     projectedMeasure{3} = projectedMeasure{1} - projectedMeasure{2};
% % 
% %     % mask insignificent differences
% %     % -- YR added --
% %     if loadRStats == 'Y' || loadRStats == 'y'
% %         disp('YR : Masking with P Values...');
% %         projectedMeasure{4} = projectedMeasure{3};
% %         projectedMeasure{4}(R_Pvals > significanceLevelForConditionDifference) = 0;
% %     end
% %     % -- /YR added --
% %     projectedMeasure{3}(significanceValue > significanceLevelForConditionDifference) = 0;
% % 
% %     % since when plotting, the mean spectra is automatically added for spectra, we should
% %     % add the negative of it so it is canccled out.
% %     if strcmp(obj.measureLabel, 'Spec')
% %         projectedMeasure{3} = projectedMeasure{3} - obj.specMeanOverAllIcAndCondition;
% %     end;
% % 
% %     % set the titles
% %     conditionTitle = [obj.conditionLabel{twoConditionIdsForComparison(1)} ' - ' obj.conditionLabel{twoConditionIdsForComparison(2)} ' (p<' num2str(significanceLevelForConditionDifference) ')'];
% % 
% %     title = [obj.conditionLabel(twoConditionIdsForComparison), conditionTitle];
% %     % -- YR added --
% %     if loadRStats == 'Y' || loadRStats == 'y'
% %         title = [title, 'R Stats'];
% %     end
% %     % -- /YR added --
% % 
% %     % plot
% %     obj.plotMeasureAsArrayInCell(projectedMeasure, title, plottingParameter{:});
% % 
% %     % -- YR added --
% %     % !!! DEBUG !!!
% %     if loadRStats == 'Y' || loadRStats == 'y'
% %         Pvals = {significanceValue significanceValue};  
% %         title = {'MPT P Values', 'MPT P Values'};
% %         obj.plotMeasureAsArrayInCell(Pvals, title, plottingParameter{:});               
% %     end
% %     % !!! DEBUG !!!
% %     % -- /YR added --
% % end


%                 var1pVals = {};
%                 var2pVals = {};
%                 
%                 if(~bPlotDataOnly)
%                     for curPlotV1 = 1:length(Val1)                    
%                         var1pVals{curPlotV1} = mixPlots{curPlotV1, size(mixPlots,2)};
%                         var1Titles{curPlotV1} = mixTitles{curPlotV1, size(mixTitles,2)};
%                         
%                         mixPlots{curPlotV1, size(mixPlots, 2)} = {};
%                         mixTitles{curPlotV1, size(mixTitles, 2)} = {};
%                     end
%                     
%                     for curPlotV2 = 1:length(Val2)                    
%                         var2pVals{curPlotV2} = mixPlots{size(mixPlots,1), curPlotV2};
%                         var2Titles{curPlotV2} = mixTitles{size(mixTitles,1), curPlotV2};
%                         
%                         mixPlots{size(mixPlots, 1), curPlotV2} = {};
%                         mixTitles{size(mixTitles, 1), curPlotV2} = {};
%                     end
%                     
%                     for d1 = 1:length(Val1)
%                         for d2 = 1:length(Val2)
%                             dataPlots{d1,d2} = mixPlots{d1,d2};
%                             dataTitles{d1,d2} = mixTitles{d1,d2};
%                         end
%                     end
%                     mixPlots = dataPlots;
%                     mixTitles = dataTitles;
%                 end
                
                %pr.std_plotcurve(df.timeData, mixPlots, 'titles', mixTitles, 'datatype','erp', 'groupstats', var1pVals, 'condstats', var2pVals, 'plotconditions', 'apart','plotgroups', 'apart', 'figure', 'on');%, varargin{:});
                
end
% Show Stats! ---- The Good Stuff Happens Here! ---- 

