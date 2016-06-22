% pop_specifdipole(): plots dipoles by groups x sessions from MPT domains.
%       

% Author: Yannick Roy, UdeM
% 
% === HISTORY ===
% 06/13/2016 ver 0.01 by Yannick. Creation of the Dialog Box (UI).
% 06/24/2016 ver 0.02 by Yannick. Legend Added.
% === /HISTORY ===
%
% === KNOW ISSUES & TODO ===
% Domain doesn't select by default.
% === /KNOW ISSUES ===
%
% === NOTES ===
%
% The following code must be added to create_mpt_submenu.m  
% (~ line 300 in 14.4.4b)
%
% % % function command_measure_show_domain_specifdipoles(callerHandle, evnt, measureName)
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
% % %             study_local = pop_specifdipoles(STUDY, ALLEEG, STUDY.measureProjection.(measureName), domainIndex) 
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
% % % uimenu(domain_menu,'Label', 'Show Specific Dipoles','Callback',{@command_measure_show_domain_specifdipoles, measureString},'tag',[measureString 'SpecificDipolesForDomain ' num2str(j)], 'userdata', 'study:on');


function STUDY = pop_specifdipoles(STUDY, ALLEEG, obj, curDomain)

com = '';
if nargin < 2
    help pop_specifdipoles;
    return;
end;

% create domains string
domainString = '';
if length(obj.projection.domain)>1
    for n = 1:length(obj.projection.domain)
        if isempty(domainString)
            domainString = obj.projection.domain(1,n).label;
        else
            domainString = [domainString '|'  obj.projection.domain(1,n).label];;
        end
    end
else
    domainString = 'N/A';
end

% groups in the STUDY (not checking the current design...)
groupString = '';
if length(STUDY.group)>1
    for n = 1:length(STUDY.group)
        if isempty(groupString)
            groupString = STUDY.group{n};
        else
            groupString = [groupString '|'  STUDY.group{n}];
        end
    end
    groupString = [groupString '|' 'All'];
else
    groupString = 'N/A';
end
disp('ToDo: Check if all the groups are in the current design!!!');

% sessions in the STUDY (not checking the current design...)
sessionString = '';
if length(STUDY.session)>1
    for n = 1:length(STUDY.session)
        if isempty(sessionString)
            sessionString = num2str(STUDY.session(n));
        else
            sessionString = [sessionString '|'  num2str(STUDY.session(n))];
        end
    end
    sessionString = [sessionString '|' 'All'];
else
    sessionString = 'N/A';
end
disp('ToDo: Check if all the sessions are in the current design!!!');

nbPlots = 6;
nbPlotsValid = 1;
nbSessions = length(STUDY.session);
nbGroups = length(STUDY.group);

% collect user input
try
    nRows = 8;
    nCols = 5;
   userInput = inputgui('title', 'pop_specifDipoles()', 'geom', ...
       {{nCols nRows [0 0] [1 1]} {nCols nRows [1 0] [1 1]} {nCols nRows [2 0] [1 1]} {nCols nRows [3 0] [1 1]} {nCols nRows [4 0] [1 1]} ...
        {nCols nRows [0 1] [1 1]} {nCols nRows [1 1] [1 1]} {nCols nRows [2 1] [1 1]} {nCols nRows [3 1] [1 1]} {nCols nRows [4 1] [1 1]} ...
        {nCols nRows [0 2] [1 1]} {nCols nRows [1 2] [1 1]} {nCols nRows [2 2] [1 1]} {nCols nRows [3 2] [1 1]} {nCols nRows [4 2] [1 1]} ...
        {nCols nRows [0 3] [1 1]} {nCols nRows [1 3] [1 1]} {nCols nRows [2 3] [1 1]} {nCols nRows [3 3] [1 1]} {nCols nRows [4 3] [1 1]} ...
        {nCols nRows [0 4] [1 1]} {nCols nRows [1 4] [1 1]} {nCols nRows [2 4] [1 1]} {nCols nRows [3 4] [1 1]} {nCols nRows [4 4] [1 1]} ...
        {nCols nRows [0 5] [1 1]} {nCols nRows [1 5] [1 1]} {nCols nRows [2 5] [1 1]} {nCols nRows [3 5] [1 1]} {nCols nRows [4 5] [1 1]} ...
        {nCols nRows [0 6] [1 1]} {nCols nRows [1 6] [1 1]} {nCols nRows [2 6] [1 1]} {nCols nRows [3 6] [1 1]} {nCols nRows [4 6] [1 1]} ...
        {nCols nRows [0 7] [1 1]} {nCols nRows [4 7] [1 1]} 
        }, ... 
    'uilist',...
       {{'style' 'text' 'string' ' '} {'style' 'text' 'string' 'Domain        '} {'style' 'text' 'string' 'Group        '} {'style' 'text' 'string' 'Session      '} {'style' 'text' 'string' 'Color        '} ...
        {'style' 'text' 'string' '           Plot 1:           '}  {'style' 'popupmenu' 'string' domainString 'tag' 'domain' 'value' curDomain} {'style' 'popupmenu' 'string' groupString 'tag' 'group' 'value' 1} {'style' 'popupmenu' 'string' sessionString 'tag' 'session' 'value' 1} {'style' 'popupmenu' 'string' 'N/A|White|Yellow|Fuchsia|Red|Silver|Gray|Olive|Purple|Maroon|Aqua|Lime|Teal|Green|Blue|Navy|Black' 'tag' 'color' 'value' 3} ...
        {'style' 'text' 'string' '           Plot 2:           '}  {'style' 'popupmenu' 'string' domainString 'tag' 'domain' 'value' curDomain} {'style' 'popupmenu' 'string' groupString 'tag' 'group' 'value' 1} {'style' 'popupmenu' 'string' sessionString 'tag' 'session' 'value' 2} {'style' 'popupmenu' 'string' 'N/A|White|Yellow|Fuchsia|Red|Silver|Gray|Olive|Purple|Maroon|Aqua|Lime|Teal|Green|Blue|Navy|Black' 'tag' 'color' 'value' 1} ...
        {'style' 'text' 'string' '           Plot 3:           '}  {'style' 'popupmenu' 'string' domainString 'tag' 'domain' 'value' curDomain} {'style' 'popupmenu' 'string' groupString 'tag' 'group' 'value' 1} {'style' 'popupmenu' 'string' sessionString 'tag' 'session' 'value' 3} {'style' 'popupmenu' 'string' 'N/A|White|Yellow|Fuchsia|Red|Silver|Gray|Olive|Purple|Maroon|Aqua|Lime|Teal|Green|Blue|Navy|Black' 'tag' 'color' 'value' 1} ...
        {'style' 'text' 'string' '           Plot 4:           '}  {'style' 'popupmenu' 'string' domainString 'tag' 'domain' 'value' curDomain} {'style' 'popupmenu' 'string' groupString 'tag' 'group' 'value' 2} {'style' 'popupmenu' 'string' sessionString 'tag' 'session' 'value' 1} {'style' 'popupmenu' 'string' 'N/A|White|Yellow|Fuchsia|Red|Silver|Gray|Olive|Purple|Maroon|Aqua|Lime|Teal|Green|Blue|Navy|Black' 'tag' 'color' 'value' 1} ...
        {'style' 'text' 'string' '           Plot 5:           '}  {'style' 'popupmenu' 'string' domainString 'tag' 'domain' 'value' curDomain} {'style' 'popupmenu' 'string' groupString 'tag' 'group' 'value' 2} {'style' 'popupmenu' 'string' sessionString 'tag' 'session' 'value' 2} {'style' 'popupmenu' 'string' 'N/A|White|Yellow|Fuchsia|Red|Silver|Gray|Olive|Purple|Maroon|Aqua|Lime|Teal|Green|Blue|Navy|Black' 'tag' 'color' 'value' 1} ...
        {'style' 'text' 'string' '           Plot 6:           '}  {'style' 'popupmenu' 'string' domainString 'tag' 'domain' 'value' curDomain} {'style' 'popupmenu' 'string' groupString 'tag' 'group' 'value' 2} {'style' 'popupmenu' 'string' sessionString 'tag' 'session' 'value' 3} {'style' 'popupmenu' 'string' 'N/A|White|Yellow|Fuchsia|Red|Silver|Gray|Olive|Purple|Maroon|Aqua|Lime|Teal|Green|Blue|Navy|Black' 'tag' 'color' 'value' 1} ...
        {'style' 'text' 'string' 'YR Custom'} {'style' 'text' 'string' 'YR Custom 2'} });
catch
    disp('## ERROR ## pop_specifDipoles() cant load...');
end

% canceled
if isempty(userInput)
    return
end

% store userInput to STUDY (MPT.StaR)
STUDY.measureProjection.StaR = userInput;

% create colorIndex
colorIndex = [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16];

% 16 colors names officially supported by W3C specification for HTML
colors{1,1}  = [1 1 1];            % White
colors{2,1}  = [1 1 0];            % Yellow
colors{3,1}  = [1 0 1];            % Fuchsia
colors{4,1}  = [1 0 0];            % Red
colors{5,1}  = [0.75  0.75  0.75]; % Silver
colors{6,1}  = [0.5 0.5 0.5];      % Gray
colors{7,1}  = [0.5 0.5 0];        % Olive
colors{8,1}  = [0.5 0 0.5];        % Purple
colors{9,1}  = [0.5 0 0];          % Maroon
colors{10,1} = [0 1 1];            % Aqua
colors{11,1} = [0 1 0];            % Lime
colors{12,1} = [0 0.5 0.5];        % Teal
colors{13,1} = [0 0.5 0];          % Green
colors{14,1} = [0 0 1];            % Blue
colors{15,1} = [0 0 0.5];          % Navy
colors{16,1} = [0 0 0];            % Black

colorName{1,1}  = 'White';
colorName{2,1}  = 'Yellow';
colorName{3,1}  = 'Fuchsia';
colorName{4,1}  = 'Red';
colorName{5,1}  = 'Silver';
colorName{6,1}  = 'Gray';
colorName{7,1}  = 'Olive';
colorName{8,1}  = 'Purple';
colorName{9,1}  = 'Maroon';
colorName{10,1} = 'Aqua';
colorName{11,1} = 'Lime';
colorName{12,1} = 'Teal';
colorName{13,1} = 'Green';
colorName{14,1} = 'Blue';
colorName{15,1} = 'Navy';
colorName{16,1} = 'Black';

defaultColorName = colorName{5,1};
defaultColor = colors{5,1};

plotParams{1,1}.domain = userInput{1,1};
plotParams{1,1}.group   = userInput{1,2};
plotParams{1,1}.session   = userInput{1,3};
if userInput{1,4}>1
    plotParams{1,1}.color     = colors{colorIndex(userInput{1,4}-1)};
    plotParams{1,1}.colorName = colorName{colorIndex(userInput{1,4}-1)};
else
    plotParams{1,1}.color       = -1;
    plotParams{1,1}.colorName   = 'N/A';
end

plotParams{1,2}.domain = userInput{1,5};
plotParams{1,2}.group   = userInput{1,6};
plotParams{1,2}.session   = userInput{1,7};
if userInput{1,8}>1
    plotParams{1,2}.color     = colors{colorIndex(userInput{1,8}-1)};
    plotParams{1,2}.colorName = colorName{colorIndex(userInput{1,8}-1)};
else
    plotParams{1,2}.color       = -1;
    plotParams{1,2}.colorName   = 'N/A';
end

plotParams{1,3}.domain = userInput{1,9};
plotParams{1,3}.group   = userInput{1,10};
plotParams{1,3}.session   = userInput{1,11};
if userInput{1,12}>1
    plotParams{1,3}.color     = colors{colorIndex(userInput{1,12}-1)};
    plotParams{1,3}.colorName = colorName{colorIndex(userInput{1,12}-1)};
else
    plotParams{1,3}.color       = -1;
    plotParams{1,3}.colorName   = 'N/A';
end

plotParams{1,4}.domain = userInput{1,13};
plotParams{1,4}.group   = userInput{1,14};
plotParams{1,4}.session   = userInput{1,15};
if userInput{1,16}>1
    plotParams{1,4}.color     = colors{colorIndex(userInput{1,16}-1)};
    plotParams{1,4}.colorName = colorName{colorIndex(userInput{1,16}-1)};
else
    plotParams{1,4}.color       = -1;
    plotParams{1,4}.colorName   = 'N/A';
end

plotParams{1,5}.domain = userInput{1,17};
plotParams{1,5}.group   = userInput{1,18};
plotParams{1,5}.session   = userInput{1,19};
if userInput{1,20}>1
    plotParams{1,5}.color     = colors{colorIndex(userInput{1,20}-1)};
    plotParams{1,5}.colorName = colorName{colorIndex(userInput{1,20}-1)};
else
    plotParams{1,5}.color       = -1;
    plotParams{1,5}.colorName   = 'N/A';
end

plotParams{1,6}.domain = userInput{1,21};
plotParams{1,6}.group   = userInput{1,22};
plotParams{1,6}.session   = userInput{1,23};
if userInput{1,24}>1
    plotParams{1,6}.color     = colors{colorIndex(userInput{1,24}-1)};
    plotParams{1,6}.colorName = colorName{colorIndex(userInput{1,24}-1)};
else
    plotParams{1,6}.color       = -1;
    plotParams{1,6}.colorName   = 'N/A';
end

% Get Unique Domains from all the options.
tempDomains = [];
for i=1:nbPlots tempDomains = [tempDomains plotParams{1,i}.domain]; end
uniqueDomains = unique(tempDomains);

% Get nbPlotsValid.
nbPlotsValid = 0;
for i=1:nbPlots 
    if(~strcmp(plotParams{1,i}.colorName, 'N/A')) 
        nbPlotsValid = nbPlotsValid + 1; 
    end
end

% For each domain, for each dipole, check if it's in one of the nbPlots,
% if so -> color it!
for d=uniqueDomains
    domain = obj.projection.domain(d); 
    projection = obj.projection;
    [dipoleId sortedDipoleDensity orderOfDipoles dipoleDenisty dipoleDenistyInRegion] = obj.object.getDipoleDensityContributionToRegionOfInterest(domain.membershipCube, projection, [1 0.05])% the last value, [1 0.05]) indicates that we want all the ICs that at least has a 0.05 chance of being in the domain. You may want to use 0.1 or even 0.5 to get fewer ICs.

    % Get Sessions for Dipoles.
    sessions = {ALLEEG([obj.object.datasetId(dipoleId)]).session};

    % Get Groups for Dipoles.
    groups = obj.object.groupNumber(dipoleId);

    % Check that you have a group and a session for each contributing dipole! (dipoleId)
    if length(dipoleId) ~= length(sessions)
        disp('## ERROR ## dipoleId and sessions length are different... This should not happen!');
    end
    if length(dipoleId) ~= length(groups)
        disp('## ERROR ## dipoleId and groups length are different... This should not happen!');
    end

    nbDipoles = length(dipoleId);
    nbDipolesColored = 0;
    
    for i=1:length(dipoleId) dipolesColors(i,:) = defaultColor; end
    
    for i=1:length(dipoleId) % For each Dipole.
        for c=1:nbPlots % For each Color (plot row in UI)
            if(plotParams{1,c}.domain == d && ~strcmp(plotParams{1,c}.colorName, 'N/A')) % Check if same domain!
                disp(['D' num2str(d) ' - ID: ' num2str(dipoleId(i)) ' - Sessions: ' sessions{i} 'vs' num2str(plotParams{1,c}.session) ' |  Groups: ' num2str(groups(i)) 'vs' num2str(plotParams{1,c}.group)]);
                if((str2num(sessions{i}) == plotParams{1,c}.session || plotParams{1,c}.session == (nbSessions + 1))  && (groups(i) == plotParams{1,c}.group || plotParams{1,c}.group == (nbGroups + 1)))
                    dipolesColors(i,:) = plotParams{1,c}.color;
                    nbDipolesColored = nbDipolesColored + 1;
                    disp(['Yes - ' plotParams{1,c}.colorName]);
                else
                    disp('No');
                end
            end
        end
    end
    
    disp([num2str(nbDipolesColored) '/' num2str(nbDipoles) ' dipoles colored']);

    dipolesColorAsCell = {};
    for i=1:size(dipolesColors, 1) - 2
        dipolesColorAsCell{i} = dipolesColors(i,:);
    end;
    
    figure;
    plot_dipplot_with_cortex(obj.object.location(dipoleId,:), true, 'coordformat', 'MNI', 'gui', 'off', 'spheres', 'on', 'color', dipolesColorAsCell);
    %domain.plot_head_surface(domain.headGrid, domain.membershipCube, 'surfaceColor', inputOptions.surfaceColor, 'surfaceOptions', {'facealpha', 0.9});%inputOptions.surfaceAlpha});
    pr.plot_head_surface(domain.headGrid, domain.membershipCube, 'surfaceColor', [0.15 0.8 0.15], 'surfaceOptions', {'facealpha', 0.3});%inputOptions.surfaceAlpha}); 
    
    labels = {};
    for i=1:nbPlots 
        if ~strcmp(plotParams{1,i}.colorName, 'N/A') 
            tempDomains = strsplit(domainString, '|');
            tempGroups =  strsplit(groupString, '|');
            tempSessions = strsplit(sessionString, '|');
            tempLabel = strcat(tempDomains(plotParams{1,i}.domain), ' G', tempGroups(plotParams{1,i}.group), ' S', tempSessions(plotParams{1,i}.session));
            labels{i} = char(tempLabel);
        else
            labels{i} = 'N/A';
        end
    end
        
    % Legend with Colored Background + Face Marker.
    % l -> legend handle. Useful to "set" params.
    % o -> object handle. Useful to modify text and icons.
    % p -> plot handle. Not used.
    % t -> labels. Not used.
    [l,o,p,t] = legend(labels);
    for i=1:length(o)
        %length(o)/2
        %i
        %get(o(i))
        if(i <= length(o)/2)
            if( plotParams{1,i}.color ~= -1 )
                set(o(i),'BackgroundColor', plotParams{1,i}.color)
            else
                set(o(i),'BackgroundColor', [0 0 0]);
            end
        else
            i - (length(o)/2)
            if( plotParams{1, i - (length(o)/2)}.color ~= -1 )
                set(o(i),'FaceColor', plotParams{1, i - (length(o)/2)}.color)
            else
                set(o(i),'FaceColor', [0 0 0]);
            end
        end
    end
    set(l, 'Box', 'off')
end
