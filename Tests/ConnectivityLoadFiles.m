NB_SUBJECTS = 4;
NB_SESSIONS = 1;
NB_CONDITIONS = 2;
NB_GROUPS = 2;

CONDITIONS = {'FOF','SOF','FOM','SOM'};
CONNECTIVITY_SRC_FOLDER = '/media/user/BrainData/UdeM/';

filesToLoad = {};
i = 1;
for group = 3:3+NB_GROUPS-1
    for subject = 1:NB_SUBJECTS
        curFolder = sprintf('%d-%02d', group, subject);
        if exist(curFolder, 'dir')               
            cd(curFolder) % Open curFolder.

            disp(['-Group: ' num2str(group) ' Subject: ' num2str(subject)]);
            
            files_regname = sprintf([CONNECTIVITY_SRC_FOLDER '*%d-%02d*_SIFT.mat'], group, subject);
            files = dir(files_regname); % List all the files
            
            for f = 1:length(files)
                curFile = [CONNECTIVITY_SRC_FOLDER files(f).name];
                
                disp(['--Group: ' num2str(group) ' Subject: ' num2str(subject) ' File:' curFile]);
                copyfile(curFile,'.')
            end
            
%             length(files)
%             disp(files_regname);
%             disp(['group:' num2str(group) ' subject:' num2str(subject) ' lenght:' num2str(length(files))]);
%             for f = 1:length(files)
%                 disp(files(f));
%             end
%             
            %for session = 1:NB_SESSIONS
            %   for condition = 1:NB_CONDITIONS
            %       % Get the file name, depending on
            %       % session/group/subject/condition
            %       curFile = sprintf('%d_%d-%02d_%d-%s', session, group, subject, condition, CONDITIONS{condition});
            %       if exist([curFile '.set'], 'file')
            %           filesToLoad{1, i} = {'index' i 'load' [curFolder '/' curFile '.set'] 'subject' sprintf('%d_%d-%02d', session, group, subject) 'session' 1};
            %           
            %           % To count the number of files.
            %           i = i + 1;
            %       else
            %           disp(['The curFile : ' curFile ' doesnt exist']);            
            %       end;
            %   end;
            %end;

            cd ..   % Go back to folders.
        else
            disp(['The curFolder : ' curFolder ' doesnt exist']);
        end;
    end;
end;

% if ~isempty(filesToLoad)
%     filesToLoad{1, i} = {'inbrain' 'on' 'dipselect' 0.15};
%     
%     [ALLEEG EEG CURRENTSET ALLCOM] = eeglab;
%     pop_editoptions( 'option_storedisk', 1, 'option_savetwofiles', 1, 'option_saveversion6', 1, 'option_single', 0, 'option_memmapdata', 0, 'option_eegobject', 0, 'option_computeica', 1, 'option_scaleicarms', 1, 'option_rememberfolder', 1, 'option_donotusetoolboxes', 0, 'option_checkversion', 1, 'option_chat', 0);
%     [STUDY ALLEEG] = std_editset( STUDY, ALLEEG, 'name','mTBI_Study','commands', filesToLoad, 'updatedat','off');
%     
%     [STUDY ALLEEG] = std_checkset(STUDY, ALLEEG);
%     [STUDY EEG] = pop_savestudy( STUDY, EEG, 'filename','Study_mTBI.study','filepath','/home/user/Documents/Playground/Study_mTBI/');
% 
%     CURRENTSTUDY = 1; EEG = ALLEEG; CURRENTSET = [1:length(EEG)];
%     eeglab redraw;
% end;