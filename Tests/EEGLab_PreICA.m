

current_folder = pwd;
destination_folder = ('~/Documents/Playground/JC_Study/Step1');

eeglab;

waittime = 0;
while waittime < 1800
    
    % Must be in a Temp folder.
    files = dir;
    
    for i=1:size(files, 1) % . | .. | CudaWorkingTemp | etc.
        curName = files(i).name;
        if(~isempty(strfind(curName, '.set')))
            disp(strcat('New File : ', curName));
                          
            % Open File.
            EEG = pop_loadset('filename',curName,'filepath',current_folder);
            EEG.setname = curName(1:6);
            [ALLEEG, EEG, CURRENTSET] = pop_newset(ALLEEG, EEG, 0,'gui','off');
            EEG = eeg_checkset(EEG);

            eeglab redraw

            % Go in CudaWorkingTemp
            cd 'CudaWorkingTemp'
            
            % --- ICA ---
            EEG = pop_runica(EEG, 'icatype', 'cudaica', 'extended', 1, 'interupt', 'off', 'verbose', 'matlab');
            [ALLEEG, EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);
            EEG = eeg_checkset(EEG);
            [ALLEEG, EEG, CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'setname',[curName(1:6) '_ICA'],'overwrite','on','gui','off');
            EEG.comments = pop_comments(EEG.comments,'', '5. Run ICA decomposition (CUDA-ICA)', 1);
            [ALLEEG, EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);
            % --- ICA ---
            
            
            % Save Post CudaICA !
            EEG = eeg_checkset(EEG);
            EEG = pop_saveset(EEG, 'filename',EEG.setname,'filepath',destination_folder);
            [ALLEEG, EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);

            STUDY = []; CURRENTSTUDY = 0; ALLEEG = []; EEG = []; CURRENTSET = [];

            eeglab redraw

            % Go back in PWD (when started this script).
            cd ..
            % Delete File !
            delete(curName);
            
            % reset wait.
            waittime = 0;
        end
    end
    
    disp(strcat('Waiting... ', num2str(waittime)));
    pause(10);
    waittime = waittime + 10;
end
