current_folder = pwd;
files = dir(fullfile(current_folder,'/*.bdf'));
files = {files.name};
files = sort(files);

destination_folder = ('~/Documents/Playground/JC_Study/Step1');

eeglab;

subject = 1;
%for subject = 1:length(files)
    
    filename = cell2mat(files(subject));
    open = fullfile(current_folder, filename);
    EEG = pop_biosig(open,'channels',[1:69] ,'ref',[68 69],'refoptions',{'keepref','off'});
    EEG.setname = filename(1:6);
    [ALLEEG, EEG, CURRENTSET] = pop_newset(ALLEEG, EEG, 0,'gui','off');
    EEG = eeg_checkset(EEG);

    eeglab redraw

    EEG = pop_editset(EEG, 'subject', filename(3:6), 'session', filename(1), 'group', filename(3));
    [ALLEEG, EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);
    EEG = eeg_checkset(EEG);
    EEG.comments = pop_comments(EEG.comments,'',char('','===============================','Filename code:','Session_Group-Subject_Condition series_Initials_Sex',...
        '','Group codes:','3 = mTBI','4 = Control','','Condition codes:','F = flicker','M = motion','','Sex codes:','F = female','H = male',...
        '','==============================','Triger codes:','1 = first-order flicker (FOF)','2 = second-order flicker (SOF)','3 = first-order motion (FOM)','4 = second-order (SOM)',...
        '', '==============================', 'Tasks', '   Flicker:','   stimulus detection; give response as soon as stimulus is detected',...
        '   Motion:', '   motion direction discrimination; indicate stimulus drifting direction (right/left) as soon as possible'), 1);
    [ALLEEG, EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);
    EEG = eeg_checkset(EEG);

    %eeglab redraw
    
    EEG=pop_chanedit(EEG,'load',{'~/Documents/Playground/JC_Study/Std_locs.ced' 'filetype' 'autodetect'},'setref',{'1:67' 'M1 M2'},'changefield',{68 'datachan' 0},'changefield',{69 'datachan' 0});
    [ALLEEG, EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);
    EEG = eeg_checkset(EEG);   
         
    %eeglab redraw

    EEG = eeg_checkset(EEG);
    EEG = pop_reref(EEG, [],'refloc',struct('labels',{'M2','M1'},'theta',{100.419,-100.419},'radius',{0.74733,0.74733},'X',{-10.9602,-10.9602},'Y',{-59.6062,59.6062},'Z',{-59.5984,-59.5984},'sph_theta',{-100.419,100.419},'sph_phi',{-44.52,-44.52},'sph_radius',{85,85},'type',{'REF','REF'},'ref',{'',''},'urchan',{68,69},'datachan',{0,0}));
    [ALLEEG, EEG, CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'setname', [filename(1:6) '_average-ref'],'overwrite','on','gui','off');
    EEG = eeg_checkset(EEG);
    [ALLEEG, EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);
    EEG = eeg_checkset(EEG);
    
    EEG.comments = pop_comments(EEG.comments,'',char('','===============================','Processing steps','1. Re-reference (average reference)'), 1);
    [ALLEEG, EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);
    ALLEEG = pop_delset(ALLEEG, 2);

    %eeglab redraw

    EEG = eeg_checkset(EEG);    
    EEG = pop_eegfiltnew(EEG, [], 1, 1690, true, [], 0);
    [ALLEEG, EEG, CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'setname', [filename(1:6) '_HPF'],'overwrite','on','gui','off'); 
    EEG = eeg_checkset(EEG);
    EEG.comments = pop_comments(EEG.comments,'','2. Basic FIR filter - High pass filter (1 Hz)', 1);
    [ALLEEG, EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);
    
        EEG = eeg_checkset(EEG);
    EEG = pop_runica(EEG, 'icatype', 'cudaica', 'extended', 1, 'interupt', 'off', 'verbose', 'matlab');
    [ALLEEG, EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);
    EEG = eeg_checkset(EEG);
    [ALLEEG, EEG, CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'setname',[filename(1:6) '_ICA'],'overwrite','on','gui','off');
    EEG.comments = pop_comments(EEG.comments,'', '5. Run ICA decomposition (CUDA-ICA)', 1);
    [ALLEEG, EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);
    
    eeglab redraw
    