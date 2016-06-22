for i=1:length(ALLEEG)
    curSubject = ALLEEG(1, i).subject;
    curGroup = ALLEEG(1, i).group;
    curSession = ALLEEG(1, i).session;
    curCondition = ALLEEG(1, i).condition;
    curSetname = ALLEEG(1, i).setname;
    
    fileName = [curSetname '_SIFT.mat'];
    fileFolder = ['/media/user/BrainData/Study_mTBI_Sub/' curSubject];
    filePath = [fileFolder '/' fileName];
    %*%d-%02d*_SIFT.mat
    %load('/media/user/BrainData/Study_mTBI/3-01/1_3-01_4-SOM_SIFT.mat')
    
    if(exist(filePath))
        disp(['Loading : ' filePath]);
        load(filePath);
        disp(['Changing CAT value : ' curSetname]);
        ALLEEG(1, i).CAT = CAT;
        disp('Done!');
    else
        disp(['** ERROR : File doesn''t exist. ** ' filePath]);
    end
end