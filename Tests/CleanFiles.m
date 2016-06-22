elems = dir;

for i = 1:length(elems)
    if elems(i).isdir && ~strcmp(elems(i).name, '.') && ~strcmp(elems(i).name, '..')
        disp(['opening : ' elems(i).name])
        
        % Open Sub Folder.
        cd(elems(i).name)
        
        sub_elems = dir;
        for j = 1:length(sub_elems)
            if ~isempty(findstr(sub_elems(j).name, 'design'))
                disp(['deleting : ' sub_elems(j).name])
                delete(sub_elems(j).name);
            end
        end    
        
        % Go Back to Main Folder.
        cd ..
    end
end;