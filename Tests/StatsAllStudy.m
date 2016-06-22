
conditions = ['1'; '2'; '3'; '4'];
groups = ['3'; '4'];
values_RT = [];
means = [];
meds = [];
quants = [];

for cond=1:size(conditions, 1)
    for group=1:size(groups, 1)
        values_RT = [];
        for i=1:size(EEG,2)
            for j=1:size(EEG(1, i).epoch, 2)
                if strcmp(EEG(1, i).epoch(1, j).eventtype{1, 1}, conditions(cond)) && strcmp(EEG(1, i).group, groups(group))
                    values_RT = [values_RT EEG(1, i).epoch(1, j).eventlatency{1, 2}];                
                end
            end
        end
        means = [means mean(values_RT)];
        meds = [meds median(values_RT)];
        quants = [quants; quantile(values_RT, [0.25 0.5 0.75])];
    end
end
