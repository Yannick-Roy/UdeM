% #1- Get pipole Id.
dpIds = dipoleId

% #2- Get dataset Id.
%dsIds = STUDY.measureProjection.ersp.projection.domain(1,1).dipoleAndMeasure.datasetId(dpIds)
obj.dipoleAndMeasure.groupNumber(dipoleId)

% #3.1- Get group.
groups = obj.dipoleAndMeasure.groupNumber(dipoleId);
for i=1:length(groups)
    if(groups(i) == 1)
        colors(i,:) = [1 0 0];
    else
        colors(i,:) = [0 0 1];
    end
end

% #3.2- Get condition.
conditions = obj.dipoleAndMeasure.groupNumber(dipoleId);
for i=1:length(groups)
    if(groups(i) == 1)
        colors(i,:) = [1 0 0];
    else
        colors(i,:) = [0 0 1];
    end
end


% #3.3- Get session.
sessions = obj.dipoleAndMeasure.session(dipoleId);
for i=1:length(sessions)
    if(sessions(i) == 1)
        colors(i,:) = [1 0 0];
    elseif(sessions(i) == 2)
        colors(i,:) = [0 1 0];
    else
        colors(i,:) = [0 0 1];
    end
end
