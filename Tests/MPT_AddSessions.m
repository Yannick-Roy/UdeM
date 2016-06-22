nb_domains = length(STUDY.measureProjection.erp.projection.domain);
for i = 1:nb_domains
    if(isempty(STUDY.measureProjection.erp.projection.domain(1, i).dipoleAndMeasure.session))
        nb_dipoles = length(STUDY.measureProjection.erp.projection.domain(1, i).dipoleAndMeasure.datasetId)
        for j = 1:nb_dipoles
            cur_datasetId = STUDY.measureProjection.erp.projection.domain(1, i).dipoleAndMeasure.datasetId(j)
            STUDY.measureProjection.erp.projection.domain(1, i).dipoleAndMeasure.session = [STUDY.measureProjection.erp.projection.domain(1, i).dipoleAndMeasure.session, STUDY.datasetinfo(cur_datasetId).session];
        end
    else
        disp(['Abort! -> session variable in domain #' num2str(i) ' is not empty!']);
    end
end
