source('/tmp/triomics-m7-017/sol-repair1-time-probe.R')
ing$sample_metadata$time[ing$sample_metadata$sample_id==excluded_sample] <- 0
map <- triomics_b_provenance_unit_map(ing,a,res)
comp <- triomics_c06_cli_compose_unit_map(res,ing,a,ids,map)
ctx <- list(study_id=ing$study_id,analysis_id=a$analysis_id,preparation_context_id=a$preparation_context_id,representation_id='subject_centered')
for(mode in c('NA','NULL')) {
 unit_map <- comp$unit_map
 unit_map[[excluded_sample]]$time <- if(mode=='NA') NA_real_ else NULL
 got <- tryCatch(triomics_c08_validate_unit_map(unit_map,comp$training_unit_ids,ctx),error=identity)
 cat('EXCLUDED_ENDPOINT_TIME',mode,if(inherits(got,'error')) paste(got$code,conditionMessage(got)) else 'ACCEPTED','\n')
}
