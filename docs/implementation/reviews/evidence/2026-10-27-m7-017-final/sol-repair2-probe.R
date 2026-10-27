source('/tmp/triomics-m7-017/sol-repair1-time-probe.R')
stopifnot(!inherits(composed,'error'))
ctx <- list(study_id=ing$study_id,analysis_id=a$analysis_id,preparation_context_id=a$preparation_context_id,representation_id='subject_centered')
mapped <- triomics_c08_validate_unit_map(composed$unit_map,composed$training_unit_ids,ctx)
cat('R1_COMPOSE_AND_ENDPOINT_ACCEPTED','EXCLUDED_IN_TRAINING',excluded_sample %in% composed$training_unit_ids,'\n')
json_path <- tempfile(fileext='.json');triomics_write_json_atomic(mapped,json_path);json <- paste(readLines(json_path),collapse='\n');cat('EXACT_MAP_JSON',json,'\n')
roundtrip <- jsonlite::fromJSON(json,simplifyVector=FALSE)
ids2 <- vapply(roundtrip,`[[`,character(1),'unit_id')
row <- roundtrip[[match(excluded_sample,ids2)]]
cat('ROUNDTRIP_HAS_TIME_FIELD','time' %in% names(row),'ROUNDTRIP_TIME_NULL',is.null(row$time),'\n')
tryCatch({invisible(triomics_c08_validate_unit_map(roundtrip,composed$training_unit_ids,ctx));cat('REPARSED_MAP_ACCEPTED','\n')},error=function(e)cat('REPARSED_MAP_ERROR',conditionMessage(e),'\n'))
null_map<-composed$unit_map;null_map[[excluded_sample]]['time']<-list(NULL);invisible(triomics_c08_validate_unit_map(null_map,composed$training_unit_ids,ctx));cat('NAMED_NULL_ACCEPTED','\n')
for(kind in c('wrong_reason','missing_blocks','represented','missing_time_field','nan','inf','retained_missing','excluded_score')) {
 map2 <- composed$unit_map;scores<-composed$training_unit_ids
 if(kind=='wrong_reason')map2[[excluded_sample]]$exclusion_reason<-'different_exclusion'
 if(kind=='missing_blocks')map2[[excluded_sample]]$represented_blocks<-NULL
 if(kind=='represented')map2[[excluded_sample]]$represented_blocks<-'block_1'
 if(kind=='missing_time_field')map2[[excluded_sample]]$time<-NULL
 if(kind=='nan')map2[[excluded_sample]]$time<-NaN
 if(kind=='inf')map2[[excluded_sample]]$time<-Inf
 if(kind=='retained_missing')map2[[scores[[1]]]]$time<-NA_real_
 if(kind=='excluded_score')scores<-c(scores,excluded_sample)
 got<-tryCatch(triomics_c08_validate_unit_map(map2,scores,ctx),error=identity)
 cat('NEGATIVE',kind,if(inherits(got,'error'))got$code else 'ACCEPTED','\n')
 stopifnot(inherits(got,'error'))
}
exprs<-parse('tests/testthat/test-C-08-shared.R')
expr<-Filter(function(x)is.call(x)&&identical(x[[1]],quote(testthat::test_that))&&is.character(x[[2]])&&x[[2]]=='C-08 permits unavailable time only for an explicit absent-all exclusion',as.list(exprs))
stopifnot(length(expr)==1);eval(expr[[1]],globalenv())
