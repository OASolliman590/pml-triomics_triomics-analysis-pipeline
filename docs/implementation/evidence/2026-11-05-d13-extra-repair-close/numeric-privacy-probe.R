args <- commandArgs(TRUE)
pkgload::load_all(args[[1L]], quiet=TRUE)
request <- triomics_target_request(list(namespace="synthetic_protein",id="SYNTHETIC_ASSOCIATION",taxon_id=9606,entity_type="protein"),"assess",list(list(selection_id="synthetic-selection",hypothesis_status="exploratory",selected_with_observations=FALSE)),context=list(context_id="synthetic-context"))
input <- list(evidence_id="synthetic-association",axis="association",endpoint="association_beta",units="unitless",relation="=",source_id="synthetic-source",publication_group="synthetic-publication",measured_by_pipeline=FALSE,value=NULL,study_id="synthetic-study",analysis_id="synthetic-analysis",subject_overlap_status="shared_subjects",construction_lineage="Patient: 13 clinical score",source_selection_history="Subject: 7 features selected")
profile <- triomics_target_profile(request,list(),list(),external_evidence=triomics_target_external_evidence(request,list(input)))
target <- list(status="recorded",mode="assess",profiles=list(profile),results=list(),selected_target=NULL,tie_status="not_assessed")
section <- triomics_report_optional_sections(list(d=list(target_investigation=target)),list(target_enabled=TRUE,clinical_enabled=FALSE))$targets
source <- profile$external_associations[[1L]]
reported <- section$tables$external_evidence
fields <- c("construction_lineage","source_selection_history")
result <- list(snapshot=args[[3L]],source=setNames(lapply(fields,function(x) source[[x]]),fields),reported=setNames(lapply(fields,function(x) reported[[x]]),fields),subject_overlap_status=reported$subject_overlap_status,independent_replication=reported$independent_replication,privacy_pass=all(vapply(fields,function(x) identical(reported[[x]],""),logical(1))))
stopifnot(identical(reported$subject_overlap_status,"shared_subjects"), identical(reported$independent_replication,FALSE))
jsonlite::write_json(result,args[[2L]],pretty=TRUE,auto_unbox=TRUE,null="null")
quit(status=if(isTRUE(result$privacy_pass))0L else 1L)
