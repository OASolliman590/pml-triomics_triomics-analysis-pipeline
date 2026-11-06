a <- commandArgs(TRUE)
pkgload::load_all(a[[1L]], quiet=TRUE)
request <- triomics_target_request(list(namespace="synthetic_protein",id="SYNTHETIC_ASSOCIATION",taxon_id=9606,entity_type="protein"),"assess",list(list(selection_id="synthetic-selection",hypothesis_status="exploratory",selected_with_observations=FALSE)),context=list(context_id="synthetic-context"))
safe <- c("Score constructed from 3 features across 32 subjects", "SCORE CONSTRUCTED FROM 3 FEATURES ACROSS 32 SUBJECTS")
unsafe <- c("cohortPatientS clinical score", "ClinicalSubjectS used in selection", "cohortParticipantS used in construction", "SubjectS-7 used in selection")
long_phrase <- "Sample: Pooled quality control based signal correction of 32 measurements"
safe <- unique(c(safe, long_phrase, tolower(long_phrase), toupper(long_phrase)))
results <- lapply(seq_along(c(safe,unsafe)), function(i) {
 text <- c(safe,unsafe)[[i]]; keep <- i <= length(safe)
 input <- list(evidence_id="synthetic-association",axis="association",endpoint="association_beta",units="unitless",relation="=",source_id="synthetic-source",publication_group="synthetic-publication",measured_by_pipeline=FALSE,value=NULL,study_id="synthetic-study",analysis_id="synthetic-analysis",subject_overlap_status="shared_subjects",construction_lineage=text,source_selection_history=text)
 profile <- triomics_target_profile(request,list(),list(),external_evidence=triomics_target_external_evidence(request,list(input)))
 target <- list(status="recorded",mode="assess",profiles=list(profile),results=list(),selected_target=NULL,tie_status="not_assessed")
 section <- triomics_report_optional_sections(list(d=list(target_investigation=target)),list(target_enabled=TRUE,clinical_enabled=FALSE))$targets
 rows <- section$tables$external_evidence; html <- paste(triomics_report_optional_section_html(section),collapse="\n")
 want <- if(keep) text else ""
 stopifnot(identical(profile$external_associations[[1L]]$construction_lineage,text),identical(profile$external_associations[[1L]]$source_selection_history,text))
 ok <- identical(rows$construction_lineage,want) && identical(rows$source_selection_history,want) && identical(rows$subject_overlap_status,"shared_subjects") && identical(rows$independent_replication,FALSE) && identical(grepl(text,html,fixed=TRUE),keep)
 list(input=text,expected=want,kind=if(keep)"scientific_lineage" else "synthetic_identifier",construction=rows$construction_lineage,selection=rows$source_selection_history,visible_in_html=grepl(text,html,fixed=TRUE),passed=ok)
})
passed <- vapply(results,function(x)x$passed,logical(1));result <- list(candidate=a[[3L]],passed=sum(passed),failed=sum(!passed),case_count=length(results),scope="Actual synthetic producer/profile/report boundary and HTML projection; not native rendering or universal anonymization",cases=results)
jsonlite::write_json(result,a[[2L]],auto_unbox=TRUE,pretty=TRUE,null="null")
print(c(passed=result$passed,failed=result$failed));quit(status=if(all(passed))0L else 1L)
