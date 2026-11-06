a <- commandArgs(TRUE)
pkgload::load_all(a[[1L]], quiet=TRUE)
request <- triomics_target_request(list(namespace="synthetic_protein",id="SYNTHETIC_ASSOCIATION",taxon_id=9606,entity_type="protein"),"assess",list(list(selection_id="synthetic-selection",hypothesis_status="exploratory",selected_with_observations=FALSE)),context=list(context_id="synthetic-context"))
safe <- c("Clinical score constructed from 3 molecular features", "Top 10 features selected in the same study", "Score constructed from 3 features across 32 subjects", "PCA v2.1 with 10 features", "Sample count 32; 10 features selected")
unsafe <- c("Patient: 13 clinical score", "Subject: 7 features selected", "Patient ID A13 clinical score", "Patient number 13 clinical score", "MRN 123456 clinical score", "participant number A13 used for selection", "patient id: A13 used for selection", "Subject - A13 used for selection", "Subject identifier A13 used for selection", "MRN: A13 used for selection", "Sample ID A13 used in construction")
safe <- c(safe, "Subject count 32; 10 features selected")
unsafe <- c(unsafe, "Patient ID (A13) clinical score", "Subject 'S7' used in selection", "MRN (AB-123) used in construction", "participant No.: A13 used in selection")
unsafe <- c(unsafe, "PatientAlpha", "SubjectAlpha", "ParticipantAlpha")
safe <- c(safe, "Sample-wise normalization of 32 measurements", "Sample-level score constructed from 3 features", "Sample-level splitting with 10 features")
unsafe <- c(unsafe, "cohortPatientAlpha clinical score", "ClinicalPatient_ID_A13", "Patient, A13 clinical score", "MRN; 123456 used in construction")
safe <- c(safe, "Sample-Level score constructed from 3 features", "Sample-Wise normalization of 32 measurements", "Sample: Median normalization of 32 measurements", "Sample: median normalization of 32 measurements")
safe <- unique(c(safe, toupper(safe),
 "Sample-Level Median normalization of 32 measurements",
 "Sample-Wise PCA v2.1 with 10 features",
 "Sample: MEAN normalization of 32 measurements",
 "Sample: Quantile normalization of 32 measurements"))
unsafe <- unique(c(unsafe, "Sample: A13 used in construction", "Sample: ALPHA used in construction", "Sample-Alpha used in selection", "Sample_ID_a13 used in construction",
 "Sample-Level score constructed from 3 features; Patient ID A13",
 "Sample-Wise normalization of 32 measurements; Sample ID A13",
 "Sample: Median normalization of 32 measurements; MRN 123456",
 "Patient ID A13; Sample-Level score constructed from 3 features",
 "Sample: Median normalization of 32 measurements; cohortPatientAlpha"))
safe <- unique(c(safe, "Sample: robust normalization of 32 measurements", "Sample: Robust normalization of 32 measurements", "Sample: log normalization of 32 measurements", "Sample: Log normalization of 32 measurements"))
unsafe <- unique(c(unsafe, "SubjectS-7 used in selection", "PatientS-A13 used in construction", "cohortPatientS-A13 clinical score"))
safe <- unique(c(safe,
 "Sample: Total intensity normalization of 32 measurements",
 "Sample: Median scaling of 32 measurements",
 "Sample: Pareto scaling of 32 measurements",
 "Sample: Unit variance scaling of 32 measurements",
 "Sample: Batch correction of 32 measurements",
 "Sample: Probabilistic quotient normalization of 32 measurements",
 "Sample: Log transformation of 32 measurements",
 "Sample: Robust feature filtering of 32 measurements"))
safe <- unique(c(safe, toupper(safe), tolower(safe)))
unsafe <- unique(c(unsafe,
 "cohortSubjectS-7 used in selection", "SampleS-7 used in construction", "ParticipantS-A13 used in selection",
 "SubjectS.7 used in selection", "PatientS:A13 used in construction", "SampleS_A13 used in construction",
 "Sample: Robust normalization of 32 measurements; Sample ID A13",
 "Sample: Median scaling of 32 measurements; cohortPatientS-A13",
 "SubjectS-7; Sample: Total intensity normalization of 32 measurements"))
additional <- c("Sample: Score constructed from 3 features", "Sample: Top 10 features selected in the same study")
safe <- unique(c(safe, additional, toupper(additional), tolower(additional)))
unsafe <- unique(c(unsafe, "Sample: ALPHA used in construction of 32 measurements"))
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
