args <- commandArgs(trailingOnly=TRUE)
repo <- args[[1L]]
out <- args[[2L]]
dir.create(out, recursive=TRUE, showWarnings=FALSE)
pkgload::load_all(repo, export_all=FALSE, quiet=TRUE)
e <- new.env(parent=asNamespace("triomics"))
for (expr in parse(file.path(repo,"tests/testthat/test-D-T15.R"))) {
  if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      is.symbol(expr[[2L]]) && startsWith(as.character(expr[[2L]]), "dt15_") &&
      is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], as.name("function"))) eval(expr,e)
}
fixture <- e$dt15_native_projection_fixture()
ns <- asNamespace("triomics")
member_fn <- get("triomics_report_clinical_b_bundle_member",ns)
section_fn <- get("triomics_report_clinical_section",ns)
sha_fn <- get("triomics_sha256",ns)
control <- member_fn(fixture$loaded,fixture$config)
stopifnot(identical(control$integrity_status,"verified"))
clinical <- fixture$loaded$payloads$clinical
clinical_ref <- fixture$loaded$artifacts$clinical
fixture$loaded$payloads$clinical <- NULL
fixture$loaded$artifacts$clinical <- NULL
fixture <- e$dt15_bind_b_bundle(fixture)
absent <- member_fn(fixture$loaded,fixture$config)
stopifnot(identical(absent$integrity_status,"absent"))
bundle_path <- file.path(fixture$root,dirname(fixture$loaded$d$immutable_inputs$b_artifacts$family_ledger$path),"bundle.json")
pinned_sha <- sha_fn(bundle_path)
fixture$loaded$payloads$clinical <- clinical
fixture$loaded$artifacts$clinical <- clinical_ref
warnings <- character()
result <- withCallingHandlers(tryCatch(section_fn(fixture$loaded,fixture$config),error=identity), warning=function(w) warnings <<- c(warnings,conditionMessage(w)))
is_error <- inherits(result,"error")
receipt <- list(candidate=system2("git",c("-C",shQuote(repo),"rev-parse","HEAD"),stdout=TRUE),
  evidence_kind="typed synthetic report-boundary probe; not native producer reachability or acceptance",
  control_bound_member=control$integrity_status, absent_member=absent,
  loaded_clinical_present=TRUE, pinned_bundle_sha256=pinned_sha,
  pinned_bundle_bytes_unchanged=identical(pinned_sha,sha_fn(bundle_path)),
  error_returned=is_error, warnings=warnings)
if(is_error) receipt$error <- conditionMessage(result) else {
  receipt$section_status <- result$status
  receipt$section_reason <- result$reason_code
  receipt$associations <- result$tables$associations
  receipt$coverage_rows <- nrow(result$tables$variable_coverage)
  receipt$objective_rows <- nrow(result$tables$objectives)
}
receipt$defect_reproduced <- !is_error && nrow(result$tables$associations)>0L && identical(pinned_sha,sha_fn(bundle_path))
jsonlite::write_json(receipt,file.path(out,"receipt.json"),auto_unbox=TRUE,pretty=TRUE,null="null",digits=NA)
stopifnot(isTRUE(receipt$defect_reproduced),identical(as.numeric(result$tables$associations$effect),0.4))
cat("DEFECT REPRODUCED: a D-pinned absent clinical member permits a loaded effect of 0.4; pinned B bundle unchanged.\n")
cat("This is a negative boundary result, not a passing acceptance gate.\n")
