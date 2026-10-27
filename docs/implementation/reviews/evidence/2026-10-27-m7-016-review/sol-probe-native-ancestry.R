for (f in sort(list.files('R', pattern='[.]R$', full.names=TRUE))) sys.source(f, .GlobalEnv)
source_root <- '/private/tmp/triomics-m7-016-luna/native-debug'
probe_root <- tempfile('sol-native-path-', tmpdir='/private/tmp')
dir.create(probe_root)
stopifnot(all(file.copy(list.files(source_root, full.names=TRUE, all.files=TRUE, no..=TRUE), probe_root, recursive=TRUE)))
on.exit <- NULL
config_path <- 'inst/examples/tickets/D-T01.yaml'
config <- triomics_read_config(config_path)
ingested <- triomics_ingest_configured_study(config, config_path)
metadata <- list(sample_metadata=ingested$sample_metadata, sample_registry=ingested$sample_registry,
                 declared_factor_levels=ingested$declared_factor_levels)
read_probe <- function() triomics_d_read_evidence_bundle(
  probe_root, as.character(config$study_id), as.character(config$analyses[[1L]]$analysis_id),
  ingested$source_manifests, ingested$feature_registry,
  current_config_sha256=as.character(attr(config,'config_sha256')), current_metadata=metadata)
baseline <- read_probe()
cat('baseline=', baseline$artifact_type, '\n', sep='')
native_dir <- file.path(probe_root, 'C', 'native-states')
outside <- tempfile('sol-outside-native-', tmpdir='/private/tmp')
stopifnot(file.rename(native_dir, outside), file.symlink(outside, native_dir))
cat('native_parent_is_symlink=', triomics_is_symlink(native_dir), '\n', sep='')
outcome <- tryCatch(read_probe(), error=identity)
cat('read_result=', if (inherits(outcome,'error')) paste(outcome$code, conditionMessage(outcome)) else outcome$artifact_type, '\n', sep='')
cli_outcome <- tryCatch(triomics_execute_stage_d(config_path, probe_root), error=identity)
cat('executor_result=', if (inherits(cli_outcome,'error')) paste(cli_outcome$code, conditionMessage(cli_outcome)) else cli_outcome$status, '\n', sep='')
cat('probe_root=',probe_root,'\n',sep='')
cat('outside=',outside,'\n',sep='')
