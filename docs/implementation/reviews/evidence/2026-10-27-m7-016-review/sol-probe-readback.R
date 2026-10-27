for (f in sort(list.files('R', pattern='[.]R$', full.names=TRUE))) sys.source(f, .GlobalEnv)
source_root <- '/private/tmp/triomics-m7-016-luna/native-debug'
pointer <- jsonlite::read_json(file.path(source_root,'registries','current-D.json'), simplifyVector=FALSE)
original <- file.path(source_root,'D','generations',pointer$generation_id)
payload <- jsonlite::read_json(file.path(original,'evidence-routing.json'),simplifyVector=FALSE)
minimal <- list(artifact_type='evidence_bundle', study_id=payload$study_id, analysis_id=payload$analysis_id,
                immutable_inputs=payload$immutable_inputs)
minimal$immutable_inputs$b_profile_artifact_id <- NULL
minimal$immutable_inputs$b_profile_sha256 <- NULL
minimal$immutable_inputs$b_profile_envelope_sha256 <- NULL
minimal$immutable_inputs$c_profile_artifact_id <- NULL
minimal$immutable_inputs$c_profile_sha256 <- NULL
minimal$immutable_inputs$c_profile_envelope_sha256 <- NULL
routed <- tryCatch(triomics_annotate_evidence(minimal),error=identity)
cat('profileless_exported_api=',if(inherits(routed,'error')) routed$code else routed$status,'\n',sep='')

probe_root <- tempfile('sol-generation-type-',tmpdir='/private/tmp')
generations <- file.path(probe_root,'D','generations')
dir.create(generations,recursive=TRUE)
staging <- file.path(generations,'staging')
stopifnot(file.copy(original,generations,recursive=TRUE))
staging <- file.path(generations,basename(original))
manifest_path <- file.path(staging,'generation-manifest.json')
manifest <- jsonlite::read_json(manifest_path,simplifyVector=FALSE)
for(i in seq_along(manifest$members)) if(identical(manifest$members[[i]]$path,'evidence-routing.json')) {
  manifest$members[[i]]$artifact_type <- 'wrong_type'
  manifest$members[[i]]$scientific_role <- 'wrong_role'
}
manifest$provenance$b$profile$payload_sha256 <- paste(rep('0',64),collapse='')
unlink(manifest_path)
triomics_d_write_raw_file(manifest_path,triomics_b07_prov_canonical_value(manifest))
generation_id <- triomics_sha256(manifest_path)
stopifnot(file.rename(staging,file.path(generations,generation_id)))
result <- tryCatch(triomics_d_validate_generation(probe_root,generation_id),error=identity)
cat('wrong_member_type_and_conflicting_provenance_readback=',if(inherits(result,'error')) result$code else result$manifest$status,'\n',sep='')
cat('probe_root=',probe_root,'\n',sep='')
