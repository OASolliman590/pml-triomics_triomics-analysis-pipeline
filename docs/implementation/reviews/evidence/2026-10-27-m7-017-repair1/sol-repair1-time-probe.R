pkgload::load_all('.',quiet=TRUE)
original <- '/private/tmp/m7-017-native-trajectory'
root <- tempfile('sol-repair1-missing-time-',tmpdir='/private/tmp');dir.create(root)
file.copy(file.path(original,c('tickets','data')),root,recursive=TRUE)
config_path <- file.path(root,'tickets','D-T01.yaml')
cfg <- triomics_read_config(config_path)
metadata_path <- file.path(root,'data','c04','metadata.tsv')
md <- read.delim(metadata_path,check.names=FALSE,stringsAsFactors=FALSE)
excluded_sample <- tail(md$sample_id,1)
md$time[md$sample_id==excluded_sample] <- NA_real_
write.table(md,metadata_path,sep='\t',row.names=FALSE,quote=FALSE,na='NA')
for(b in cfg$blocks) {
 p <- normalizePath(file.path(dirname(config_path),b$file),mustWork=TRUE)
 v <- read.delim(p,check.names=FALSE,stringsAsFactors=FALSE)
 v <- v[,names(v)!=excluded_sample,drop=FALSE]
 write.table(v,p,sep='\t',row.names=FALSE,quote=FALSE)
}
ing <- tryCatch(triomics_ingest_configured_study(cfg,config_path),error=identity)
if(inherits(ing,'error')) {cat('INGEST_REFUSED',ing$code,conditionMessage(ing),'\n');quit(status=0)}
cat('INGEST_ACCEPTED','EXCLUDED_SAMPLE',excluded_sample,'TIME',ing$sample_metadata$time[ing$sample_metadata$sample_id==excluded_sample],'\n')
a <- cfg$analyses[[1]];ids<-unlist(a$block_ids,use.names=FALSE)
s <- list(study_id=ing$study_id,preparation_context_id=a$preparation_context_id,sample_metadata=ing$sample_metadata,sample_registry=ing$sample_registry,declared_factor_levels=ing$declared_factor_levels,feature_registry=ing$feature_registry,assays=ing$assays[ids],assay_hashes=NULL,analysis_unit_map=NULL)
res <- tryCatch(triomics_resolve_analysis(s,a),error=identity)
if(inherits(res,'error')){cat('B_RESOLVER_REFUSED',res$code,conditionMessage(res),'\n');quit(status=0)}
cat('B_RESOLVER_ACCEPTED','\n')
map <- triomics_b_provenance_unit_map(ing,a,res)
row <- map[map$unit_id==excluded_sample,,drop=FALSE]
cat('B_MAP_DISPOSITION',row$eligible,row$exclusion_reason,'N_SOURCE',length(row$source_sample_ids[[1]]),'N_BLOCKS',length(row$represented_blocks[[1]]),'\n')
composed <- tryCatch(triomics_c06_cli_compose_unit_map(res,ing,a,ids,map),error=identity)
if(inherits(composed,'error'))cat('C_COMPOSITION_REFUSED',composed$code,conditionMessage(composed),'\n') else cat('C_COMPOSITION_ACCEPTED','\n')
cat('RETAINED_SYNTHETIC_FIXTURE',root,'\n')
