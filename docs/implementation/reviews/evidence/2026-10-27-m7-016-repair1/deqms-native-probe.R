for (f in sort(list.files('R', pattern='[.]R$', full.names=TRUE))) sys.source(f, .GlobalEnv)
source_config <- normalizePath('inst/examples/tickets/D-T01.yaml')
fixture_root <- tempfile('d-t01-deqms-fixture-',tmpdir='/private/tmp')
dir.create(file.path(fixture_root,'tickets'),recursive=TRUE)
data_source <- normalizePath(file.path(dirname(source_config),'..','data'),winslash='/',mustWork=TRUE)
stopifnot(file.copy(data_source,fixture_root,recursive=TRUE),file.copy(source_config,file.path(fixture_root,'tickets')))
fixture_root <- normalizePath(fixture_root,winslash='/',mustWork=TRUE)
config_path <- file.path(fixture_root,'tickets',basename(source_config))
metadata_path <- file.path(fixture_root,'data','c04','metadata.tsv')
metadata <- utils::read.delim(metadata_path,check.names=FALSE,stringsAsFactors=FALSE)
metadata$group <- ifelse(as.integer(sub('^SYN_','',metadata$sample_id)) <= 6L,'control','treatment')
utils::write.table(metadata,metadata_path,sep='\t',row.names=FALSE,quote=FALSE)
config_text <- paste(readLines(config_path,warn=FALSE),collapse='\n')
config_text <- sub('    score: numeric\n  factor_levels: {}', '    score: numeric\n    group: factor\n  factor_levels:\n    group:\n      - control\n      - treatment', config_text, fixed=TRUE)
config_text <- sub('      condition: score\n      condition_type: continuous', '      condition: group\n      condition_type: factor', config_text, fixed=TRUE)
config_text <- sub('      reference_level: null\n      target_level: null', '      reference_level: control\n      target_level: treatment', config_text, fixed=TRUE)
config_text <- sub('weights: {intercept: 0, continuous.score: 1}', 'weights: {condition.control: -1, condition.treatment: 1}', config_text, fixed=TRUE)
config_text <- sub('effect_unit: per_unit_beta', 'effect_unit: group_difference', config_text, fixed=TRUE)
config_text <- sub('method: limma', 'method: deqms', config_text, fixed=TRUE)
config_path <- tempfile('d-t01-deqms-',tmpdir=dirname(config_path),fileext='.yaml')
writeLines(config_text,config_path,useBytes=TRUE)
config <- triomics_read_config(config_path)
ingested <- triomics_ingest_configured_study(config,config_path)
cat('deqms_version=',if(requireNamespace('DEqMS',quietly=TRUE)) as.character(packageVersion('DEqMS')) else 'missing','\n',sep='')
cat('metadata groups=',paste(names(table(ingested$sample_metadata$group)),as.integer(table(ingested$sample_metadata$group)),collapse=';'),'\n',sep='')
count_frames <- lapply(sort(unique(as.character(ingested$feature_registry$block_id)),method='radix'),function(block_id) {
 features <- as.character(ingested$feature_registry$feature_key[as.character(ingested$feature_registry$block_id)==block_id])
 samples <- as.character(ingested$sample_metadata$sample_id)
 data.frame(block_id=block_id,feature_key=rep(features,each=length(samples)),sample_id=rep(samples,times=length(features)),stringsAsFactors=FALSE)
})
counts <- do.call(rbind,count_frames); rownames(counts)<-NULL
counts$psm_count <- seq_len(nrow(counts))+2L
counts$acquisition <- 'synthetic_D_T01_qualified_acquisition'
counts$provenance <- 'synthetic public D-T01 qualified count fixture'
count_path <- tempfile('d-t01-counts-',tmpdir='/private/tmp',fileext='.tsv')
utils::write.table(counts,count_path,sep='\t',row.names=FALSE,quote=FALSE)
config_text <- paste(readLines(config_path,warn=FALSE),collapse='\n')
config_text <- sub('count_metadata: null',paste0('count_metadata: ',as.character(jsonlite::toJSON(count_path,auto_unbox=TRUE))),config_text,fixed=TRUE)
writeLines(config_text,config_path,useBytes=TRUE)
config <- triomics_read_config(config_path)
ingested <- triomics_ingest_configured_study(config,config_path)
cat('hash=',attr(config,'config_sha256'),'\n',sep='')
root <- tempfile('d-t01-deqms-output-',tmpdir='/private/tmp');dir.create(root);root<-normalizePath(root,winslash='/',mustWork=TRUE)
cli <- function(stage) {
 exe<-normalizePath('inst/exec/triomics');old<-setwd(dirname(exe));on.exit(setwd(old),add=TRUE)
 out<-system2(file.path(R.home('bin'),'Rscript'),c('--vanilla',shQuote(basename(exe)),'--config',shQuote(config_path),'--stage',shQuote(stage),'--output',shQuote(root)),stdout=TRUE,stderr=TRUE)
 status<-attr(out,'status');if(is.null(status))status<-0L;cat(stage,' status=',status,'\n',paste(out,collapse='\n'),'\n');as.integer(status)
}
b<-cli('B')
if(b==0L){
 br<-jsonlite::read_json(file.path(root,'B','block-results.json'),simplifyVector=FALSE)
 cat('B method=',br$method,' requested=',br$requested_method,' status=',br$status,' reason=',br$unavailable_reason %||% '', '\n',sep='')
 if(length(br$blocks)) { block<-br$blocks[[1L]];claim<-block$contrasts[[1L]];cat('countprior=',jsonlite::toJSON(claim$rows[[1L]]$count_prior_ref,auto_unbox=TRUE),'\n',sep='') }
 cstatus<-cli('C')
 if(cstatus==0L) { dstatus<-cli('D');cat('d_status=',dstatus,'\n',sep='') }
}
cat('root=',root,'\n',sep='')
