args <- commandArgs(TRUE)
stopifnot(length(args) %in% c(2L,3L))
filter_text <- if(length(args)==3L) args[[3L]] else NULL
source <- normalizePath(args[[1L]], mustWork=TRUE)
out <- normalizePath(args[[2L]], mustWork=FALSE)
dir.create(out, recursive=TRUE, showWarnings=FALSE)
setwd(source)
revision <- trimws(system2('git',c('rev-parse','HEAD'),stdout=TRUE))
stopifnot(length(revision)==1L, grepl('^[a-f0-9]{40}$',revision),
          length(system2('git',c('status','--porcelain'),stdout=TRUE))==0L)
testthat::set_max_fails(Inf)
started <- format(Sys.time(), tz='UTC', usetz=TRUE)
res <- testthat::test_local(filter=filter_text, reporter='summary', stop_on_failure=FALSE)
df <- as.data.frame(res)
count <- function(x) list(pass=sum(x$passed),fail=sum(x$failed),error=sum(x$error),
                         warn=sum(x$warning),skip=sum(x$skipped),blocks=nrow(x))
conditions <- list()
for (i in seq_along(res)) for (v in res[[i]]$results) {
 if (!inherits(v,'expectation_success')) conditions[[length(conditions)+1L]] <- list(
   test=res[[i]]$test,file=res[[i]]$file,type=class(v)[[1L]],message=conditionMessage(v))
}
by_file <- lapply(split(df,df$file),count)
script_path <- gsub('~+~',' ',sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)[[1L]]),fixed=TRUE)
result <- list(commit=revision,command=paste('Rscript --vanilla',shQuote(script_path),paste(shQuote(args),collapse=' ')),filter=filter_text,
              started=started,finished=format(Sys.time(),tz='UTC',usetz=TRUE),
              counts=count(df), exit_code=if(sum(df$failed,df$error,df$skipped)>0)1L else 0L,
              by_file=by_file,conditions=conditions,
              runtime=list(R=R.version.string,MOFA_library=Sys.getenv('TRIOMICS_MOFA_LIBRARY'),
                           python=Sys.getenv('TRIOMICS_MOFA_PYTHON')))
jsonlite::write_json(result,file.path(out,'source-receipt.json'),auto_unbox=TRUE,pretty=TRUE,null='null')
saveRDS(res,file.path(out,'testthat-results.rds'))
stopifnot(identical(trimws(system2('git',c('rev-parse','HEAD'),stdout=TRUE)),revision),
          length(system2('git',c('status','--porcelain'),stdout=TRUE))==0L)
print(result$counts)
quit(status=if(sum(result$counts$fail,result$counts$error,result$counts$skip)>0)1L else 0L)
