for (f in sort(list.files('R',pattern='[.]R$',full.names=TRUE))) sys.source(f,.GlobalEnv)
for (e in as.list(parse('tests/testthat/test-C-08.R'))) {
 if(is.call(e)&&identical(e[[1]],as.name('<-'))&&is.call(e[[3]])&&identical(e[[3]][[1]],as.name('function'))) eval(e,.GlobalEnv)
}
x<-c08_set_representation(c08_inputs('mofa2'),'delta')
b<-triomics_integration_score_endpoints(x$result,x$fit,x$context)
for(i in seq_along(b$endpoints)) names(b$endpoints[[i]]$samples[[1]]$visit_weights)<-c('wrong_reference','wrong_target')
r<-tryCatch(triomics_c08_cli_endpoint_payload(b),error=identity)
cat('tampered_names_rejected:',inherits(r,'error'),'\n')
if(!inherits(r,'error')) print(r$endpoints[[1]]$samples[[1]]$visit_weights)
if(inherits(r,'error')) cat('error:',conditionMessage(r),'\n')
