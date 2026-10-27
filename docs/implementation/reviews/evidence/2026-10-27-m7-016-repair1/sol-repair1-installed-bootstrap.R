# Exercise the repair's recovery-subprocess bootstrap using the installed
# resource branch in helper-installed-paths.R, without running any suite.
cli <- system.file('exec','triomics',package='triomics')
stopifnot(nzchar(cli))
repository_root <- normalizePath(file.path(dirname(cli),'..','..'),winslash='/',mustWork=TRUE)
setwd(repository_root)
sources <- sort(list.files(file.path(repository_root,'R'),pattern='[.]R$',full.names=TRUE))
for (f in sources) sys.source(f,.GlobalEnv)
cat('repository_root=',repository_root,'\n',sep='')
cat('R_source_count=',length(sources),'\n',sep='')
cat('triomics_namespace_loaded=', 'triomics' %in% loadedNamespaces(),'\n',sep='')
outcome <- tryCatch(triomics_read_config('unused-config-path'),error=identity)
cat('next_call=',conditionMessage(outcome),'\n',sep='')
