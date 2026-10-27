pkgload::load_all('.',quiet=TRUE)
ns<-asNamespace('triomics')
trace('triomics_abort',where=ns,print=FALSE,tracer=quote({
 if(code %in% c('A_ARTIFACT_FINALIZE','C_ELIGIBILITY_UNKNOWN_UNIT')) {
  cat('\nBOUNDARY',code,'KEY',key,'MESSAGE',message,'\n');print(sys.calls())
 }
}))
for(kind in c('paired','trajectory')) {
 config<-if(kind=='paired') '/private/tmp/d-t01-paired-cli-probe-803926a04d4c/tickets/D-T01.yaml' else '/private/tmp/d-t01-trajectory-cli-probe-7ff610834a8e/tickets/D-T01.yaml'
 root<-tempfile(paste0('sol-',kind,'-boundary-'),tmpdir='/private/tmp');dir.create(root)
 cat('\nKIND',kind,'ROOT',root,'\n')
 b<-get('triomics_main',ns)(c('--config',config,'--stage','B','--output',root));cat('B_EXIT',b,'\n')
 if(b==0L) {c<-get('triomics_main',ns)(c('--config',config,'--stage','C','--output',root));cat('C_EXIT',c,'\n')}
}
