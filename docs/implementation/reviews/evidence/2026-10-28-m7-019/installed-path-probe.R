.libPaths(c("/private/var/folders/94/fy64sdws3rl4tv96_nf4hl5c0000gp/T/RtmpdkqBjd/triomics-package-check-76652ec7b8e2/triomics.Rcheck",.libPaths()))
library(triomics)
cat("revision:",triomics:::triomics_source_revision(),"\n")
tryCatch({x<-triomics:::triomics_read_config("/private/var/folders/94/fy64sdws3rl4tv96_nf4hl5c0000gp/T/Rtmpu8aUKy/b-t01-packaged-fixture-78de17789ac1.yaml");triomics:::triomics_ingest_configured_study(x,"/private/var/folders/94/fy64sdws3rl4tv96_nf4hl5c0000gp/T/Rtmpu8aUKy/b-t01-packaged-fixture-78de17789ac1.yaml");cat("INGEST_OK\n")},error=function(e)cat("INGEST_ERROR:",conditionMessage(e),"\n"))
