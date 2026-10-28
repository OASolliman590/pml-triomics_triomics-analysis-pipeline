set -eu
export R_LIBS_USER=/tmp/triomics-mofa-lib-m7-006:/Users/megastore/Library/R/x86_64/4.6/library
export TRIOMICS_MOFA_PYTHON=/tmp/triomics-mofa-py-m7-006/bin/python
export RETICULATE_PYTHON=/tmp/triomics-mofa-py-m7-006/bin/python
Rscript --vanilla /tmp/triomics-m7-018/archive/tools/check-package.R --root /tmp/triomics-m7-018/archive --output /tmp/triomics-m7-018/package > /tmp/triomics-m7-018/package-wrapper.log 2>&1
