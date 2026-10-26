#!/bin/zsh
set -eu
proof=/tmp/triomics-m7-014-sol/proof-18b6e90
mkdir -p "$proof"
Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/C-02.yaml --stage B --output "$proof/c02-b" > "$proof/c02-b.log" 2>&1
Rscript --vanilla /tmp/triomics-m7-014-sol/readback.R "$proof/c02-b" before "$proof/c02-b-hashes.json" > "$proof/c02-b-readback.log" 2>&1
printf 'PROOF 1: native C02 B and provenance readback passed\n'
Rscript --vanilla /tmp/triomics-m7-014-sol/run-tests.R '^C-08-shared$' > "$proof/c08-tests.log" 2>&1
Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/C-08.yaml --stage C --output "$proof/c08-only" > "$proof/c08-cli.log" 2>&1
Rscript --vanilla /tmp/triomics-m7-014-sol/check-c08.R "$proof/c08-only" > "$proof/c08-readback.log" 2>&1
printf 'PROOF 2: C08 suite and endpoint-only readback passed\n'
Rscript --vanilla /tmp/triomics-m7-014-sol/run-tests.R 'C-06' > "$proof/c06-tests.log" 2>&1
Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/C-06.yaml --stage B --output "$proof/c06-bc" > "$proof/c06-b.log" 2>&1
Rscript --vanilla /tmp/triomics-m7-014-sol/readback.R "$proof/c06-bc" before "$proof/c06-b-before.json" > "$proof/c06-b-readback.log" 2>&1
Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/C-06.yaml --stage C --output "$proof/c06-bc" > "$proof/c06-c.log" 2>&1
Rscript --vanilla /tmp/triomics-m7-014-sol/readback.R "$proof/c06-bc" after "$proof/c06-b-before.json" > "$proof/c06-c-readback.log" 2>&1
printf 'PROOF 3: C06 suite and exact B-to-C readback passed\n'
