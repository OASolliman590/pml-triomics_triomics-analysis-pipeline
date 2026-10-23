# M7-004 integration verification and pause disposition

- Date: 2026-10-23
- Accepted specification base: `221df65a20e6a53bf523a4f5592a9ffe1e006ec8`
- Final isolated candidate: `647192f`
- Integrated commits: `8b8c3cd`, `fe34495`, `1cb9daf`
- Sol-owned dependency registration: `5dcc25f` (`V8` in `DESCRIPTION` Imports)
- Disposition: **implementation and source-suite green; acceptance review pending**

## Integration integrity

After serial integration, the four packet-owned files matched the exact blobs in
candidate `647192f`:

- `R/provenance_contract.R`;
- `inst/schema/stage-handoff-provenance.schema.json`;
- `tests/testthat/test-M7-003-provenance-contract.R`; and
- `docs/implementation/registrations/M7-003-contract-foundation.json`.

Sol independently reran the focused M7-003 provenance-contract suite on the
final candidate. It exited 0 and completed `DONE` with no failure, warning, or
skip. The final repair covers the two public-validator paths reproduced by the
focused rereview: a B method state detached from its producer-declared method,
and normalized metadata detached from the profile analysis scope.

## Combined source-suite gate

Command:

```text
env R_LIBS_USER=/tmp/triomics-mofa-lib:/Users/megastore/Library/R/x86_64/4.6/library \
  TRIOMICS_MOFA_PYTHON=/tmp/triomics-mofa-py/bin/python \
  Rscript --vanilla -e 'testthat::test_local(reporter = "summary")'
```

Result: shell exit 0; `DONE`; failures 0, errors 0, skips 0, warnings 14. The
summary reporter did not emit a numeric PASS total. All fourteen warnings are
the existing C-05 native MOFA small-fixture diagnostics:

- four low-feature-count warnings;
- four factor-1/average-expression correlation warnings;
- four `no non-missing arguments to max; returning -Inf` warnings; and
- two inactive-factor warnings in the missing sample-view capability fixture.

No warning was suppressed or reclassified. No private study data were read.

## Why acceptance remains pending

The initial two-axis review passed Standards but required Spec repairs. The one
focused Spec rereview resolved route matching and the first coverage probes, but
returned one remaining Important scope finding. Repair 2/2 now passes the exact
reproduced paths and the full suite, but the cycle's single focused rereview has
already been consumed. Sol therefore does not silently invent another review,
mark the registration accepted, release M7, or start producer/D seams.

The next authorized transition is a governance decision: either authorize one
fresh read-only acceptance audit of immutable integrated snapshot `5dcc25f`, or
record M7-004 blocked despite its green tests. Until then, B-T07/C-06 producer
provenance, D verification/publication, migration, D-T01 acceptance, M7 release,
and every Stage-D successor remain closed.
