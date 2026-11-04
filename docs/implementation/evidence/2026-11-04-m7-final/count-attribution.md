# Source versus installed expectation counts

Product P: `126a30fc68fd593826a3ca82cd399fff35ee90ef`.
Source reports 7353 pass; installed reports 7326 pass. Difference: 27.
Both report zero failures and zero skips. No test was rerun for this note.

The source per-test counts below were read from the preserved testthat-results.rds
(with the testthat namespace loaded for its data-frame conversion). Installed
per-test counts were not emitted by CheckReporter: that column is a static count
of the existing installed branches, not a claimed measured per-test transcript.

| Existing test | Observed source pass | Installed branch expectations (static) | Difference |
|---|---:|---:|---:|
| C-03 fixture is schema-valid and shared registration remains coordinator-owned | 11 | 6 | 5 |
| C-04 registration request records the applied shared contract change | 7 | 2 | 5 |
| C-05 environment check reports both runtimes as not loaded from an empty library and exits non-zero | 5 | 3 | 2 |
| C-05 registration records the accepted synthetic-software-only route | 16 | 1 | 15 |
| Total | 39 | 12 | 27 |

At P, `tests/testthat/test-C-03.R:306` checks source schema identity and four
source registration expectations; installed layout checks its packaged schema.
`tests/testthat/test-C-04.R:563` adds five source registration expectations while
both layouts check the registered adapter. `tests/testthat/test-C-05.R:198`
uses the installed runtime alternative because tools/ is not shipped: c05_error
adds two expectations and the untrained guard adds one, versus five source
script checks. `tests/testthat/test-C-05.R:289` replaces sixteen source
registration checks with one installed adapter check because docs/ is not
shipped. Native MOFA execution and runtime availability are separately tested in
both layouts. These unchanged branches account exactly for the aggregate delta;
the actual installed aggregate remains the authoritative measured value.
