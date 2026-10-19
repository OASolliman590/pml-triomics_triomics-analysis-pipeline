# Package check of fda190f

Date: 2026-10-19. Archive commit `fda190f`. This note does not accept C-03 or C-04.

`tools/check-package.R` reported `Status: OK` for `triomics_0.1.0.tar.gz`.
The preserved `testthat.Rout` ends with:

`[ FAIL 0 | WARN 8 | SKIP 0 | PASS 3985 ]`

Elapsed test time was 632 seconds. The previous `ed2f579` check was FAIL 1, WARN 8, SKIP 1, PASS 3983. The second untrace failure is gone and the empty registration test is no longer skipped. The eight testthat warnings remain. R CMD check still printed `Status: OK` because those warnings were caught inside testthat.

The warning messages are not itemized in `testthat.Rout`. The transcript shows limma text `Coefficients not estimable` for `covariate.batch`, `condition.high`, and `between_group.unobserved`, and mixOmics text `Design matrix has changed to include Y`. A source-tree installed run that could still see `discovery/` counted zero testthat warnings, so the eight are tied to the installed layout. They are not named to a test yet.

Acceptance waits until those eight warnings are named. No suppression has been added.
