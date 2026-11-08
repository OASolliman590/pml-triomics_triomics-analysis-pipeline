# RDY-002 documentation repair 1: package-visible runner

The initial protocol audit passed at `7afe808`. During the initial Luna RED-test
checkpoint, the worker identified that tracked `.Rbuildignore` excludes all of
`tools/`. Sol reproduced this from `f198cf9`: `tools/check-package.R` stages only
unexcluded package entries and does not supply an external source path to tests.
The existing `triomics_test_resource()` helper supports installed `inst/` resources.

Sol chooses `inst/qualification/run-c07-qualification.R` instead of
`tools/run-c07-qualification.R`. The only other worker-owned file remains
`tests/testthat/test-RDY-002-c07-qualification.R`. This keeps the same runner bytes
available for source and package tests, without packaging/shared-helper edits,
test skipping, a duplicated implementation, exports or registration.

This is documentation repair 1, followed by the single allowed focused protocol
scope rereview. It does not consume a Luna repair; Luna is still in the initial
RED-test checkpoint and no native fit has run. Generators, seeds, comparisons,
numerical criteria, 2,550-fit cap and all scientific boundaries are unchanged.
No runner implementation proceeds until that rereview passes. Existing failed
or superseded records remain intact.
