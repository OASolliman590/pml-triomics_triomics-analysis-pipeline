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

## Focused rereview disposition

Astra passed immutable `b99fa2174fad126bffb94ee14439d83a26e5e1ff`: **audit_pass yes**,
no P0/P1. All three archive manifest hashes matched. Existing `inst/` resource
lookup supports the corrected two-file scope; no shared seam is needed. The
immutable protocol must be supplied explicitly at execution because `docs/`
is not installed. Scientific design and budget are unchanged. Initial audit 1,
focused rereview 1/1 exhausted. Requested Astra low; effective settings unexposed.
Sol authorizes continuation of the same Luna initial attempt after RED inspection.
