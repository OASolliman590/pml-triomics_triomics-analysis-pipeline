# C-04 installed untrace guard

Date: 2026-10-19. Commit: `fda190f`. This note does not accept C-03, C-04, or C-05.

## Prior installed check

Archive `ed2f579` checked as Status 1 ERROR, FAIL 1, WARN 8, SKIP 1, PASS 3983.
The failure was the `on.exit` second `untrace("triomics_block_spls_variance")`
after the explicit untrace had already removed the trace. On the installed
namespace that second call throws `could not find function`. The skip was the
empty registration test when `discovery/` is absent from the built package.
The eight warning texts were not in the retained check log.

## Change

`tests/testthat/test-C-04.R` untraces only when
`methods::is(fn, "traceable")`. The registration test always checks that
`triomics_integration_backend("mixomics_block_spls")` is a `BackendAdapter`
with evaluation mode `continuous`.

## Evidence

`test_local(filter='C-04')` on the integration tree: 132 passed, 0 failed,
0 warnings, 0 skips, 155.9 seconds. The log printed one trace and one untrace.

Astra low, thread `01a153dd-2836-7a52-a703-b02b953b7e7f`, returned pass for
this guard and for the fail-closed C-05 draft at `697a5a0`. The review did
not rerun tests. It found no Critical or Important contract break. The guard
does not skip the forced variance failure. No reviewed C-05 path reports a
trained model.

## Still open

The package check of `fda190f` had not returned when this note was written.
Acceptance still requires that check to end with the line `Status: OK`.
The eight warnings, if they remain, must be named from the preserved
`testthat.Rout` before another code change.
