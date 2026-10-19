# C-02 registration verification — 2026-10-19

Software evidence only. No private study data were read or fitted.

## Closed before this registration

- Findings F01–F08 are closed in `a3a1cfd`, `249e2d0`, and `2f4e8e4`.
- Luna high rechecked that tree and changed no files. Reported result: 17 cases, 131 assertions, 0 failures.
- Astra low returned a code-only pass on `a3a1cfd..2f4e8e4` and did not rerun tests.
- The splits helper and its three tests are `028f7c8`. Sol reran those three tests before that commit.

## Sol checks on the registered tree

- `testthat::test_local(filter='C-02')`: 165 passed, 0 failed, 0 warnings, 0 skips.
- `testthat::test_local(filter='A-T05')`: 127 passed, 0 failed, 0 warnings, 0 skips.
- Stage C command with `inst/examples/tickets/C-02.yaml` exited 0.
- Result: `concat_plsda`, 12 eligible units, 12 predicted, 0 failed predictions, balanced error 0, 8 inspected fits, max fits 20.
- An inner fold with training rank 1 rejected `ncomp=2` as `C_EVAL_COMPONENT_RANK` and did not reduce the component count silently.
- `min_observed` in the synthetic fixture is 1. The previous value, 3, removed every feature from a two-sample inner training fold and surfaced as `A_SOURCE_CONTRACT` dimname drift. Empty prevalence filters now abort as `A_STATE_PARTIAL`.

## Not done

- Clean package check.
- Immutable M5 Astra review.
- C-02 acceptance and G5 release.
- C-03, C-04, C-05, and every D packet.
