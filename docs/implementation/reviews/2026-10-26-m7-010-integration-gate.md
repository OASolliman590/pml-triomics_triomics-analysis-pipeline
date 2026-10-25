# M7-010 review and integration receipt

## Scope and immutable candidates

Authorization: resume the running B-T04 test, finish the final Astra rereview,
integrate if it passes, run the full source gate, and update the handover.
Stage D stays closed. No private study data or GitHub push is authorized.

Base: `b3671143683ae22a247e43229fd136c3e6f91adb`.

| Candidate | Change |
| --- | --- |
| `52a5dfe7f75ca780e451f9a510240396d24ba6fc` | Native B-T04 synthetic protocol and producer provenance |
| `970694533453834c709f496240e0160cca32ecc0` | Complete descriptor sealing and persisted protocol/calibration checks on resume |
| `bf41f10d423888fa8191fe1ac84d9b4e8e32e311` | Structured missing-calibration error before path normalization |

The complete candidate changes exactly `R/design_repeated.R`, `R/export_b.R`,
`tests/testthat/test-B-T04.R`,
`tests/testthat/test-M4-005-stage-b-composition.R`, and
`docs/implementation/registrations/B-T07.json`. The final worktree is clean.
The frozen spec and provenance schema are unchanged.

## Review trail

Fresh Spec (Astra-low) and Standards (Sol-high) reviews of `52a5dfe` required
two repairs: context-free resume omitted persisted protocol/calibration checks,
and the lineage seal omitted consumed source descriptor fields. Both reviewers
passed `9706945` with no open findings. Independent named tests passed 91
descriptor/context expectations and 13 restart expectations, with no failures,
warnings or skips.

The fresh whole-candidate Astra-low review of `b367114..9706945` found no
Critical or Important issue and one Minor issue: same-run missing calibration
failed closed through a plain filesystem error instead of `B_EXPORT_INCOMPLETE`.
The single final fix wave produced `bf41f10`. Its exact regression went from
FAIL 1 / PASS 1 to FAIL 0 / PASS 3. The existing complete B-T04 run finished
with FAIL 0 / WARN 0 / SKIP 0 / ERROR 0 / PASS 430. Earlier repair verification
passed composition 94 and unchanged B-T07 315, all without failures, warnings
or skips.

The initial exploratory repair RED had test-helper errors and cascading
shared-output assertions. It is not authoritative product evidence. Corrected
descriptor RED was FAIL 2 / PASS 87; restart RED was FAIL 5 / PASS 4, with
zero warnings/skips/errors. These clean regressions preceded the repair.

Final scoped Astra-low rereview of `9706945..bf41f10` returned ADDRESSED, pass,
ready_to_integrate yes, Critical 0 / Important 0 / Minor 0. It inspected the
guard ordering and meaningful regression; it did not repeat the suites.

## Integration

Integration commits are `a659313449f5b054b773e0714f9d6cbaf0158b19` and
`0afe2c533c49684f914f62d2a93169f122b73053`. The multi-commit cherry-pick stopped
after the first commit with an index/worktree freshness error. Continuation
advanced to the final repair and conflicted because the middle repair had not
landed. The coordinator resolved only the four affected allowlisted files to
the exact final candidate blobs; the second integration commit therefore
contains both reviewed repairs. A comparison of all five files between
`bf41f10` and `0afe2c5` is empty. The integrated diff passes whitespace checks
and changes exactly the allowlist. Unrelated dirty files were preserved.

## Coordinator source gates

Integrated focused gate at `0afe2c5`: exit 0, FAIL 0 / WARN 0 / SKIP 0 /
ERROR 0 / PASS 839 across 39 test blocks. Per-file totals are B-T04 430,
B-T07 315 and composition 94.

The user then requested a stop at the next checkpoint. The coordinator sent
SIGINT to the identified full-gate R process (PID 54999). Session 91535 ended
with exit 1 and `Execution halted`, during B-T01 test `the stage B fixture
envelope matches limma on predecessor matrices`. This is an interrupted,
incomplete gate, not a passing gate or a confirmed product failure.

The partial log contains three numbered A-T01 diagnostics. It does not contain
their final names/types or the final suite totals. Those diagnostics remain
unresolved; no full-suite warning count or pass claim is made. The next
authorized run must inspect them and complete a fresh full source gate without
repeating Luna repairs or the completed reviews. All run-owned agents and the
gate process are stopped. No new work starts at this checkpoint.

Durable evidence: `evidence/2026-10-26-m7-010/` contains the exact runner,
focused log and JSON totals, and `full-interrupted.log`.

Both commands use `TRIOMICS_MOFA_PYTHON=/tmp/triomics-mofa-py-m7-006/bin/python`
and prepend `/tmp/triomics-mofa-lib-m7-006` to `.libPaths()` without replacing
the ordinary package library. MOFA2 1.22.1 and mofapy2 0.7.5 were verified.
The runner uses `testthat::test_local(reporter = "summary", stop_on_failure = FALSE)`;
the focused selection is `^(B-T04|B-T07|M4-005-stage-b-composition)$` and the
full gate has no filter. It captures totals and named conditions from returned
test results in the same run, without suppressing warnings or rerunning for counts.
Evidence directory: `/private/tmp/triomics-m7-010-gates.0OPH7k`.

## Scientific limits and disposition

The synthetic protocol is the response source. The configured paired TSVs are
not response ancestors. The exported uncentered matrix is distinguished from
the centered fitted response, with ordered design, generated registries, method
and calibration identities retained. Numerical results, seeds `1001:1400`, the
`0.08` bound, and the historical ordinary 23/400 diagnostic are unchanged.

B-T07 remains `candidate/review_pending`, `accepted=false`. B-T07/M7 acceptance,
G7 and Stage D stay closed. This cycle establishes public synthetic software
evidence only; it does not qualify real-study or biological validity.
