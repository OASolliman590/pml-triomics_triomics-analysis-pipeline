# RDY-002 initial runner: changes required

Sol reviewed immutable candidate `ee9d905426e2b9fd13e874bf6fa4d9b775b030ba`
against `16077f5da5e7a757cf061ac94f28228dd23b077f`. The two-file diff adds
only the installed qualification runner and its tests. Archive SHA-256:
`a4904b182f70d391aeb2280c6d30578ec9216b123e46d1516b83d37b297c0de1`.
It is **not integrated, GREEN, scientifically qualified or accepted**.

Sol independently ran the new test file on that archive using Rscript --vanilla,
pkgload::load_all and testthat::test_file, with machine-readable results retained
locally: **13 tests; 178 passed assertions, 4 failed assertions, 0 errors,
0 warnings, 0 skips; exit 1; native fits 0**. The worker's unsandboxed failures
were reproduced: two timeout/identity tests each leave the owned root alive and
report `subscript out of bounds`. No warning was suppressed or reclassified.

A separate root no-fit primitive probe also found that processx kill_tree can
return without proving root/descendant termination. Exact-root cleanup and the
self-limited dummy's eventual absence were verified; that is failed supervisor
qualification evidence, not a successful cleanup guarantee. Diskutil rejected
an arbitrary output directory; querying its actual mounted volume confirmed the
required internal APFS host. No model call was used to test supervision.

## Standards review

Fresh read-only `/root/rdy002_sol_standards`, requested Sol 6.1/high, returned
**changes required**. Effective settings were not exposed. No edits or fits.
Four hard P1 findings, independently checked against the frozen source:

- Cleanup marker inspection at runner lines 548/559 can throw before the cleanup
  handler at 565 exists, leaving the owned process alive. RDY-003 requires
  explicit cleanup and verified termination before another cell.
- Resume at 788–798 skips reserved/interrupted cells without reconciling persisted
  PID, creation time and run ownership. An error releases the writer lock, so
  resume can launch later cells while a previous process survives. RDY-003
  requires refusal when orphan identity, liveness or cleanup is uncertain.
- Plan/preflight at 1595–1598 rewrites inputs outside the writer lock; execution
  at 1416–1421 reads manifest paths without verifying stored hashes. This can
  replace the ancestry of existing evidence. RDY-003 requires a single writer
  and unchanged execution identity; RDY-002 requires retained input hashes.
- Runtime inspection at 295–299 calls nonexistent utils::package_dependencies
  and silently substitutes an empty dependency set. The function belongs to
  tools. Required dependency identity is consequently incomplete.

Blocked-host branches in tests establish refusal handling, not successful host
qualification. No discretionary style issue is an acceptance blocker.

## Scientific-spec review

Fresh read-only `/root/rdy002_sol_spec`, requested Sol 6.1/high, returned
**changes_required**. Effective settings were not exposed. No edits or fits.
Four grouped blocking findings:

- Protocol line 26 requires the identical split object. Runner 1325 saves one
  SplitPlan per lane; 1150 and 1237 index that object by lane, producing NULL.
  Protocol line 11 requires the fixed class labels; ordinary categorical runs
  instead retain numeric y (994, 1135–1136), which the actual evaluator refuses
  with C_EVAL_RESPONSE_INVALID. Pure no-fit checks confirmed both failures.
- Protocol line 50 requires failed/absent originals to leave mutations unrun.
  Runner 1233–1236 accepts a failed original containing an EvaluationResult;
  1261–1267 leaves a failed mutation verdict's cell complete. Mutated inputs
  receive original matrix hashes at 1166–1170, contrary to required input and
  outcome ancestry in protocol line 58.
- Protocol lines 38/40/52 require strong recovery accounting, signed factor/truth
  assignment, g/z and residual associations, initialization sensitivity,
  seedwise counts/median/range and paired uncertainty. Runner 1213–1221 returns
  only first-score z correlation and a subspace comparison. Its alignment,
  pairing and bootstrap helpers are not connected to execution. Final manifest
  publication is also unfinished, as the worker reported.
- Protocol line 28 fixes analysis_id per seed/lane. Runner 1042–1051 omits it,
  causing the evaluator's generic supervised-evaluation fallback and incorrect
  prescribed analysis lineage.

No unauthorized scientific scope was identified. The two axes each have four
grouped blockers: unsafe process continuation and invalid predictive bindings
are respectively the most immediate failures; neither axis passes.

## Parent disposition and bounded next transition

The initial attempt is preserved. Dispatch **the same Luna worker for targeted
repair 1 of 2**, restricted to the same two files. Repair all reviewed defects,
finish the already prescribed scientific output/manifest path, and add meaningful
no-fit tests of actual adapter/evaluator argument construction, mutation
accounting, input tampering, concurrent writer refusal, orphan uncertainty and
standalone plan/worker CLI behavior. No native fit or new scientific choice.

Also verify runtime fingerprints cover executable R/Python package code rather
than only DESCRIPTION, shared libraries or a Python initializer. Fix APFS volume
resolution without weakening the local-host condition. Separate positive
process-cleanup qualification from simulated refusal cases: failed/unknown
inspection never counts as termination, and host-blocked witnesses cannot be
renamed as successful qualification. Preserve failed logs and attempt history.

Record ordinary cell failures and continue only when prior ownership/cleanup is
verified; a global timeout or unresolved cleanup leaves remaining rows unrun
with reasons. A failed/incomplete qualification must remain visibly incomplete
through result status, exit and final evidence. Keep the fixed 282-cell/2550-fit
plan, original deadline, no retries/refunds and all numerical criteria unchanged.

If the actual host cannot prove the required cleanup, stop with that evidence;
do not replace the tests or spend a model call. Sol will review and rerun the
repaired immutable candidate before any qualification execution. M1–M9/G7–G9
records, frozen scientific contracts and conditional dispositions are unchanged.
No private study analysis, external upload, GitHub push or model fit occurred.

### Additional no-fit standalone witness

After the initial review, Sol verified that processx with an explicit named
`env` vector does not inherit a parent sentinel variable. The runner's child
launcher must explicitly preserve its required pinned runtime and temporary-root
settings, without serializing the complete environment or secrets. This is part
of repair1's existing standalone/runtime requirement, not a new attempt or native
call. Both original root-probe dummy processes were verified absent afterward.
