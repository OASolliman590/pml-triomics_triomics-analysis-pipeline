# RDY-002 initial RED checkpoint

Sol inspected the initial behavioral tests and independently ran only filter
`RDY-002-c07-qualification` on base `b99fa2174fad126bffb94ee14439d83a26e5e1ff` with the new test
file uncommitted. Test SHA-256: `664b7fbd8da27eb316906e0370fd63888a4143f3e497f2c4a6738bf2071c107a`.

Result: **4 tests, 4 expected errors, 0 failed assertions, 0 passed assertions,
0 warnings, 0 skips; exit 1**. Every error names the absent runner behavior. This
is RED evidence, not a successful implementation or scientific result. No native
fit ran. The local `rdy002-sol-red` receipt retains the exact command, log and
per-test counts.

Tests cover deterministic generation and matched maps, the 282-cell plan and
2,550-fit reservation, durable pre-launch accounting with injected crash and no
retry, and truthful failure/coverage/uncertainty summaries. Sol independently
recomputed the five fixed generator reference values and the seed-bootstrap
interval, agreeing with the tests. The inspected diff contains no skip.

The same Luna initial attempt may now implement the two files authorized by the
[focused scope pass](2026-11-08-rdy-002-packaging-scope-repair.md). Repairs remain
0/2; this checkpoint is not a repair. Worker runs no-fit tests only; Sol reviews
the resulting immutable candidate before any qualification call. Scientific
criteria, fixed seeds and budget remain unchanged. Stage acceptance and all
conditional qualifications are preserved.
