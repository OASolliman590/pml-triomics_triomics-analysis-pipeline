# M3 / G3 Astra review

Date: 2026-10-14
Reviewer role: fresh Astra low, read-only
Immutable snapshot: `d96ca310cea5715f40e2cae72d59a29d411713bb`
Frozen spec hash: `f0d482db62f16c8e4c802c99c1fd88e501f29959bc475d67f934b40c6f6ae5d2`

## Initial review

Verdict: `CHANGES_REQUIRED`.

1. **G3-01 / A-T08-AC3.** Resume validated an exported state pointer but did
   not validate the referenced fit-state payload and hash. Deleting that payload
   still caused all artifacts to report reuse.
2. **G3-02 / A-T06-AC3.** Weighted carbon/double-bond endpoints and ratios
   inherited the parent assay quantity and units. A reproduced weighted-carbon
   value of 17.2 was incorrectly labelled as intensity in synthetic arbitrary
   units.
3. **G3-03 / A-T05-AC3.** A caller-supplied qualification marker could enable
   `qualified_inductive` even when its declared kNN/RF native fit/apply symbols
   did not exist. Qualification must be bound to real implemented APIs and
   versioned evidence, otherwise the optional lane must remain unavailable.

Required regression evidence: referenced-state deletion/corruption tests,
endpoint-specific quantity/unit assertions, and rejection of qualification
records that name nonexistent APIs.

## Evidence and scope

Astra independently reran the five packet suites successfully and found no
Stage B source or CLI enablement in the snapshot. PQN ordering, training-only
preparation, missingness safeguards, and arbitrary-profile dispatch retained
substantive coverage. The reviewer inspected, but did not rerun, the five CLI
and package-check receipts.

The snapshot remained clean and the frozen specification, seeds, thresholds,
and packet identities were unchanged. These findings concern synthetic software
behavior; they do not constitute real-data scientific validation.

## Sol disposition and bounded repair outcome

G3 is not accepted. A-T08 and A-T06 repairs passed fresh Sol 6.1 high review and
were integrated as `610a732` and `62b2e7d`. Their combined integrated suite
passed 455 assertions with no failures, warnings, or skips.

A-T05 exhausted both permitted repairs. Final review independently demonstrated
that a caller-authored artifact carrying current API fingerprints can still
self-qualify kNN/RF without an authoritative approval source. Both A-T05
candidates remain unintegrated. G3 is therefore blocked and did not enter the one
permitted focused Astra re-review. A-T04 and A-T07 remain Sol-verified but cannot
be accepted while the milestone is blocked.

## Focused re-review after G3-001

Date: 2026-10-14
Reviewer role: fresh Astra low, read-only
Immutable snapshot: `af323f80dc91fd0f70e1ca9fc7664ab3956aaea5`
Verdict: `PASS`

- **G3-01 — addressed.** Referenced state existence, structure, hashes and
  identity are checked before reuse. Missing, corrupt, malformed and incomplete
  state probes refresh the bounded state/handoff/index while unaffected state is
  reused.
- **G3-02 — addressed.** Weighted carbon and double-bond endpoints carry index
  quantities and property-count units; saturation ratio is dimensionless.
- **G3-03 — addressed under change record G3-001.** Metabolomic kNN/RF always
  return `unavailable / qualification_unavailable_in_v2`; caller-supplied
  qualification cannot activate them. `none` and fold-local training median
  remain executable.

Astra independently reran 580 assertions with no failures, warnings or skips,
plus both malformed-state probes. It found exactly seven allowlisted A-owned
files in the focused diff, no Stage B changes, and matching frozen-file
checksums. The full-suite, CLI and package-check results remained supplied Sol
evidence rather than independently rerun evidence.

Nonblocking scientific limits remain explicit: Omar's studies were not used,
real-data scientific validation is absent, and advanced imputation qualification
is intentionally unavailable. Sol therefore accepts G3 as a software milestone,
not as evidence of biological validity.
