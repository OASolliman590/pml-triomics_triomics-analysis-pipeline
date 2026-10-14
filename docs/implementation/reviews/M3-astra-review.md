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

## Sol disposition

G3 is not accepted. Sol authorized one bounded repair attempt for each affected
packet, followed by fresh Sol 6.1 high packet review and the one permitted focused
Astra G3 re-review. A-T04 and A-T07 remain Sol-verified but cannot be accepted
until the milestone closes.
