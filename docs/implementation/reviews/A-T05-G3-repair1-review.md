# A-T05 G3 repair 1 — Sol review

Date: 2026-10-14
Reviewer role: fresh Sol 6.1 high, read-only
Candidate: `4c51fcca5e0306c83e4aac539414443fd692ebed`
Parent snapshot: `d96ca310cea5715f40e2cae72d59a29d411713bb`

Verdict: `CHANGES_REQUIRED`.

The candidate changed only the three A-T05 allowlisted files. It correctly made
nonexistent APIs and mismatched evidence-version labels unavailable, and all 148
packet expectations plus the Stage A CLI passed. Held-out mutation checks also
preserved the kNN donor and RF forest hashes.

One P1 finding remains: qualification is still marker-only. A caller can copy the
hardcoded labels without resolving a version-bound `MethodQualification`
artifact and thereby enable `qualified_inductive`. A same-name incompatible
in-memory function also remained qualified under the copied markers. The CLI
fixture uses `imputation: none`, so it does not exercise this optional lane.

Repair 2 must add adversarial coverage for copied labels and stale/same-name
incompatible APIs, then bind qualification to a resolved versioned artifact and
the actual callable implementation identity. If valid evidence is absent, the
optional route must remain unavailable.
