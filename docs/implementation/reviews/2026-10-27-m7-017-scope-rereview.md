# M7-017 focused scope rereview

- Immutable scope: `5eaf18c`; prior scope: `8c68b68`.
- **audit_pass: yes** — scope only. Critical 0; Important 0; Minor 0.
- Requested reviewer: Astra/low. Effective model/effort attestation is not exposed independently to this reviewer; effective settings remain unverified.
- This consumes the one focused scope rereview. No further scope correction/rereview slot is inferred.

## Bounded examination

Read only the change-record diff `8c68b68..5eaf18c`, the relevant existing endpoint validation at preserved candidate `cfe9160` (`R/integration_endpoints.R`, approximately 125–210), and accepted M7-010's representation requirements at scope commit `5eaf18c` (approximately 88–120). No broad reread, implementation, fitting, child agents, or repository edits.

The existing endpoint validator hardcodes `levels` for sample/visit-level maps (167–193), so a correctly declared trajectory `subject_centered` map cannot pass. The new allowlist entry is a necessary consumer compatibility seam within C unit-map composition. It does not authorize numerical transformation or a general relaxation of representation validation.

The amended record explicitly preserves one-to-one source membership, subject/time lineage, existing levels/mean/delta behavior, and all mean/delta rules. It requires native trajectory binding plus negative tests for arbitrary representation labels, missing subject/time identity, wrong source cardinality, and map mismatch. Therefore merely admitting the label `subject_centered` without proving its trajectory context is not compliant implementation. Retained/excluded coverage, nonduplicated source membership, context identity consistency, and signed delta weights remain required existing guards.

M7-010 binds the exact uncentered response handed to C separately from its fitted centered method view and centering rule. The correction preserves this distinction expressly: an analysis representation label is not an assertion that handoff values are already centered. No matrix mutation, new centering operation, provenance-schema change, or protected IntegrationResult shape change is authorized by this addition.

All prior scope constraints, inherited native B→C→D verification, attempt limits, final immutable-snapshot Astra gate and M7/G7 closure remain unchanged. This pass permits the coordinator to resume the bounded implementation under the amended allowlist; it does not establish product correctness or scientific validity.

## Required correction

None. At implementation review, verify that the new branch is narrowly conditioned on native trajectory identity, rejects the specified negative cases, and leaves existing levels/mean/delta checks intact. No M7/G7 or downstream release follows from this scope audit.
