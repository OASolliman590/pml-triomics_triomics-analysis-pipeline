# C-06 shared repair 1 focused rereview — 2026-10-21

The sole focused rereview compared repair `be2a4e4` with initial shared
candidate `1cc35048907ab4dd866d7129759ddcb7672d05eb`. The exact three-file
allowlist was preserved. Independent focused verification passed 266 assertions
with no failure, warning or skip, and a fresh C-06 CLI wrote readable partial D
artifacts.

The B nomination finding is addressed, including complete-empty and absent
branches. Source ancestry and real preparation states are present. Nested C-02
no longer aborts with `C_EVAL_EVIDENCE_INVALID`. Four Important findings remain:

1. The already accepted, successful C-02 nested evaluation is reclassified as a
   required single-fit failure: CLI exit 4 and run unavailable. Its accepted
   contract is exit 0 with 12/12 nested predictions. Requiredness must follow the
   requested evaluation, not the absence of a single-fit C-06 export.
2. A nested-only D handoff receives no root identity and serializes
   `study_id="unknown"`, `analysis_id="unknown"` instead of the verified C-02
   identities.
3. Preparation envelope inputs use semantic state hashes in a field that
   promises byte-level artifact SHA-256. Fresh output proves the values differ.
4. Explicit unrun C-07 controls unconditionally create a 2×2 future split plan,
   imposing an unrequested feasibility/grouping policy. A three-unit exploratory
   case fails `A_STATE_SPLIT` even though C-07 was not run.

AC1 remains partial/fail; AC4 fails because accepted C-02 status/exit semantics
regress. Repair 2 must preserve nested success and verified identities, bind
preparation state semantics separately from persisted-file hashes, and represent
unrun controls without inventing a split policy. There is no second rereview;
repair 2 receives direct Sol inspection and verification only.

