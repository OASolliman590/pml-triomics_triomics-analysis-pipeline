# M7-024 target implementation audit — blocked close

Astra read-only initial implementation audit: **audit_pass no**, immutable
`6e73239ebbca95bc0aab24bb7b7faf78d6a640cf`, based on Sol shared CLI `54f0b69`.
Agent `/root/m7_024_implementation_astra`, requested gpt-6-astra / low;
effective settings are not independently exposed by the native tool.

- P1: unequal coverage is declared comparable. `R/target_alternatives.R:131`
  requires coverage on all four axes, but `R/target_assessment.R:140` provides
  none on external axes. The fallback at alternatives lines155–157 uses only
  top-level measured. Read-only probe: one versus ten expected links and zero
  versus nine unmeasured features returns comparable.
- P1: the recursive guard at alternatives line110 scans the external ledger,
  rejecting axis binding / endpoint Kd (lines2,39–40,101–104). A sourced exact
  identity match attaches as request, then comparison throws
  D_TARGET_CLAIM_COLLAPSED. Native composition calls comparison for two targets
  at assessment line273. Legitimate external assay evidence is not an inferred
  molecular binding claim.

The worker finished its initial implementation and both repairs. Repair1 fixed
context-builder compatibility and explicit typed refusals; repair2 fixed missing
path identity error precedence. The last focused run passed318, zero warnings or
skips, before two fixture tests were appended. Those appended tests and native
B/C/D were not verified. Do not attribute318 to the final candidate. No integration
or acceptance. Candidate and worktree are preserved; no repair3, no old-worker
redispatch. M7-024 is blocked and incomplete. M7/G7 remain closed. A separately
frozen/audited bounded successor is authorized by the current user instruction.
