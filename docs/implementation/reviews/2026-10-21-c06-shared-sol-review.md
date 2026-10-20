# C-06 shared CLI fresh Sol review — 2026-10-21

Candidate `1cc35048907ab4dd866d7129759ddcb7672d05eb` was reviewed read-only
against integrated base `634c29f`. The diff contains exactly `R/cli.R`, the new
shared C-06 test and the C-06 registration. Independent focused verification
passed 172 assertions with no failure, warning or skip. A fresh C-06 CLI run
wrote valid native and D artifacts with verified payload/envelope hashes and
honest partial status.

Verdict: **changes required**. Critical 0, Important 4, Minor 1.

1. The CLI always passes `primary_nominations=NULL`, so it cannot consume a real
   immutable B nomination artifact and always labels that branch not requested.
2. Complete D integration evidence receives incomplete provenance: source roles
   and selection are placeholders and preparation-state hashes are null despite
   available ingest/preparation declarations.
3. The change regresses accepted nested-CV execution. The exact C-02 CLI finishes
   evaluation, then the C-06 unrun-control path uses a null root unit map and
   aborts `C_EVAL_EVIDENCE_INVALID`, exits 2 and writes no D manifest/run registry.
4. Required incomplete integrations do not enforce the declared requiredness/exit
   policy; a partial aggregate can still return process exit 0.
5. Minor: a digest of an unrun-context record is stored under
   `split_plan_sha256`, although no split plan was executed.

AC1 fails at the shared seam; AC4 is partial. The candidate is not ready to
integrate. Repair must consume/verify an applicable B artifact including the
complete-empty case, propagate authoritative lineage, preserve accepted C-02
nested execution with real regression coverage, enforce requiredness without
erasing member dispositions, and represent an absent unrun split plan honestly.
This review neither accepts C-06 nor releases G6.
