# D-T15 bounded close — 5 November 2026

**Blocked and unaccepted.** One initial attempt plus repairs 1 and 2 are complete. The final local reviews and Sol reproduction retain one P1. AGENTS.md hard rule 8 permits “One initial attempt + at most two targeted repair attempts per packet.” No third repair, product integration, G8 release or G9 release occurred.

The preserved worker commit is `e891344e5d7e492030749d88e9645d7f0822c59a`; the byte-equivalent clean Sol snapshot is `518ae4ae6b2b3aec282e1c1f45fa46c0509954b0`. Both isolated worktrees are clean. Canonical product remains the verified `ee43448` predecessor with coordinator records on top. The final snapshot archive SHA-256 is `620e2f4e411d2459088b6c87bca734c3a59ffddaf8f25b2ccf3cb13773892f33`; all 1,104 file hashes match the frozen manifest.

Worker source command: `Rscript --vanilla <local-proof-root>/scripts/run-source-unbounded-report.R <worker-worktree> <local-evidence-dir> 'D-T15|D-T08'`. Exit 0; **274 pass, 0 fail, 0 errors, 0 warnings, 0 skips, 47 test blocks**. D15 contributes 123 assertions in 22 blocks; D08 contributes 151 in 25. There are no testthat warnings to name. These are worker source counts, not independent final acceptance.

Spec and Standards reviewers both returned changes_required for **D15-REVIEW-ABSENCE-01**. D's pinned B bundle may declare no clinical result while the renderer displays a separately loaded result, marking it partial/unverified. Sol's [negative boundary probe](../evidence/2026-11-05-d-t15-bounded-close/absence-probe.json) reproduced effect 0.4, one objective and two coverage rows, with no error or warning. Pinned B bundle SHA-256 remained `1747c467fc3e910faefaeb90b27d165e4debae16fc710875885203bb9e047cdb`. The probe exit 0 confirms reproduction of the defect; it is not a passing gate or proof of a complete native B-to-C-to-D route for that combination.

The initial candidate alone completed the public synthetic native A/C/B/C/D/report chain. Final independent source, installed-example and native-report proofs were not run after the final local review block. No package Status line exists for this candidate; no D15 Astra verdict exists. Earlier passing logs cannot accept the repaired snapshot.

The five-file candidate contains clinical dictionary/coverage, objectives/models, effects/intervals/units, source linkage, inference limits, report template, fixture/tests and an unaccepted registration. Neither repair changed the shared renderer, schemas, producer methods, frozen numerical bounds or seeds. No private data, server execution, push or Notion sync occurred. C-07 checks remain not_run, C-09 remains unqualified, and conditional D routes receive no completion credit.

All three run-owned agents and all owned processes are terminal. The app inventory has no run-owned child chats to archive. No polling remains. The ledger and handover are reconciled. D13 retains its earlier exhausted cap; M7/G7 and D08 remain accepted; M8/G8 and M9/G9 remain unreleased.

Literal resume: **D15 is blocked at initial attempt 1 plus repairs 2 of 2. Preserve worker e891344 and Sol candidate 518ae4a; do not integrate them or reopen repair 3. A separately authorized bounded follow-up must address D15-REVIEW-ABSENCE-01, retain the original caps and evidence, then pass local review, independent source/installed/native report proofs and the applicable fresh Astra gate before acceptance. Do not redo S01/S02 or reopen D13 at its exhausted cap. Keep G8/G9 closed.**

See the [review history](2026-11-05-d-t15-implementation-review.md) and [machine-readable receipt](../evidence/2026-11-05-d-t15-bounded-close/receipt.json).
