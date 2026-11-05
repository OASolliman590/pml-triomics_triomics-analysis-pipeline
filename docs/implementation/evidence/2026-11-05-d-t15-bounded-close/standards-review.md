# D15 final focused Standards review

Candidate: 518ae4ae6b2b3aec282e1c1f45fa46c0509954b0. Reviewer: /root/d15_standards_review, requested gpt-6.1-sol high; effective settings not exposed. Read-only static review. Verdict: changes_required; one P1 remains.

P1 D15-REVIEW-ABSENCE-01: reject absent-versus-present contradictions. R/report_clinical.R:444 returns absent for a D-pinned unavailable member with four null references, even when loaded clinical payload/artifact fields exist. The section exports their values, merely marking lineage unverified. These values are outside D's pinned evidence, contrary to DATA_CONTRACTS.md:132. Genuine absence can remain unavailable; contradictory present evidence must be rejected or excluded. The no-matching-member branch at line 410 has the same gap. The current absence test removes both loaded fields.

The repair correctly compares hashes and status for referenced partial members, preserves intact partial evidence and structured errors, and stays within the two permitted files. No actionable heuristic concerns. All 1,104 snapshot hashes and the repair diff matched. No tests, fits, edits or acceptance. Worker source receipt: 274 passes, zero failures/errors/warnings/skips, 47 blocks. Parent final gates remain pending; G8/G9 remain closed.
