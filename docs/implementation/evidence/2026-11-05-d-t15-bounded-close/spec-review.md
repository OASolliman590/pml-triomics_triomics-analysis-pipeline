# D15 final focused Spec review

Candidate: 518ae4ae6b2b3aec282e1c1f45fa46c0509954b0. Reviewer: /root/d15_spec_review, requested gpt-6.1-sol high; effective settings not exposed. Read-only static review. Verdict: changes_required; one P1 remains.

The partial-artifact finding is resolved: every present member reference reaches payload, envelope and status comparisons before integrity is marked verified. Intact partial results retain their scientific availability.

P1 D15-REVIEW-ABSENCE-01: pinned absence still permits an unbound clinical payload to display. R/report_clinical.R:437–446 returns integrity_status=absent for unavailable/null-reference members without checking loaded clinical evidence. The no-member branch at line 410 has the same gap. The section later projects loaded effects, objectives and coverage at lines 1155–1159, labelled partial/unverified. A freshly built ReportBundle can reference those current artifacts, but that does not bind them to D's earlier B pin. This violates D-T15.md:15, “Consume only dependency-produced immutable artifacts.” The absence test removes both loaded fields and misses this contradiction.

Earlier C-02 units, B/C transport binding and producer-qualified B classification resolutions are unchanged. Product and repair diff hashes matched. No tests, fits, edits or acceptance by this reviewer. The supplied 274-pass receipt is worker evidence, not final parent proof. Both repair allowances are consumed; G8/G9 remain closed.
