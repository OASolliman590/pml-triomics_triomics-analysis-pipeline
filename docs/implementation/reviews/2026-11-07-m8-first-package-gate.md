# M8 first clean-archive package gate

Candidate `ac66bd65a4d99957b1e631cadb73fe3b4e346d2a`, archive SHA-256 `39ebc4959c14c92202d1f196ed7f5bf7809da79ff123090fba171e849ab107ce`, 1,198 files. The archive remained unchanged. The completed command exited **1** and `00check.log` says exactly **`Status: 1 WARNING, 1 NOTE`**. Testthat: **FAIL 0, WARN 15, SKIP 0, PASS 11518**.

The package warning is non-ASCII U+2019 literals in `R/report_targets.R`. The NOTE identifies undeclared `setNames` calls in report frame/config helpers. Neither is waived. [M8-PKG-001](../change-records/M8-PKG-001-portability.md) bounds the correction to equivalent ASCII source encoding and one namespace import; D13 logic/tests/attempts are preserved.

Named test warnings, retained without suppression:

- `C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes`: 11, with exactly matching prior messages/multiplicities (small-view power 3, correlation 4, empty-max 4).
- `C-05 real MOFA2 fits and extracts a missing sample-view capability cell`: 3 (small-view power 1, disabled inactive-factor removal 2), exact prior messages.
- `D-T01 refuses stale paired signs and preserves the old pointer`: 1, the intended native CLI status-2 rejection; generated temporary paths differ.

[Receipt and retained logs](../evidence/2026-11-07-m8-first-package/receipt.json) include raw-log hashes and sanitized public copies. Raw logs and synthetic outputs remain local. The overnight pause and SSD disconnection occurred while the check ran from its internal archive; it was never restarted. The restored SSD HEAD and protected local dirt matched. Wall time is not a benchmark.

Astra milestone calls used: 0. M8 is not accepted; G8/G9 remain closed. Next: the bounded packaging correction, a new immutable archive gate, then fresh Astra. No D13 reimplementation or later packet.
