# M7-021 precursor verification and scope release

Verified immutable commit `4e8c1c2ee24f8150ebc2b67e29a40c8595816fa9`; clean isolated source. This is dependency evidence for the new D02/D03 cycle, not M7 acceptance. C06 registration remains false pending final same-snapshot receipt reconciliation.

Astra-low scope audit: **audit_pass yes**, one initial review plus one focused rereview. Ledger parse error and D05-before-D04 dependency ordering were corrected. Historical packet fields/events were preserved. Requested gpt-6-astra/low; tool did not expose independent effective-setting confirmation. No implementation acceptance implied.

Focused command: `Rscript --vanilla /tmp/triomics-stage-d-evidence/run-focused.R /tmp/triomics-m7-021 'B-T07|C-06|D-T01' /tmp/triomics-stage-d-evidence/m7-021/predecessor.json`. Exit **0**; **PASS 1087, FAIL 0, ERROR 0, WARN 1, SKIP 0**, 60 test blocks. Warning: **D-T01 refuses stale paired signs and preserves the old pointer** — native D subprocess exit 2 is the expected rejection; unchanged previous pointer is asserted. No warning suppression.

Native proof command: `Rscript --vanilla /tmp/triomics-stage-d-evidence/m7-021/native-proof.R`; exit **0**, **23 assertions**. Its native synthetic B, C and D commands exit **0 / 0 / 0**. Read-back: SYN_12 time is JSON **null**, excluded from score/training. Public B/C provenance validation passed. Independent `python3 /tmp/triomics-stage-d-evidence/m7-021/native-readback.py` exits **0**, matches all four B predecessor SHA-256s in C and validates D generation/member bytes and sizes. Full paths/hashes are in the adjacent evidence JSON and manifest.

Key SHA-256 values:
- B handoff: `bc59a37245edfc38da29c28f84efcf5b6aba23d865644e6d4892b42c8d0e7070`.
- C profile: `7a626c910c7691a89d702aab16f89dfc8b55706861e1a67d586b030e26e7b101`.
- D generation: `161fb953e8ff18d9a34ab025f9bee1aa58c789dc786984f999a79c49d1979599`.
- Endpoint: `6f63d224d7bcd7f29b74a62898c4fab48b3a366f50977f790eb3685d76546b86`.

Isolated runtime restored without changing project locks/user library: MOFA2 1.22.1 and mofapy2 0.7.5 load successfully under R 4.6.1/Python 3.12.14. Temporary paths are `/tmp/triomics-runtime-20261103/R` and `/tmp/triomics-runtime-20261103/python/bin/python`. Native training/full installed qualification remains for final gates. Spec validator still reports the inherited AGENTS.md mismatch; direct manifest comparison finds AGENTS.md/CLAUDE.md only (2/92). No checksum was rebaselined. Independent spec-audit metadata remains separate.

Transition: fresh `/root/m7_021_luna`, requested gpt-6-luna/max, isolated `/tmp/triomics-m7-021-luna` at 4e8c1c2, bounded D02/D03 mapping scope only. Sol reviews and verifies before downstream D04. M7/G7 closed; C07 not_run; C09 unqualified; conditional D06/07/14 no completion credit. No private data, push, AIU or fasting run.
