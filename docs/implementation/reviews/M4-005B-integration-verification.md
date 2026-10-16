# M4-005B / M4-007 / M4-008 coordinator integration verification

- Date: 2026-10-16
- Reviewed candidate: `f59abdf467cca274a1174c13be8daacb50e01a01`
- Integrated tip before this receipt: `f83aeda`
- Integrated commits: `f486232`, `df6b4fd`, `053d8ae`, `d76bc1e`, `b3e7c95`, `8ef8c9b`, `595bac5`, `f83aeda`
- Candidate-path identity: integrated blobs are byte-identical to candidate `f59abdf`
- Complete local suite: 3,169 passed, 0 failed, 0 warned, 0 skipped
- Fresh package build/check: `Status: OK`; no package error, warning or note; `.superpowers/` absent from the tarball
- Fresh parity probes: eight exits 0—source and installed B-T04, B-T05 unavailable, B-T05 explicit limma fallback and B-T06
- Preserved evidence: B-T04 effect 1.5, ordinary df 7, design rank 5 and passing calibration; B-T05 method/unavailability/fallback provenance; B-T06 complete family accounting and rank sensitivity
- Reviews: M4-005B Sol PASS, M4-007 Astra/Sol PASS and M4-008 Astra/Sol PASS, all with no findings
- Disposition: immutable shared M4-005 Astra audit is now required; B-T05/B-T06 remain blocked and B-T07 remains unreleased until Sol accepts that audit
- Privacy: public synthetic fixtures only; no private study data were read, fitted or uploaded

