# A-T06 G3 repair 1 — Sol review

Date: 2026-10-14
Reviewer role: fresh Sol 6.1 high, read-only
Candidate: `6903639f86cc54a3c6b1e4c2cde9b8ef84aefd36`
Parent snapshot: `d96ca310cea5715f40e2cae72d59a29d411713bb`
Integration commit: `62b2e7d`

Verdict: `ACCEPTED` for the bounded G3-02 repair.

Only `R/lipid_derivation.R` and `tests/testthat/test-A-T06.R` changed. The
reviewer reran four test blocks with 171 assertions and the exact Stage A CLI;
both commands exited zero with no test failures, warnings, or skips. An
independent six-method/basis probe added 96 successful checks.

Carbon and double-bond endpoints now identify weighted composition indices with
property-count units, while the saturation ratio is dimensionless. Arbitrary
signal units cancel in these formulas without implying molar weighting. The
review traced the metadata through fit, persisted JSON state, and apply, and
confirmed invariance under common weight scaling. Chain and sn-position
limitations remain explicit.

The supplied CLI fixture requests class sums, not weighted endpoints, so it does
not itself demonstrate weighted-endpoint export. Direct behavioral and state
round-trip evidence covers the repaired contract. No real-data scientific
validity is established.
