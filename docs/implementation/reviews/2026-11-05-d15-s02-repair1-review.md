# M8-D15-S02 repair1 review and final repair dispatch

Candidate `4a09b92fd47aeafe5894389ce9c8fd1d4a8bd3bf` is clean and changes only R/export_b.R and tests/testthat/test-B-T07.R. The worker source receipt records 445 pass, 0 fail/error/warn/skip, 30 blocks, exit 0. Sol read the receipt and independently confirmed the three original rejection probes now pass. No native gate was run on this revision. Initial native evidence is not carried forward as proof of this commit.

## Standards review

The same read-only Sol6.1/high reviewer resolved whole-count validation, exact optional p/q access, predictor-visit matching and correction vectors. One P2 remains: q_published FALSE can retain row q, and the flag is not required to be a scalar logical. No new standards breach or style repair was requested.

## Spec review

The same read-only Sol6.1/high reviewer resolved the three assigned defects but found one P1: false, absent or non-boolean q_published bypasses published-row checks. This violates the audited consistent-family evidence rule. Existing producers remove row q when withholding publication. The 1e-12 serialization comparison does not alter fits, published values, methods or thresholds. Effective model settings remain unexposed; requested roles are recorded accurately.

Sol independently reproduced the remaining defect without fitting: FALSE, missing, string and numeric flags with retained q=0.99 all produce all_accounted TRUE and absent-sibling not_requested. The expected-rejection script exits 1. This pre-existing gap is within the audited family-consistency scope, not a new statistical rule.

The same Luna receives final repair2of2: typed scalar logical publication flag, exact row-q coherence in both branches, artifact-level RED/GREEN tests and clean-commit full B-T07. Only the original two files are allowed. Sol then independently verifies native publication and profile readback. No third repair, integration or milestone acceptance is authorized by this receipt. S01 retains its separate four defects and repairs0of2. M7/G7 remain accepted; G8/G9 closed.
