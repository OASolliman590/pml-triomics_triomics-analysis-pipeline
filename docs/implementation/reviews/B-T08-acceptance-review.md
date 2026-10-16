# B-T08 fresh acceptance review

- Date: 2026-10-16
- Reviewer: `/root/b_t08_acceptance_review` (`gpt-6.1-sol`, high)
- Snapshot: `542a7b4`
- Verdict: **FAIL**; P0 0, P1 3, P2 0
- Required suite: 7 tests, 139 assertions passed, 0 failures, warnings, errors or skips
- Source CLI: exit 0; acceptance record, hashes, numeric oracles and privacy checks passed
- Temporary package installation: exit 0; installed CLI exited 0 but omitted B-T08 clinical results and its acceptance record
- AC1 PASS; AC4 PASS; AC2 FAIL because a constant continuous response was labelled estimated with zero SE and `NaN` p; AC3 FAIL because duplicate family members were fitted twice and inflated counts
- Registration FAIL: all six requested B-T08 APIs were internal-only and installed CLI had no explicit B-T08 dispatch
- Five packet-owned files remain byte-identical to implementation `bded42a`; this is a historical implementation/registration gap, not later drift
- Disposition: do not accept B-T08 or release B-T07; use bounded repair 1 and affected acceptance review
- Privacy: public synthetic fixtures only; no private identifiers, paths or biological validation

