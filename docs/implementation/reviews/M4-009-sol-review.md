# M4-009 fresh Sol review

- Date: 2026-10-16
- Reviewer: `/root/m4_009_sol_review` (`gpt-6.1-sol`, high)
- Immutable candidate: `5df2043`
- Base: `c876059`
- Verdict: **PASS**; P0 0, P1 0, P2 0
- Focused verification: 30 tests, 407 assertions passed; 0 failures,
  warnings or skips across B-T08, B-T09 and Stage B composition

The reviewer confirmed that constant responses fail closed, duplicate family
members are rejected before fitting, and mixed-family internal p = 1 accounting
does not publish a q-value for a failed constant member. No method or threshold
changed.

All six requested installed exports and documented signatures match. Actual
source and installed B-T08 and B-T09 CLI runs exited zero with one correct
acceptance record each; B-T09 produced no B-T08 output. An installed nonfixture
clinical request exited 2 with `B_DESIGN_UNSUPPORTED` and created no output
directory.

Only public synthetic inputs were used. This is software acceptance evidence,
not private-data execution or biological validation. Both B-T08 repair attempts
are consumed. The next authorized transition is the complete suite and a fresh
warning-free package check, followed by the focused M4-005 Astra rereview.
