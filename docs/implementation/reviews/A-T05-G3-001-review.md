# A-T05 G3-001 — Sol review

Date: 2026-10-14

Reviewer role: fresh Sol 6.1 high, read-only

Candidate: `e969279d356c1891204e9eae5be9c3d94d44c4df`

Base: `11e3cf73a032ffb1c16c78e4dd0004d8d4e5fd20`

Verdict: `CHANGES_REQUIRED`.

## Compliance and correctness

The implementation satisfies the scientific behavior authorized by G3-001.
kNN/RF return `unavailable / qualification_unavailable_in_v2` before inspecting
caller qualification. Production imputation cannot dispatch their retained
private helpers. `prepare_fit` refuses both without input mutation. `none`,
training-only median, and AC1/AC2/AC4 behavior remain covered. No replacement
advanced imputer was introduced.

The reviewer independently reran the exact A-T05 suite: 125 passing assertions,
zero failures, warnings, or skips. Running the final committed tests against the
base profile functions exited 1 with 10 failures and 111 passes, including both
caller-authored activation exploits. This independently confirms that the tests
detect the rejected behavior. The worker's earlier 8-failure/107-pass RED count
was from an earlier test draft and is not the final regression count.

The Stage A CLI exit 0 and full-suite 2,321-pass results remain worker evidence;
the reviewer did not rerun them. CLI ingestion alone does not prove preprocessing
refusal.

## Blocking finding

**P2 — committed-file boundary.** `artifacts/acceptance/A-T05/receipt.md` was
committed outside the packet's exact allowlist. Generated evidence may remain
local, but the packet may commit only its source, test, fixture if needed, and
registration handoff. Remove the receipt from the candidate Git diff and retain
it only as local evidence.

No additional production correctness or code-quality blocker was found. A
focused repair and re-review are required before integration.
