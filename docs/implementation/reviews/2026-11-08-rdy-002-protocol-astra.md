# RDY-002 protocol and runner-scope audit

Fresh read-only Astra requested `gpt-6-astra`, effort `low`, reviewed immutable
`7afe8082e7a3d97d49ea50ec95917d72163b0ff6`. Verdict: **pass; audit_pass yes**.
No P0/P1 finding. Effective runtime model/effort was not exposed.
This is initial audit 1; focused rereview 0. RDY-001 was not restarted.

The reviewed [protocol](../readiness/RDY-002-c07-qualification-protocol.md) SHA-256
is `8a3b2664210db9e6aeda6d9b901f02027dcc8ab71ef6c51e37b99dffcaf44780`;
the [machine plan](../readiness/RDY-002-c07-protocol.json) SHA-256 is
`d926eb237db5c2a4cce875292b37bed3af03aa57ee3884ed6b93b894309e479e`.
Archive SHA-256: `294cc00e0428034932ae365e0f88bb764296c7baca0ae0d7868ec180d6e37ae0`.

Astra checked shared synthetic identity, nested training-only preparation,
the frozen strong-control engineering criterion, adversarial subspace scoring,
failure denominators, uncertainty, native API compatibility and saved-model
read-back. It verified the maximum: 2,400 predictive + 40 PCA + 80 MOFA +
30 leakage-mutation fits = 2,550. Scientific motivation was checked against
[Varma–Simon](https://doi.org/10.1186/1471-2105-7-91) and
[MOFA+](https://doi.org/10.1186/s13059-020-02015-1); these do not prove the
engineering recovery threshold.

Nonblocking implementation requirements: a child process receives the smaller
of its per-call timeout and remaining global time. Terminate only run-owned
descendants, preserve precharged reservations after a crash, and inspect native
MOFA defaults before optimization using the existing option helpers.

Sol authorizes one fresh Luna-max implementation with at most two targeted
repairs, limited to new `tools/run-c07-qualification.R` and
`tests/testthat/test-RDY-002-c07-qualification.R`. First return failing behavioral
tests for Sol inspection, without fitting. No native run starts before that
inspection; all later native calls consume the frozen budget. Sol owns final
verification and acceptance. Production code, CLI publication, registration,
schemas, statistical methods and milestone states are outside this scope.
Synthetic qualification never supplies real-study ancestry or clinical validity.

No model was fitted in this audit. C-07 qualification remains pending. Existing
Stage D/M1–M9 acceptance and released G7/G8/G9 are preserved. Private readiness
inspection stays local; no private rows, identifiers or source paths were sent
to the reviewer. No push or Notion sync.
