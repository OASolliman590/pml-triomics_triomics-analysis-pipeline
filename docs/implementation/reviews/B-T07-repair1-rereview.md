# B-T07 repair 1 focused Sol-high rereview

- Date: 2026-10-16
- Prior candidate: `b7a9455bc14a4c2a062a92ad2d8a68e9c2d2dccd`
- Repair candidate: `25415fbe4d88a684133267696024d43683e33f64`
- Role: fresh Sol 6.1 high, read-only
- Verdict: **PASS for bounded repair; packet not yet accepted**
- New findings: P0 0, P1 0, P2 0

All four initial findings are addressed. Success tests now fail explicitly on a
`triomics_error`, execute the AC assertions and use the nested family payload.
Public result `source_hash` values are compared to actual keyed matrix hashes;
the +1000 mutation fails with `B_EXPORT_MATRIX`, while caller-provided matrices
remain unverified and cannot satisfy integrated AC3. Reproduction and acceptance
payload/envelope pairs participate in final completeness and resume; deleting
any one refuses reuse. Reproduction wording derives from verified configuration
and privacy context rather than unconditional synthetic/no-private claims.

Unavailable-but-present clinical artifacts retain their typed disposition and do
not count as missing. Partial bundles remain non-reusable. Fresh verification
passed 10 cases and 88 expectations with no failure, warning or skip;
`git diff --check` passed and the repair changed only `R/export_b.R` and
`tests/testthat/test-B-T07.R`.

AC1 and AC2 pass packet behavior. AC3 hash/adversarial behavior and AC4 exporter
behavior pass, but integrated same-run AC3 and CLI reproduction remain blocked
until Sol applies the registered shared runner/matrix hook. The packet commits
may be integrated serially; B-T07 acceptance and consumer release remain gated.
