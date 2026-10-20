# C-08 Sol acceptance

Date: 2026-10-20  
Accepted snapshot: `d69ef1e`  
Evidence class: public synthetic software evidence only

## Outcome

C-08 is accepted. The integrated route preserves one exact fitted backend,
binds immutable score endpoints to its fitted state, and schedules a separate
B-owned clinical score-association request without executing a B model or
recursing into Stage C. C-09 and C-06 are released by dependency; G6 and Stage D
remain closed.

Integrated implementation commits are `bd34cb8`, `519f48c`, `b45f31a`,
`c5535fb`, `2170820`, and installed-fixture correction `d45c88f`. The last
correction changed only the shared test and registration.

## Review and focused verification

- Initial packet review found four Important defects; repair-1 rereview retained
  three Important defects. Both packet repairs were consumed and integrated
  only after direct verification.
- Fresh shared-diff review of `b579bfd` found two Important defects: discarded
  source-selection ancestry and a post-hash schema mutation. Targeted repair
  `9338bd8` passed the single focused rereview with Critical 0, Important 0,
  Minor 0.
- Fresh integrated tests: C-08 PASS 288, C-01 PASS 109, C-03 PASS 82, C-04
  PASS 132; every focused run had FAIL 0, WARN 0, SKIP 0.
- Exact Stage-C CLI exited 0. JSON read-back validated the endpoint bundle and
  all three envelopes. The request remained B-owned, `scheduled/not_started`,
  empty-result and non-recursive. Its source bundle hash matched
  `0559bcb352370fc719a923b31b787ddb829fc02bff4d35ff16f5b299e08d7879`.

## Clean archive package gate

The first package attempt correctly failed three installed-context C-08 tests
because their fixture path was source-only. After the narrow fixture resolver
correction, the clean archive of `d69ef1e` at
`/private/tmp/triomics-c08-pkg4.ikfCkd` passed:

- `Status: OK`
- testthat: FAIL 0, WARN 22, SKIP 0, PASS 4367
- source archive SHA-256:
  `3add3184bc71a12998eec8975fe57f012a1f7c0321cb7dca27f5588eee72051f`
- `check.log` SHA-256:
  `6451fce47a1e6bc3d3a5aeb9b58411d455e1f3d85dac6e32232ce4e44d83c548`
- `00check.log` SHA-256:
  `4b529369df742964cbbb4b32439788a3f9c881b42f98addf382b0a8f0d20c2d7`
- `testthat.Rout` SHA-256:
  `9e39253521cfbd9ef74800432bb7a7cef38222ebe868932aa75e87760927e67b`

The 22 warnings are not new and are not suppressed: eight are the accepted
B-T07 installed-artifact missing-Git-metadata probes, and fourteen are native
MOFA small-fixture/QC diagnostics. The two failed-file-open warnings from the
first package attempt are absent.

## Acceptance criteria

- **C-08-AC1 accepted:** PCA, native DIABLO and native block.spls endpoint
  shapes bind same-fit state, scores, inputs, units, selection and training-Y
  lineage. The public IntegrationResult remains six fields and fit-count
  instrumentation observes one adapter fit.
- **C-08-AC2 accepted:** Stage C writes immutable endpoint artifacts and a
  scheduled B request; it does not execute or duplicate clinical modeling.
- **C-08-AC3 accepted:** supervised endpoints remain descriptive-only and
  outcome-selected endpoints remain withheld from independent-validation
  release.
- **C-08-AC4 accepted:** unit/visit/time/coverage mismatches fail closed;
  named-list mean maps and direct B-T08/B-T09 consumers pass; a derived delta is
  refused rather than misrepresented as two visit-level scores.

The MOFA endpoint adapter shape is tested at the accepted public seam; C-08 did
not claim a new native MOFA endpoint run. The package gate used the already
verified isolated MOFA2 1.22.1/mofapy2 runtime. No private study was read or
fitted, and no biological association, endpoint validity or predictive
performance is claimed.
