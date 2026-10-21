# M6 clean archive package gate — successful resume

Date: 2026-10-21
Authority: Sol coordinator
Implementation snapshot: `76fe5e2974d32da4f496027b41b3ca1499f998b4`

## Immutable input and runtime

The run re-extracted the verified `git archive` of exact commit `76fe5e2` into
a fresh temporary directory. The live dirty checkout was not used. The archive
SHA-256 was
`d667a72f771f34040f5725e91c1e7ca0a89d66a31a323516a5ca54dcef76aaff`;
the extracted tree contained neither `.git` nor AppleDouble files.

The command was `Rscript --vanilla tools/check-package.R --root <archive> --output
<logs>` with the normal R library plus isolated MOFA2 1.22.1 from
`/tmp/triomics-mofa-lib`, and mofapy2 0.7.5 through
`/tmp/triomics-mofa-py/bin/python`. All required and suggested packages were
confirmed loadable before launch. No product file was edited during the check.

## Decisive result

- Wrapper exit: `0`.
- Exact `00check.log` status: **`Status: OK`**.
- Installed testthat: **FAIL 0, WARN 22, SKIP 0, PASS 4787**.
- Documentation, usage, examples, installation, loading, static code and tests
  all ended `OK`.

Evidence hashes:

- `00check.log`:
  `3cb325b43b35703aceab8cebd39c2e5991ff0475f054f6cb7920eb9a6f7ca013`
- `check.log`:
  `a6b80992aa93b67dfa7100d66ac70af734bf33aa6978a5ee77ff516fff563283`
- `testthat.Rout`:
  `6983b076649162f5d641a525443cfc150591262c67a09911cbd7cc73fe7d975c`
- built source tarball:
  `856cb62fa32edd1a2b5964c1f72f05cb562a1cf5d6015e84d1e8a66fe342a363`

## Named warning disposition

The 22 warnings match the already accepted inventory; none was suppressed.
R code, tests, NAMESPACE, DESCRIPTION, schemas and the frozen spec are unchanged
from the prior named-warning snapshot `c5e65f9`, and the B-T07/C-05 test blobs
are identical.

Eight B-T07 missing-Git-metadata probes:

- 1 — `B-T07 AC1 exports full public bundles without dropping failures or lineage`;
- 1 — `B-T07 AC2 independent and paired CLI smokes retain signed effects and unavailable methods`;
- 1 — `B-T07 AC3 validates the matrix hash and blocks unverified provenance`;
- 4 — `B-T07 AC4 refuses reuse if any reproduction or acceptance artifact is absent`;
- 1 — `B-T07 reproduction wording follows the verified privacy classification`.

Fourteen native MOFA small-fixture diagnostics occurred under:

- `C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes`;
- `C-05 real MOFA2 fits and extracts a missing sample-view capability cell`.

Their accepted inventory is five low-feature-count power warnings, four
factor-versus-average-expression cautions, three single-factor correlation-QC
`max(..., na.rm=TRUE)` warnings, and two all-low-variance factor-retention
warnings. The transcript again showed the native small-view/small-group
messages and the same aggregate count.

## Limits and disposition

This is synthetic software and package evidence, not real-study, biological or
clinical-performance validation. C-09 remains `synthetic_fixture_only` with no
qualified real endpoint, deployment or uncertainty protocol. C-05 remains
exploratory and C-07 controls remain unrun. The successful package gate permits
the immutable M6 review; it does not itself accept M6 or release G6.

