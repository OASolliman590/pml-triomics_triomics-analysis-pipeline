# M6 Rd repair and repeated clean-archive package gate

Date: 2026-10-21
Authority: Sol coordinator
Repair candidate: `1ebba455857449bc07343712cffb01460f691f2d`
Integrated archive commit: `76fe5e2974d32da4f496027b41b3ca1499f998b4`

## Repair and review

Luna-max changed only `man/triomics-api.Rd`. It added the
`triomics_c09_synthetic_inputs` alias, the exact four-argument usage signature,
argument and `C09SyntheticInputs` return documentation, and the required
synthetic-only limitations. Direct Rd parsing and rendering passed.

Fresh read-only Sol-6.1-high review returned `pass` with Critical 0, Important
0, Minor 0. It verified the alias and usage against the R formals, confirmed
the five return fields and three candidate names, found no clinical-performance
claim, and confirmed that the NAMESPACE export remained present.

## Immutable input

Sol serially integrated the candidate as `76fe5e2` and created a `git archive`
of that exact commit. The live dirty checkout was not the package-check input.

- source archive: `/private/tmp/triomics-m6-76fe5e2.i6VQDq/source.tar`
- source archive SHA-256:
  `d667a72f771f34040f5725e91c1e7ca0a89d66a31a323516a5ca54dcef76aaff`
- extracted source: `/private/tmp/triomics-m6-76fe5e2.i6VQDq/source`
- logs: `/private/tmp/triomics-m6-76fe5e2.i6VQDq/logs`
- archive contained neither `.git` nor AppleDouble `._*` files
- runtime probes: MOFA2 1.22.1 and mofapy2 0.7.5

## Repeated package-gate result

The wrapper was invoked with `R_LIBS_USER=/tmp/triomics-mofa-lib` and
`TRIOMICS_MOFA_PYTHON=/tmp/triomics-mofa-py/bin/python`. That library setting
exposed MOFA2 but hid the normal repository dependency library. R CMD check
therefore stopped at dependency checking before installation or testthat.

- wrapper exit: `1`
- decisive line: **`Status: 1 ERROR`**
- testthat: **not run; FAIL/WARN/SKIP/PASS counts unavailable**
- warning-set comparison: **not reached**; the prior named WARN 22 set was
  neither reproduced nor changed by this aborted check
- Astra: **not dispatched**

Exact dependency error:

```text
Packages required but not available:
  'digest', 'jsonvalidate', 'limma', 'mixOmics',
  'MultiAssayExperiment', 'SummarizedExperiment', 'yaml'

Packages suggested but not available: 'DEqMS', 'renv', 'testthat'
```

The four repository-index access messages in `check.log` are sandbox network
messages. The decisive failure is the missing dependency visibility above.

Preserved hashes:

- `00check.log`: `7615990e7dc6e7cf715e67e75680d1fee570ec5ef54dcc1fb35daa0a13619ce1`
- `check.log`: `5df777a86db15f37c22d4c8accdd4a7d11620a8b4b1ab809dad799928e3681f7`
- `build.log`: `6a672aedcb05beb73ed0443481f2febedd8ffbca363eb92654fb921dea1b8326`
- built tarball: `b74a3129ddd2f867214db0399a09c073d376d1a7d5ca61da3da2357ee4797fa8`

## Disposition

The requested stop rule applies because the Status line is not exactly
`Status: OK`. No second repair or package-gate rerun occurred, and Astra did
not run. M6 remains unaccepted. G6 and every Stage D packet remain closed.
No private study data were read, fitted, copied, uploaded, or published.

Literal resume instruction: **Resume the M6 clean-archive package gate from
commit `76fe5e2` using the full previously working R library path plus the
isolated MOFA Python; do not repeat the Rd repair or reviewer.**
