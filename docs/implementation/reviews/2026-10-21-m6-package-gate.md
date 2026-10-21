# M6 clean-archive package gate

Date: 2026-10-21
Authority: Sol coordinator
Archive commit: `c5e65f906a82eff9870fdb9efcfe55546d49b005`

## Immutable input and command

The gate used `git archive` at exactly `c5e65f9`; it did not check the dirty
live checkout. Source-archive SHA-256:
`59b1bb5f444f8262856ecb7805ef6fb504fd81cb7e0b9d6b657deadb116882a5`.
The extracted archive contained no `.git` directory or AppleDouble file.

The archive copy ran:

```text
Rscript --vanilla tools/check-package.R --root <archive-source> --output <log-directory>
```

with the previously verified isolated MOFA runtime: MOFA2 1.22.1 from
`/tmp/triomics-mofa-lib` and mofapy2 0.7.5 through
`/tmp/triomics-mofa-py/bin/python`.

## Result

- Wrapper exit: `1`.
- Decisive `00check.log` line: **`Status: 1 WARNING`**.
- R CMD check warning: exported object `triomics_c09_synthetic_inputs` has no
  documentation entry.
- Failing tests: **none**.
- Installed testthat result: **FAIL 0, WARN 22, SKIP 0, PASS 4787**.
- Dependency, build, installation, loading, static-code, Rd consistency,
  examples, and installed tests otherwise completed.
- Four repository-index access messages appear in `check.log` because the
  sandbox could not reach CRAN/Bioconductor; dependency checking still ended
  `OK`. They are not testthat warnings and are not the decisive package warning.

Preserved evidence:

- log directory: `/private/tmp/triomics-m6-c5e65f9.Zp4Kfr/logs`
- `00check.log` SHA-256:
  `7ad3a843ca70eca69e734a98c7d740209a7edadac90e91b4e78489c8a7d3035c`
- `check.log` SHA-256:
  `57b4883bb1c08eca70ead1e6b876eff878e18ebd1d37171584277e9c115cdda9`
- `testthat.Rout` SHA-256:
  `b20a9b57aa890ab76de88293f7a19f01603ab43d8544e1335ec8cdc9c54f0e8a`
- `build.log` SHA-256:
  `b881f5ed990c6b7e2478ddebe84634c7d3380a7ba26af1460a4025f4f96fe0dc`
- built source tarball SHA-256:
  `d414da07d3ee8c10ed31c6823bd0ab586837ab2922bc8256c1bd938c63a96d56`

## Named testthat warning inventory

The 22 warnings reproduce the already accepted inventory; none was suppressed.

Eight B-T07 installed-artifact Git-metadata warnings:

- 1 — `B-T07 AC1 exports full public bundles without dropping failures or lineage`;
- 1 — `B-T07 AC2 independent and paired CLI smokes retain signed effects and unavailable methods`;
- 1 — `B-T07 AC3 validates the matrix hash and blocks unverified provenance`;
- 4 — `B-T07 AC4 refuses reuse if any reproduction or acceptance artifact is absent`, one for each deliberately removed artifact;
- 1 — `B-T07 reproduction wording follows the verified privacy classification`.

Fourteen native MOFA small-fixture diagnostics occurred under the two tests
that execute the four native fits:

- `C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes`;
- `C-05 real MOFA2 fits and extracts a missing sample-view capability cell`.

Their accepted message inventory is five low-feature-count power warnings,
four factor-versus-average-expression cautions, three single-factor
correlation-QC `max(..., na.rm=TRUE)` warnings, and two all-low-variance
factor-retention warnings. The preserved testthat transcript reports the two
test names' aggregate 14-warning count rather than a per-fit split; no new
warning class or count appeared.

## Disposition

The package gate failed because the Status line is not exactly `Status: OK`.
No repair was made in this run. Astra was not dispatched. C-06 and C-09 remain
M6-pending; M6 is not accepted; G6 and every Stage-D packet remain closed. No
private study data were read, fitted, copied, or published.
