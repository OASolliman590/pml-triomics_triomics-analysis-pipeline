# fda190f installed-warning disposition

Date: 2026-10-20. Archive commit: `fda190f`. This is a warning
disposition under A-T01-AC2; it is not a warning-free rerun and does not change
B-T07 code or its accepted scientific behavior.

## Fresh diagnosis

Sol installed the preserved `fda190f` source archive with tests into a clean
temporary library and reran the installed suites by stage. Installed C-03,
C-04, and all A tickets completed with zero testthat warnings. The installed B
suite reproduced exactly eight warnings, all from B-T07:

- one at `test-B-T07.R:319` in AC1;
- one at `test-B-T07.R:383` in AC2;
- one at `test-B-T07.R:422` in AC3;
- four at `test-B-T07.R:653`, one for each deliberately removed AC4 artifact;
- one at `test-B-T07.R:674` in the privacy-wording case.

Every warning has the same cause: `triomics_b07_git_ref(root)` runs
`git -C <installed-package artifact output> rev-parse HEAD`. The installed
archive has no Git checkout above that output directory, so Git exits 128 and
R exposes the nonzero command as a warning. The helper then follows its declared
fallback and records `working_ref = "unavailable"`. No fit, prediction, score,
variance, matrix hash, privacy classification, artifact content, or acceptance
assertion changes. `unavailable` is not a verified commit hash.

The preserved native check remains:

`Status: OK`; testthat `FAIL 0, WARN 8, SKIP 0, PASS 3985`.

Evidence identity:

- source archive SHA-256:
  `3edc121b735220774135e959e1e781010efe0e9eb3d5cdb431ede996e33e5bfe`;
- `testthat.Rout` SHA-256:
  `dafca78abbce192b94568e969bcde9299a17c2a3afd4379090283aa17f88648a`;
- `00check.log` SHA-256:
  `5bad850bd9a7eaaf4e862cbfeb31710ac542b79c9b6cd7e52e7f805deeac591a`;
- R 4.6.1, testthat 3.3.2, triomics 0.1.0, mixOmics 6.36.0.

## Independent adjudication and Sol disposition

Fresh read-only Astra-low reviewer `/root/c03_c04_warning_gate_astra` found no
Critical or Important defect requiring code repair. It confirmed that
A-T01-AC2 requires the wrapper to fail on errors or **undisposed** warnings,
not to pretend that no warning was emitted. The exact installed-context Git
diagnostics are now named, bounded, preserved, and disposed as expected missing
provenance metadata. Earlier M4 records support the warning-class identity but
do not replace this diagnosis.

Sol therefore closes only the unnamed-warning blocker. The run remains recorded
as `WARN 8`; no threshold, test, assertion, package gate, or repair counter is
waived. Another B-T07 repair or exception is neither authorized nor required.
