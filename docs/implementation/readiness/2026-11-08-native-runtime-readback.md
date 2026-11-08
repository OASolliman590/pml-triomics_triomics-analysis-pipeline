# Readiness runtime read-back, without model fitting

Sol located and inspected the previously qualified isolated runtime on 8 November
2026. This resolves availability and source-inspection questions in the earlier
[optional-method research](2026-11-08-optional-methods-evidence.md); it does not
qualify native enrichment or complete D-T06.

Observed versions: R 4.6.1, mixOmics 6.36.0, MOFA2 1.22.1, Python 3.12.14 and
mofapy2 0.7.5. The R and Python version probes each exited 0. Testthat 3.3.2,
digest 0.6.39, jsonlite 2.0.0, callr 3.8.0 and processx 3.9.0 were available.
No package was installed, model fitted or native enrichment executed in this
read-back. The probes do not certify untested hosts or dependency combinations.

Rscript executable SHA-256:
`7004a18d97f2470ffaf51ed4fdfef1785f641f54e1e5c7e30d0f894b6c475d91`.
Python executable SHA-256:
`f681deb489625d7f104c4549f7d15f9fdedf7ef17d2c6d7fbaeadd709ca9f739`.
The combined deparsed `run_enrichment` and `.pcgse` source captured from the
installed MOFA2 1.22.1 namespace has SHA-256
`bfece27b53e05e38fd5d8deda4b37338fd80007badcb8f1c2a82788a981371c9`.
This identifies the observed function representation; it is not a checksum of
the complete release tarball or a proof that all dependency bytes are identical.
Raw receipts remain in the local readiness runtime-readback directory.

The observed `run_enrichment` obtains native data, scaled weights and factors;
removes zero-variance features; intersects feature names with annotation columns;
and filters sets by minimum size. It adjusts p values per factor. Its permutation
branch compares absolute null statistics with strict `>` and divides counts by
`nperm`, without a plus-one term. Directional handling zeros opposite-sign weights
and replaces negative set-statistic results after adjustment. These are now
observations of the installed 1.22.1 function, rather than extrapolations from
development-source documentation. No numeric, dimension-edge-case, missingness,
permutation or resource-rights qualification was performed here.

For future D-T06 qualification, derive independent numeric references from a
verified native model and its actual dependency-produced data, masks and mappings.
Do not substitute a hand-filled model or edited loadings for that lineage. The
four-weight calculation in the research note remains arithmetic illustration
only. Freeze the native arguments, denominator/family, sign interpretation,
resources and negative cases before execution and keep D-T06 blocked until its
own required evidence and review pass.
