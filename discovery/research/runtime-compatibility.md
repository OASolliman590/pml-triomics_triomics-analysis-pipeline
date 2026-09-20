# Runtime compatibility and MOFA release-source verification

Date: 2026-09-05. Classification: source/metadata verification plus an unaccepted environment proposal. No package was installed, imported or executed. No renv lock, Python environment or pipeline code was created. This closes part of the factual environment investigation, not ENV-01/02 or API-01 runtime validation.

## ENV-Q01 — What concrete R/Bioconductor pair should be proposed?

**Proposed answer / design choice:** use R 4.6.1 with Bioconductor 3.23 as the first reference environment, subject to the maintainer's explicit language/dependency confirmation and designated host. This is not a claim of tested compatibility or a silent change to the package's supported R range.

**Verified metadata:** [Bioconductor installation, Get started §1](https://bioconductor.org/install/) pairs release 3.23 with R 4.6.0. [CRAN Windows release](https://cran.r-project.org/bin/windows/base/) lists R 4.6.1, page last changed 2026-06-26. The [MOFA2 3.23 build report](https://bioconductor.org/checkResults/3.23/bioc-LATEST/MOFA2/) records an upstream Ubuntu 24.04.4/R 4.6.1 build, dated 2026-08-12, with install/build/check OK. That is upstream evidence, not a triomics test or evidence for this Windows host.

The observed [mixOmics 6.36.0 metadata, Details](https://bioconductor.org/packages/release/bioc/html/mixOmics.html) requires R >=4.4.0. [MultiAssayExperiment 1.38.0, Details](https://bioconductor.org/packages/release/bioc/html/MultiAssayExperiment.html) requires R >=4.5.0. Therefore the current catalogue cannot simply be installed under R 4.3 because the user wrote >=4.3. Supporting an older runtime would require a separately selected release set and checks.

**Effect on specification:** propose one concrete release pair first; record the actual host, OS, compiler, BLAS, exact package sources and transitive dependencies when approved. Do not combine arbitrary current and archived BioC packages to satisfy one minimum version. The decision remains DEC-01 pending. Other core version observations remain in A/B/C dependency tables; they are not an environment lock.

## ENV-Q02 — Which Python version does released MOFA2 actually pin?

**Verified source:** downloaded the public [MOFA2 1.22.0 source archive from the explicit 3.23 repository](https://bioconductor.org/packages/3.23/bioc/src/contrib/MOFA2_1.22.0.tar.gz) to a temporary inspection file. Size 4,676,753 bytes; SHA-256 `128C1B4DC889970A9F17363D3504801D1BA5563C094AE7D1FD5840BC360DB19D`. Read DESCRIPTION, R/basilisk.R and R/run_mofa.R using tar-to-standard-output; no archive extraction into the project and no source execution.

DESCRIPTION identifies version 1.22.0, RELEASE_3_23, source commit abbreviation b132e6c, publication 2026-04-28. Its older Date field is 2023-01-12; that field is not the release date. R/basilisk.R declares:

| Environment member | Released source pin |
|---|---|
| Python | 3.12.12 |
| mofapy2 | 0.7.3 |
| numpy | 1.26.4 |
| scipy | 1.12.0 |
| pandas | 2.2.1 |
| h5py | 3.10.0 |
| scikit-learn | 1.4.0 |
| dtw-python | 1.3.1 |

These are declared pins, not a resolved environment: transitive artifacts, channels, build strings, platform availability and licenses remain unverified. Runtime install/training must still succeed on the approved host. The [MOFA2 release LICENSE](https://bioconductor.org/packages/release/bioc/licenses/MOFA2/LICENSE) identifies LGPL version 3; this does not establish the terms of every dependency or a distribution plan.

**Proposed policy / not adopted:** first verify the release's own environment (Python 3.12.12, mofapy2 0.7.3), rather than silently pairing MOFA2 1.22.0 with the newest Python package. Choosing a newer bridge remains possible after an explicit, tested compatibility decision. The optional basilisk provisioning route itself must be approved and provisioned deliberately; a training call must not become an undocumented package download.

## ENV-Q03 — Do release and development version checks behave identically?

**Verified source:** release R/run_mofa.R defines run_mofa(object, outfile = NULL, save_data = TRUE, use_basilisk = FALSE). In the direct reticulate branch, a major/minor mismatch marks the bridge unavailable and subsequently stops; a patch mismatch warns and can continue. Its warning calls the source's target the latest version, which is not proof of current PyPI currency. The basilisk branch calls basiliskStart(mofa_env); execution-process isolation depends on basilisk configuration/version and must be verified, rather than inferred from this call. Source inspection does not verify import, solver, HDF5, numerical or warning behavior at runtime.

**Version contradiction resolved at source level:** cached unpinned master pages returned DESCRIPTION 1.23.1 and a 0.7.4 target, while a separately cached run_mofa example mentioned 0.7.5. Those mixed snapshots cannot establish a coherent environment. Authenticated GitHub API inspection pinned current master to [4a102f2b491ab600738ddf693a70f0baf032ca68](https://github.com/bioFAM/MOFA2/commit/4a102f2b491ab600738ddf693a70f0baf032ca68), committed 2026-08-26. At that exact commit, DESCRIPTION is **1.23.2**, basilisk uses **Python 3.12.10 / mofapy2 0.7.5**, and the version-check helper differs from release. These development settings must not be described as MOFA2 1.22.0 behavior.

| Development file inspected at that commit | Git blob SHA |
|---|---|
| DESCRIPTION | 7109a6634893bda124ea0ee42e724d6db8426c78 |
| R/basilisk.R | e39e1fed0d23951e81e44c6f2177987e13be8261 |
| R/run_mofa.R | d9366245c847ae9428ece30f23ea9a878d59945f |

[PyPI mofapy2 0.7.5](https://pypi.org/project/mofapy2/0.7.5/) was released 2026-08-26 and declares Python >=3.8,<4.0 and LGPL-3.0 metadata. Its published source SHA-256 is dbd6b6b478afcb534edbedd3f703cbd4c4850cc23a4207164b39d2e1fb866464. This archive was not downloaded or installed; the hash is provider metadata, not a local checksum. Newest-on-PyPI and targeted-by-the-R-release are distinct facts.

## Verification artifacts and remaining acceptance

Inspection operations: gh api repos/bioFAM/MOFA2/commits/master, then contents queries with the full verified ref; Invoke-WebRequest for the explicit release archive; Get-FileHash; tar -xOf for the three named members. The temporary archive is at C:/Users/salma/AppData/Local/Temp/triomics-MOFA2-1.22.0-source-inspection.tar.gz. It is not a durable Git artifact and contains upstream assets that are not triomics fixtures. Only this evidence record belongs to the proposed project publication. Re-download from the versioned URL and verify the recorded hash if the temporary file disappears.

Next checks, all NOT RUN: approved host/runtime inventory; release-source/import signature comparison; deliberate environment provisioning and complete resolved lock; synthetic model training/reload; R/Python/HDF5 version provenance; bridge mismatch cases; basilisk version and shared/fork configuration with observed process/isolation behavior; CPU/thread/seed reproducibility; failure without authorized provisioning when the environment is missing. Do not call upstream build results a passed pipeline test. Reticulate/renv current CRAN page retrievals failed in this pass; their exact version selection remains pending. No release/development switch, language choice or new dependency has been approved here.
