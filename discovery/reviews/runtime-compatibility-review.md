# Independent runtime compatibility source/metadata review

Date: 2026-09-05. Target: `discovery/research/runtime-compatibility.md`. Initial complete-review SHA-256 **A374B73A7FAB402E98BD2540131270DF6FB7BC236455B00404B3B56B335D2B1E**; final corrected snapshot SHA-256 **83996473E6378CA8463094F1F639F902D30D58AC5F13CDB257683E3344E43EF5**. Review scope: source/metadata claims and limits, not installation or runtime compatibility. The complete initial record and actual release archive members were read; the final ENV-Q03 and planned-checks corrections were then independently reread and rehashed. No R/Python/package source was executed, no dependencies installed and no environment or code created. Only this review file was written.

**Disposition:** release source identity, eight declared pins and direct-reticulate version-check summary are independently supported. R 4.6.1/Bioconductor 3.23 is a reasonable **unaccepted reference-environment proposal**, not tested compatibility. One P2 wording correction was identified and is **resolved in the final document**: basilisk does not necessarily run in a separate process. Some development/PyPI/upstream-build claims could not be independently refetched in this review and remain attributed to the original inspection rather than newly verified.

## Independently inspected release artifact and hashes

Read-only local archive: `C:/Users/salma/AppData/Local/Temp/triomics-MOFA2-1.22.0-source-inspection.tar.gz`, linked in the target to the [explicit Bioconductor 3.23 source archive](https://bioconductor.org/packages/3.23/bioc/src/contrib/MOFA2_1.22.0.tar.gz).

Archive SHA-256 independently measured: **128C1B4DC889970A9F17363D3504801D1BA5563C094AE7D1FD5840BC360DB19D**, matching the target's recorded value. This verifies the inspected local bytes against the author's record; this reviewer did not redownload the archive or claim an independent server-side checksum signature.

| Actual archive member | Raw bytes | Independently computed SHA-256 |
|---|---:|---|
| `MOFA2/DESCRIPTION` | 2,955 | **39962D2758D2CB3453622BF6019C49CFF8C48FE2122D5EB2CCE9D987FAB75F82** |
| `MOFA2/R/basilisk.R` | 526 | **267CBA2DBA421DC06ACD016DDCC29E5E7C8C92BF1184570C5FAE4AADE531F2D2** |
| `MOFA2/R/run_mofa.R` | 11,972 | **512A76706AE5B871C9E492D66EC25741D15FBF37D8A1B09651137CC4BF8E3FD5** |

Inspection used `tar -xOf` to standard output for the named members. Member hashing captured the raw tar stdout byte stream into memory through the process stdout BaseStream, then computed SHA-256; it did not hash PowerShell-rendered text or normalize line endings. All three tar processes exited 0. No archive member was extracted into the workspace, sourced or imported. The three byte hashes are SHA-256 content hashes, distinct from Git blob SHA-1 identities.

## Claim checks

| Target claim | Independent evidence and disposition |
|---|---|
| Release identity | Actual DESCRIPTION says MOFA2 1.22.0, RELEASE_3_23, commit abbreviation b132e6c, commit/publication date 2026-04-28, older package Date 2023-01-12. **Matches.** Correctly distinguishes Date from publication. |
| Declared Python/mofapy2 environment | Actual `basilisk.R` sets Python 3.12.12, numpy 1.26.4, scipy 1.12.0, pandas 2.2.1, h5py 3.10.0, scikit-learn 1.4.0, dtw-python 1.3.1 and `.mofapy2_version` 0.7.3. **All eight match.** Environment constructor supplies dependencies and mofapy2 requirement; this is declaration, not solved/provisioned compatibility. |
| `run_mofa` public formals | Actual source defines `run_mofa(object, outfile=NULL, save_data=TRUE, use_basilisk=FALSE)`. **Matches.** Actual import/dispatch remains unrun. |
| Direct-reticulate version behavior | Actual source splits installed/target version strings, sets availability false on major/minor mismatch and reaches stop; patch mismatch emits a warning, leaving same-major/minor availability true. **Matches for normally parsed version strings.** No version-mismatch runtime was simulated. |
| “Latest” warning semantics | Target correctly treats the source string as its embedded target, not evidence of current PyPI latest release. **Matches.** |
| Basilisk separate-process claim | Actual `run_mofa.R` calls `basiliskStart(mofa_env)` without selecting shared/fork behavior. **Initially overstated; final wording is corrected; see RC-R01 below.** |
| Bioconductor release/R family | Independently opened [official installation page, Get started §1](https://bioconductor.org/install/): release 3.23 is paired there with R 4.6.0. **Matches.** |
| R patch release proposal | Independently opened [CRAN Windows release page](https://cran.r-project.org/bin/windows/base/): R 4.6.1 and last-change 2026-06-26. **Matches.** Choosing that patch release for the reference environment is a proposed design choice; neither page proves this pipeline's compatibility. |
| Upstream MOFA2 Ubuntu build/check | The cited build-report URL could not be refetched with the browser in this review; a shell read also failed at the configured network proxy. **Not independently reproduced here; not refuted.** Its original report must remain distinguished from a host-specific triomics test. |
| Higher R floors in mixOmics/MAE | mixOmics 6.36.0/R >=4.4 was already independently inspected in the B/C ledger. MAE's specific 1.38.0/R >=4.5 claim was not separately refetched during this bounded pass. **Current-release floor warning is sound, specific MAE metadata remains original-inspection attribution here.** |
| Pinned development commit and three Git blobs | Immutable raw GitHub URLs returned no content; read-only `gh api` contents query failed at the configured proxy before the first response. **This reviewer did not reproduce the development file contents or blob identities.** They are not substituted for the release archive evidence. |
| PyPI mofapy2 0.7.5 metadata/hash | Page refetch returned no content. **Not independently reproduced here.** The target correctly labels its sdist SHA as provider metadata, not a locally downloaded checksum. |

The `run_mofa.R` source also exposes ordinary runtime risks—file replacement, environment availability and HDF5 save/reload—that are outside mere pin compatibility. The target already retains training/reload, provisioning and failure checks as unrun. No additional package behavior is certified by this review.

## RC-R01 — [P2] Qualify basilisk process isolation

**Target:** ENV-Q03 sentence “The basilisk branch starts the declared environment in a separate process.”

**Evidence:** the actual release source calls `basiliskStart(mofa_env)` and `basiliskRun(...)`, without overriding process settings. The independently opened [basilisk 1.24.0 reference manual, `basiliskStart`, Arguments and Choice of process type, pp.4–5](https://bioconductor.org/packages/release/bioc/manuals/basilisk/man/basilisk.pdf) states that shared mode may use the current R session; fork/socket choices apply in other cases. Therefore `use_basilisk=TRUE` alone does not establish a separate process. The same manual documents lazy environment creation when absent; explicit provisioning remains necessary.

Manual metadata inspected: version **1.24.0**, Bioconductor 3.23, RELEASE_3_23, recorded Git commit abbreviation **e0f5793**, commit date 2026-04-28, publication 2026-08-31. The manual was read through the browser; no local PDF bytes were downloaded/hashed in this review, so a PDF SHA-256 is unavailable rather than invented.

**Requested correction:** replace the unconditional claim with “the basilisk branch starts the declared environment using basilisk's selected shared/fork/socket mode; process isolation depends on basilisk settings and session state.” Record the pinned basilisk version and effective process settings in future environment provenance. If triomics requires process isolation, make that an explicit reviewed policy and test it; do not infer it from the flag. This correction does not require installing or executing anything now.

**Acceptance:** corrected source/metadata wording can close this document finding. A future host test must separately establish selected process mode, interpreter identity, environment creation behavior and no undocumented provisioning. Those remain runtime gates.

**Final targeted recheck:** ENV-Q03 now says the branch calls `basiliskStart(mofa_env)` and that isolation depends on basilisk configuration/version and must be verified. The final next-checks paragraph explicitly adds basilisk version, shared/fork configuration and observed isolation behavior to the NOT RUN list. Both passages were independently reread at final target hash **83996473E6378CA8463094F1F639F902D30D58AC5F13CDB257683E3344E43EF5**. RC-R01 is resolved in documentation; no runtime isolation test has occurred.

## Source-versus-runtime and readiness assessment

The record correctly distinguishes release declaration, development snapshot, PyPI metadata, upstream build evidence, unaccepted reference-environment proposal and actual host validation. It does not claim that a renv lock exists, packages were imported, basilisk resolved successfully, HDF5 round trips work, or a pipeline smoke passed. Its recommendation to verify the released environment first preserves reproducibility without approving a new dependency or a release/development switch.

Outstanding before any compatibility declaration: explicit technology/host and provisioning approval; complete transitive package/OS/platform resolution and licence audit; pinned basilisk/reticulate/R/Bioconductor/Python identities; actual signature/import tests; deliberate provisioning; version-mismatch cases; synthetic train/save/reload; seed/thread/process checks; failure without implicit downloads when provisioning is not authorized. Public upstream assets found inside the archive are not project synthetic fixtures and must not be copied into tests casually.

No P1 factual error was found in the **independently reproduced release-pin/direct-check claims**. RC-R01 was the concrete source-supported correction and is now resolved in the reviewed final wording. The unavailable remote rechecks limit this review's coverage; they do not turn the author's earlier inspections into newly verified facts. ENV-01/02 and API-01 remain open regardless of this document review.
