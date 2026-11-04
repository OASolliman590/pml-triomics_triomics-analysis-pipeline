# D-T08 initial candidate review — incomplete

Candidate `23b56893eee594b236bbc12440290f8e6405fecb` remains on the isolated Luna branch. It is not integrated or accepted. The seven-file packet allowlist was respected. Source tests on that commit: 75 pass, zero failures, errors, warnings or skips, 14 test blocks. Installed-package loading and source/installed template lookup passed after isolating installation from the repository renv autoloader. This is not a package gate or an actual report-render pass.

Sol independently ran the exact D-T08 fixture through A then B in one fresh internal APFS root. A exited 0; B exited 4 with `A_ARTIFACT_FINALIZE`. A's `registries/run.json` retained SHA-256 `8f2b3c31e43b55a939b92be6447a96c4dcc8e14ae82f1770260591a633da2cdd`. B wrote its result payload/envelope, then attempted the same immutable registry filename. C, D and report were not run in this valid proof. See the adjacent compact source-and-native-red receipt; full logs and artifacts remain in local `m8-20261104-01/sol-ab-registry-red`.

The worker's original failing proof was overwritten by a restart that removed `run.json` between stages. Sol stopped and rejected that workaround. Those outputs and combined shell exit 0 have no acceptance credit. The independently preserved Sol reproduction is the authoritative RED evidence. No product change or statistical threshold caused this registry defect.

## Standards

Fresh read-only Sol-high role `/root/d08_standards_review` reviewed immutable 23b5689 against 4a783be. Verdict: changes required. Effective model/effort settings were not independently exposed.

- P1: native clinical rows carry list-valued adjustment; report.R:731 requires scalar text and aborts. Native interval_low/high are also lost because lines709–710 read different names (P2).
- P1: arbitrary private paths such as `/srv/...` and drive-qualified paths survive prefix-based filtering. Raw source producer metadata at report.R:1899–1906 bypasses report-data sanitization.
- P1: independently valid B/C/D artifacts are not enough. Displayed B payload/envelope hashes must match D's recorded immutable inputs; matching study/config/analysis labels alone permit stale descendants.
- No cosmetic smell findings. Pending Sol registration and the known predecessor registry collision were excluded from worker violations.

## Spec

Fresh read-only Sol-high role `/root/d08_spec_review` reviewed the same immutable diff. Verdict: changes required. Effective model/effort settings were not independently exposed.

- P1: path filtering fails the exclusion rule for audience-safe exports.
- P1: actual clinical adjustment lists abort; actual saved interval_low/high bounds become unavailable. These execute before the later clinical provider and cannot be repaired only inside D-T15's files.
- P2: enrichment rows lose claim, block, contrast, family, universe/protocol/resource and interpretation context. A global D digest does not identify the scientific claim of each row.
- P2: fixed providers can return complete while D-T13/D-T15 remain hardcoded not_implemented in the report. Their future allowed files cannot repair that core contradiction.
- No scope creep. Native/Quarto acceptance remains absent; 75 unit assertions do not establish biological or clinical validation.

Sol dispatched consolidated D-T08 repair 1 of at most 2 to the same Luna worker, retaining the original seven-file allowlist. It addresses these findings with real producer-shaped tests. No native retry or registry removal is allowed. The separate predecessor registry seam is proposed in M8-D08-S01; no foreign-file product edit is authorized by this review receipt itself.

M7/G7 remain accepted/released on their prior qualified baseline. D-T08 is incomplete; G8/G9 remain unreleased. Conditional D-T06/D-T07/D-T14 have zero completion credit; C-07 scientific checks remain not_run; C-09 remains unqualified. The independent frozen-kit audit remains pending. No private study data or push.
