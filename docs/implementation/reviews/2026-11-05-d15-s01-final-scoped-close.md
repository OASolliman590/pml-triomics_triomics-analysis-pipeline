# M8-D15-S01/S02 scoped close — 5 November 2026

Sol accepts the bounded producer predecessor work and integrates immutable product `ee434483bf852724c1dbb38d0fa8b710ea7c59cc` after independent verification and the fresh [Astra pass](2026-11-05-d15-s01-final-astra.md). This is not D-T15 or M8 acceptance. G8/G9 remain closed; no renderer or later packet started.

The previously interrupted source gate was rerun on the unchanged clean candidate after the SSD returned. Luna and completed local Sol reviews were not rerun. Final Luna repair2 `1464668666e63b5226b092dc11567fd1ba6196eb` changed exactly three expected refusal-code strings; product bytes remained those of repair1 `7876735ca2206897d45f3bdb93ce5b3ead5c9124`. S01 and S02 each retain one initial attempt plus two repairs; no repair3. S02 `a136db4` remains preserved in ancestry.

## Independent verification

Command: `Rscript --vanilla <local-proof-root>/scripts/run-source-unbounded-report.R <immutable-source-root> <fresh-proof-root>/source 'B-T07|B-T08|C-06|C-08-shared|D-T01|clinical-native-handoff'`. Exit0; **1,909 pass,0fail,0error,1warn,0skip,113blocks**. Execution and report caps were independently verified as unlimited with a separate 12-intentional-failure fixture and trailing success. No product test or numerical expectation was changed.

| Suite | Pass | Warn |
|---|---:|---:|
| test-B-T07.R | 505 | 0 |
| test-B-T08.R | 328 | 0 |
| test-C-06-shared.R | 362 | 0 |
| test-C-06.R | 87 | 0 |
| test-C-08-shared.R | 96 | 0 |
| test-clinical-native-handoff.R | 125 | 0 |
| test-D-T01.R | 406 | 1 |

Only warning: **D-T01 refuses stale paired signs and preserves the old pointer**, `test-D-T01.R`. Its deliberate stale-sign native D command exits2; the test proves refusal and preserves the old pointer. No warning suppression or new warning name.

Fresh native CLI command shape: `Rscript --vanilla inst/exec/triomics --config <fixed-public-synthetic-config> --stage <stage> --output <same-output-root>`. Actual argument vectors, exits and log hashes are in [the receipt](../evidence/2026-11-05-d15-s01-final/receipt.json). Fresh C08 endpoint-only0; A0/C-schedule0/missing-B refusal2/B0/existing-B refusal2/C-publish0/D0. Separate clinical-disabled A/B/C/D all0. No registry removal, fabricated predecessor or cross-root stitching in the positive chain.

Both stored profiles validate. C binds all four B predecessor bytes; D manifest/member hashes match. Original C/request bytes are unchanged and later B artifacts are not ancestors of the earlier C fit. The unchanged B08 service matches all nine compared status/method/objective/family/result fields at serialization tolerance1e-12 on twelve public synthetic subjects. Initial observed C fits once; saved C succeeds with all nine monitored fit/preparation counters0. The after-envelope-write failure is reached, cleans partial C provenance and preserves the previous D generation/pointer and complete prior inventory.

Sixteen independent adversarial cases pass: metadata, clinical link, clinical observation, measurement, external registry symlink, rehashed endpoint, reordered endpoint, rehashed objective request, reordered request, missing endpoint envelope, and rehashed saved handoff, identity, controls, scores, nominations and comparison. Each baseline validates; each altered case exits2 before fitting, leaves the artifact inventory unchanged and writes no C provenance pair. All six saved-context substitutions also fail direct decoding. Measurement uses copied public inputs configured before a separate native A/C baseline; the immutable checkout and positive chain stay untouched.

## Full artifact hashes

Paths are relative to the fresh positive output root.

| Artifact | SHA-256 |
|---|---|
| `B/handoffs/5ca753668d1a175f3eff186f309dead7fd0f5387b05f195ed1f93b0157965893/bundle.json` | `20c8b643c9f39f8da706376d39568829ad9d73fce2f5e21f2774c627a97b733e` |
| `B/handoffs/5ca753668d1a175f3eff186f309dead7fd0f5387b05f195ed1f93b0157965893/bundle.envelope.json` | `b2059c78212da9f0635ddd7f65d760d726e99a5f92c79350c84191f0a88d7f45` |
| `B/handoffs/5ca753668d1a175f3eff186f309dead7fd0f5387b05f195ed1f93b0157965893/stage-handoff-provenance.json` | `247ce312848ab496bdc5c30105282ff6abc0118aa7c4680fb3ba4f0cd3895ae8` |
| `B/handoffs/5ca753668d1a175f3eff186f309dead7fd0f5387b05f195ed1f93b0157965893/stage-handoff-provenance.envelope.json` | `4fb76a97ae0eb79cd11f92c6b47fe5aa71b32303a183b993f78567302e86669b` |
| `D/stage-handoff-provenance.json` | `eeb8f7d56120d44d46845d61142f777cd45c3a75860dc1af6d274083ce565a74` |
| `D/stage-handoff-provenance.envelope.json` | `799f750ea8102ecebe740f6ee129f6661e945625143b1017aaf3c1e20534499d` |
| D generation manifest | `fb4c90175f017a74cfca4686a5fc644e154a26ba4ec7e02388efbd6d83dc467d` |
| Clinical-disabled D generation manifest | `91f5ad3e64101fb0f62af066f3b3bba24d8d3c2e421007db90e92ec0a35eff0c` |

## Evidence limitations and preserved failures

The disconnected run remains failed diagnostic evidence (1599pass/9fail/1error/15warn/0skip), not acceptance. The summary reporter's ten-failure display limit is distinct from its execution limit; the resumed external runner sets both to Inf. Original logs are preserved.

The first supplemental wrapper was correctly refused because global sourcing under an external launcher lacks authenticated producer identity. Normal pkgload namespace loading resolved the actual clean revision without a mocked guard; the corrected observer/rollback probes passed. A later measurement probe selected the first declared source (metadata) and stopped before mutation or pipeline execution; role-based selection and a separately configured copied-input fixture corrected that harness. Prior successful source/native/numeric checks were retained, not repeated. Original failing receipts remain intact. The numeric display receipt omits its script name and has an unused null n_rows projection; its actual executed command and whole-result comparisons are retained in the wrapper receipt. No product edit resulted from these corrections.

The canonical branch fast-forward changed only seven reviewed source/test files. Existing dirty `.Rbuildignore`, `.gitignore`, `renv/activate.R` hashes and all previous untracked entries remain unchanged. Two new filesystem-created AppleDouble sidecars remain untracked; no sidecar was staged. Records are a documentation-only follow-up to the audited product. No clean archive package gate was required or run for this bounded predecessor cycle; no `Status: OK` is claimed.

M7/G7 and D08 remain accepted. D13 remains blocked at its exhausted cap; D15 renderer remains unimplemented/unaccepted. C07 four checks remain not_run; C09 remains unqualified/synthetic_fixture_only/benefit_guaranteed=false. D06/D07/D14 retain zero credit; frozen-kit independent audit remains pending. Synthetic software evidence does not establish biological or clinical validity. No private study data, server execution, GitHub push or Notion sync.

Literal resume: **Resume the D-T15 renderer under its existing packet scope using verified predecessor ee43448. Preserve its recorded preflight attempt and repair history, all earlier candidates and D13's exhausted cap. Obtain required local review and saved-output/native report evidence, then stop at that bounded receipt. Do not reopen S01/S02 repair3 or release G8/G9.**
