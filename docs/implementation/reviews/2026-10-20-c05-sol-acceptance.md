# C-05 Sol acceptance

Date: 2026-10-20. Accepted snapshot:
`c34d9fffd4f17d88d54a69dda6410d4bc6ee6365`.

Verdict: **accepted on synthetic software evidence**. This acceptance covers
C-05-AC1 through C-05-AC4. It does not establish performance on a real study,
biological meaning for any factor, enrichment validity, or new-subject
prediction.

## Integrated changes

- `76f8188` corrected native percentage conversion, declared variance
  semantics, covered the specified missingness cells, and added descriptive
  multi-seed sign/permutation/subspace stability evidence.
- `ed69a12` made the enrichment-input boundary validate canonical typed
  `MappingEdge` records against fitted feature identities before hashing.
- `049a0cd` registered `mofa2` as an exploratory-only backend, made runtime
  absence fail closed, explicitly refused prediction, and made the CLI result
  JSON-safe.
- `c34d9ff` updated the central registry inventory test for the newly registered
  method. No scientific behavior changed in that final correction.

Both permitted repairs are consumed. The single focused rereview is consumed.

## Fresh Sol verification

- Focused registered C-05 suite: FAIL 0, WARN 15, SKIP 0, PASS 141. Fourteen
  warnings were the repeatable native MOFA diagnostics described below; one
  run additionally emitted MOFA's highly-correlated-factors QC warning.
- Exact stage-C CLI: exit 0; 12 eligible units; Factor1 retained and Factor2
  removed; variance proportions `0.3074117303` and `0.3663131595`; seeds 1, 2,
  and 3 completed; save/reload comparisons passed. Correlations and subspace
  similarities of 1 are properties of this tiny fixture, not performance
  estimates.
- CLI evidence hashes: `C/integration-results.json`
  `df2b52b5a2ec0f4c45213c9af597a00c09572c35cec850fad715fdbb02c61d33`;
  envelope `ab553a105d77ed3836cfe0b16787848a296fdadbfcb75e90ca4870d3fa95263e`;
  run registry `ce48bed056b99a7f671a25a639d0f0b763a270f28ef29c9b28b7c8c551580cf3`.
- First clean archive check found one stale C-01 assertion that still expected
  five registered methods. After the inventory-only correction in `c34d9ff`,
  C-01 passed 109 expectations with no failure, warning, or skip.
- Decisive clean archive package check at `c34d9ff`: `Status: OK`; FAIL 0,
  WARN 22, SKIP 0, PASS 4076. Archive SHA-256
  `93bdcd444a52a1018d639c1614759b959ca4f3cd0c98fa00848c4835fba4f6db`;
  `00check.log` SHA-256
  `e887ca9f68def83eee939a453b52bcd59bfc8117807fff4e1dca9fdb8b86ca39`;
  `testthat.Rout` SHA-256
  `d577a2f8b92db4312dc4cdfac32a1cc23ea02546c8317195cf1467efb77e9c44`.

## Warning and dependency disposition

The decisive package run is not warning-free. Eight warnings are the already
accepted B-T07 installed-artifact Git-metadata probes. The remaining fourteen
are native MOFA diagnostics caused by deliberately small synthetic fixtures:
low feature/sample counts, factor-versus-average-expression caution,
single-factor correlation QC, and all-low-variance factor retention. They are
preserved rather than suppressed and do not demonstrate an adapter-correctness
failure.

MOFA2 1.22.1 and mofapy2 0.7.5 were verified in the isolated runtime. The main
`renv.lock` remains unchanged intentionally because it cannot represent the
isolated Python backend; `DESCRIPTION` declares the optional R dependencies.
Runtime absence continues to fail closed.

## Acceptance criteria

- **C-05-AC1 accepted:** actual isolated train/save/reload succeeded, while
  incomplete setup cannot report a trained fit.
- **C-05-AC2 accepted:** Gaussian views, native factors/weights/variance,
  declared proportional noncumulative variance, and the specified supported
  missingness cells pass.
- **C-05-AC3 accepted:** independent seeds are aligned by sign/permutation and
  compared by factor and subspace measures; weak/null controls have no forced
  recovery threshold.
- **C-05-AC4 accepted:** prediction is explicitly unsupported, and downstream
  enrichment handoff requires typed canonical mappings plus version-bound
  model/data/mask/annotation hashes.

No private study input was read, fitted, copied, uploaded, or committed.
