# RDY-005 — proposed C-05 producer for the RDY-004 fixture

**Proposed; not executed or accepted.** This is a separately budgeted,
public-synthetic producer prerequisite for the unchanged
[RDY-004 protocol](RDY-004-mofa-enrichment-qualification-protocol.md), whose
[audit](../reviews/2026-11-09-rdy-004-protocol-astra.md) passed the protocol
only. Neither the existing five-feature C-05 fixture nor the 20-feature C-07
design meets its 25-feature minimum. RDY-004's four enrichment calls remain
unspent, and D's fit budget remains zero. This is **one C-05 fit**, not a D fit.
No biological, calibration, study-performance or real-data claim follows.

## Fixed construction

These constants are engineering fixture choices, not scientific guarantees.
Run the following R construction once, in the stated order. `matrix` fills
columns first; the two residual matrices consume consecutive draws. Keep all
rows and columns in the order shown. Do not regenerate after seeing results.

```r
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(2026110905L)
views <- c("view_1", "view_2")
units <- sprintf("sample_%03d", seq_len(64L))
j <- seq_len(30L)
t <- 0:63
z1 <- sqrt(2) * sin(2 * pi * t / 64)
z2 <- sqrt(2) * cos(4 * pi * t / 64)
a <- ifelse(j <= 18L, 1.6 + 0.02*j, 0.15 + 0.001*j) * (-1)^(j+1L)
b <- ifelse(j <= 18L, 0.12 + 0.001*j, 1.4 + 0.02*j) * (-1)^((j-1L)%/%2L)
blocks <- setNames(lapply(views, function(v) {
  x <- outer(a, z1) + outer(b, z2) +
    0.08 * matrix(rnorm(30L * 64L), nrow=30L, ncol=64L)
  dimnames(x) <- list(sprintf("%s_feature_%02d", v, j), units)
  list(assay=x, feature_keys=rownames(x), sample_ids=units)
}), views)
resolved <- list(analysis_id="rdy005-synthetic",
  study_id="rdy005-synthetic", preparation_context_id="rdy005-producer",
  representation_id="synthetic_complete", unit_ids=units, blocks=blocks)
```

The two prescribed orthogonal sample directions have deliberately different
feature prevalence. They aim to make two synthetic factors distinguishable;
neither retention nor recovery is promised. Require all 60 measured rows and
all 64 columns finite, no missing values, positive per-row sample SD, unique
keys and exactly the ordered maps above. There is no outcome, grouping,
imputation, feature selection, biological entity or study data.

Before fitting create and save a source-row ledger, in view then increasing `j`
order, with `block_id=v`, `feature_key=sprintf("%s_feature_%02d",v,j)`,
`source_id=paste0("rdy005-",v)`, integer `source_row=j`, `original_id` equal
to `feature_key`, `id_namespace="synthetic"`, and
`measured_or_derived="measured"`. Create one mapping record per ledger row:
`block_id`, `feature_key`, `target_namespace="synthetic_namespace"`,
`target_entity_id=paste0("synthetic:",v,":",sprintf("feature_%02d",j))`,
`relation="maps_to"`, `resolution="exact"`, `taxon="synthetic_taxon"`,
`context="rdy005_nonbiological"`, `resource_release="rdy005-fixture-v1"`,
`evidence="generated_fixture"`, and `ambiguity_group=NA_character_`. The
ledger and mappings are original synthetic records, not a biological resource.
Check a bijection on `(block_id, feature_key)` and on
`(target_namespace, target_entity_id)`, and the one-to-one ledger join. Retain
the ledger separately: the C-05 MappingEdge fields do not include source rows.

## Actual C-05 seam and fixed invocation

Use the registered `triomics_integration_backend("mofa2")` adapter's
`validate`, `fit`, and `extract`; do not fabricate a `BackendFit` or sidecar.
Fix `params` to `method="mofa2"`, `block_ids=views`, `n_components=2L`,
`scale_features=TRUE`, `seed=20261109L`, `mofa_seed_repeats=1L`,
`mofa_convergence="medium"`, `mofa_data_options=list(use_float32=TRUE)`,
`mofa_model_options=list()`, and
`mofa_training_options=list(maxiter=1000L, stochastic=FALSE, gpu_mode=FALSE)`.
`model_path` is a fresh, local output filename under this run's synthetic root.
Use one group, Gaussian likelihoods and no outcome. C-05 sets the likelihoods,
factor count, seed and convergence; it forces native `scale_views`,
`scale_groups`, and `center_groups` to `FALSE`. Record all remaining effective
MOFA2 1.22.1 defaults, including any factor-drop settings, from
`triomics_mofa_prepare_options` on a no-fit `MOFA2::create_mofa` object before
the fit and compare them to `fit$result_params` afterward. If the requested
values, defaults or pinned runtime cannot be verified, do not fit.

`validate(resolved, params)` yields `data`, `missingness_mask`, `transforms`,
`block_ids` and sorted `units`. C-05 transforms each row using observed-value
mean and sample SD; here each observed count is 64 and every mask entry is
`FALSE`. Save these **actual returned** named matrices, masks, centers, scales,
counts and original resolved input before the fit. Call `fit` on the identical
in-memory resolved input; its own validation creates the inputs used by
`triomics_mofa_fit_once`. `fit$native_state` retains masks, transforms,
`training_input_hashes`, fitted and reloaded models, output matrices and
`training_state_hash`, but **does not return transformed data**. Recompute
`triomics_mofa_hash_training_inputs` over the saved validation output and require
all three hashes to match `fit$native_state$training_input_hashes`,
`fit$metadata$training_input_hashes`, and the extracted handoff. Hash the saved
RDS files separately as files; use the C-05 `digest(..., algo="sha256",
serialize=TRUE)` semantics for object hashes, including the named list
`list(transformed_data=data, missingness_mask=mask)`. Check the resolved input
hash before and after the fit. Any mismatch means unavailable, not reconstructed.

Call `extract(fit, context=list(annotation_mappings=edges), params=params)`
and retain its actual `sidecars$enrichment_inputs`, a
`MofaEnrichmentInputSidecar` schema `2.0`. Its canonical MappingEdge validator
requires all eleven fields, unique edge identities and fitted feature membership,
and radix-sorts the records; it does **not** enforce this protocol's bijection.
Independently enforce the bijection above, then compare the canonical mapping
object hash and model byte hash with the sidecar's `hashes`, and bind the
source-row ledger hash in the producer manifest. Missing data, a changed model,
an invalid map or a hash disagreement is a hard stop. D may neither mint the
sidecar nor infer missing ancestry. The standalone caller can retain these
artifacts without a core patch **if** these bindings and native readback pass.
If capture/readback cannot establish the actual trained inputs, stop and ask Sol
for a separately audited change to the C-05 `fit_once`/`fit` handoff to expose
and persist the exact eligibility data and masks used at native entry. Do not
substitute a recomputation or change shared code in this packet.

## Reservation, supervision and execution gate

First freeze/audit this protocol and a bounded runner scope; then pass the
runner's no-fit preflight before authorizing the one run. Pin the previously
observed R 4.6.1, MOFA2 1.22.1, Python 3.12.14 and mofapy2 0.7.5. Verify the
actual Rscript SHA-256 is
`7004a18d97f2470ffaf51ed4fdfef1785f641f54e1e5c7e30d0f894b6c475d91` and
Python executable SHA-256 is
`f681deb489625d7f104c4549f7d15f9fdedf7ef17d2c6d7fbaeadd709ca9f739`.
Before invocation, capture the identities and hashes of other executables,
native code, packages/dependencies, installed MOFA2 source/tree and mofapy2's
`run/entry_point.py`, `build_model/utils.py` and `build_model/save_model.py`,
plus platform, BLAS and configuration. A measured identity is not a prior
verified hash; do not imply otherwise. If a required identity cannot be
established, stop. No installation, network or runtime substitution. Use one
heavy qualification job at a time, one native fit at a time; set `OMP_NUM_THREADS`,
`OPENBLAS_NUM_THREADS`, `MKL_NUM_THREADS`, `VECLIB_MAXIMUM_THREADS`,
`BLIS_NUM_THREADS` and `NUMEXPR_NUM_THREADS` to `1` before R/Python start and
verify effective thread-pool limits where applicable. If they cannot be verified,
mark unavailable. Record effective settings and hardware locally.

The global wall-clock cap is **600 seconds** from supervised runner start,
including preflight and finalization; the fit subprocess cap is **180 seconds**
or the remaining global time, whichever is shorter. Atomically and durably
reserve the sole fit attempt before launching its run-owned subprocess; a crash,
timeout or unknown entry count consumes it and never triggers an automatic
relaunch. Record reserved, entered when known, completed, failed, unavailable
and unrun states separately; do not silently refund. On timeout terminate and
reap only the exact run-owned process and its verified descendants; preserve
partial files as nonfinal. Use process-group signalling only if the group's
identity and exclusivity to this run have been established; do not assume
`processx` creates an exclusive OS group. Before fitting, a no-fit supervisor
preflight must prove deadline enforcement, descendant cleanup, durable
reservation across a simulated
controller crash, and no restart after an ambiguous reservation using harmless
dummy processes. Reuse a supervisor only if its actual bytes and this runner's
preflight establish these properties; do not assume the C-07 runner qualifies.
If any preflight, runtime, capacity or time check fails, do not fit.

Attempt `fit` exactly once with `mofa_seed_repeats=1`; no hidden repeated seeds,
retry, repair fit, reroll, factor adjustment or replacement model. A failed,
timed-out, nonconvergent, pruned or otherwise ineligible model remains failed
or unavailable with evidence. Report actual convergence if verifiable; otherwise
unknown. Keep the native failure and any partial bytes; never call enrichment.
Legacy source/package suites that fit models and later integration verification
are outside this one-fit allowance. Their native calls need an explicitly
reviewed verification budget before execution, including identification of any
already budgeted RDY-004 calls; the four-call enrichment limit and zero-D-fit
rule stay unchanged. No hidden gate fits are authorized.

## Authenticity and eligibility after the fit

Require the adapter's saved/reloaded factor, weight and variance agreement.
The installed `run_mofa` itself returns `load_model(outfile)` with default factor
sorting; C-05 then loads the same file with sorting disabled. Independently load
the same model bytes using
`MOFA2::load_model(path, sort_factors=FALSE, remove_inactive_factors=TRUE)`.
Hash before and after reload. Installed MOFA2 1.22.1's matrix constructor
splits into one `group1`; `prepare_mofa` only converts sparse matrices here.
Its reticulate bridge transposes each feature-by-sample matrix before mofapy2
0.7.5 `set_data_matrix` casts to float32 and stores feature means as intercepts.
With C-05's three flags `FALSE`, `process_data` applies no group centering or
view/group scaling; it also maps the sentinel `-2147483648` to missing, which
this finite fixture must not contain. `saveData` writes processed data and
intercepts separately with the model's missing mask. These are source findings,
not proof about an unexecuted fit.

Read the reloaded native values using `MOFA2::get_data(reloaded, views=views,
groups="all", features="all", as.data.frame=FALSE, add_intercept=FALSE,
denoise=FALSE, na.rm=FALSE)`: this returns view/group matrices in R's
feature-by-sample orientation. Require the actual data-loaded flag, one `group1`,
the exact ordered views, feature and sample keys, dimensions and masks. Compare
each cell to the saved double-precision C-05 input using absolute tolerance
`1e-7 + 1e-7*abs(x)` for `x` the C-05 value, and record maximum discrepancies.
This allows float32 rounding; the two representations are not asserted to have
identical bytes or hashes. Preserve the actual returned native matrices, masks
and their own object/file hashes separately from C-05's three input hashes.

The 1.22.1 `run_enrichment` source calls `get_data` with its default
`add_intercept=TRUE`. Also save that default-accessor result and the loaded
per-view intercept vectors; require finite keyed intercepts and check that
adding each intercept to its raw feature row reproduces the default result
within the same tolerance, with unchanged keys and masks. Record both views'
hashes: neither replaces the producer input fingerprint. Unknown, absent or
mismatched native data/intercepts block the fixture.

C-05 aligns reload factors internally but does not expose the permutation and
signs. Independently record the fitted and raw-reloaded outputs and reproduce
its greedy alignment in fitted-factor order: choose the remaining reloaded
factor with greatest absolute score correlation; sign is negative only when the
score dot product is negative. Refuse zero-variance scores or a best-match tie
within `1e-6`. Apply each same permutation/sign to its weights and permutation
to variance; require C-05's agreement comparisons to pass and record the raw
and aligned IDs, signs and values. Do not edit model weights or relabel the
saved model. If this cannot be demonstrated, stop at the same producer seam.

Require exactly two retained uniquely named, finite, nonconstant factors and
rank two across the 64 keyed samples. For the two assignments between `z1,z2`
and the fitted scores, choose the larger sum of absolute correlations; require
its advantage over the other assignment to exceed `1e-6` and both assigned
absolute correlations to be at least `0.7`. Record assignment and signs; this
is an engineering identity check on this fixture, not a recovery guarantee.
Require at least 25 finite, nonconstant `view_1` training features, exactly
matching the producer map. Failure, including factor pruning, is unavailable;
do not select a different view or model. RDY-004 later selects its own first
24 keys, two IDs and annotation family exactly as already specified.

Persist locally a checksummed immutable manifest binding protocol and producer
commit, complete resolved input/config and effective options, generator seed,
runtime/executable/dependency identities, ordered maps and source-row ledger,
validated transformed data and masks, three producer input hashes, model bytes,
training-state hash, native readback and alignment evidence, canonical typed
sidecar and mapping hash, fit reservation/timing, and failure or success status.
Capture exact invocation, exit status or unknown, all warnings with their stage
and context, and actual native-entry counts or an explicit unknown for each
stage. Preserve failures and warnings without suppressing or relabeling them.
Publish no private paths, data or credentials. A qualifying producer receipt is
only permission to request the separate RDY-004 input/null/rights review; it is
not itself D-T06 acceptance or an enrichment call.

## Future implementation scope

Propose only `inst/qualification/run-c05-enrichment-producer.R` and its no-fit
behavioral test `tests/testthat/test-RDY-005-c05-enrichment-producer.R` as the
future worker allowlist. The installed runner must exercise the actual adapter,
capture its outputs and support the supervised single attempt. Its tests must
verify the deterministic generator, complete masks, map bijection, exact option
and hash bindings, refusal of changed/missing inputs, pruning and ambiguous
alignment, reservation/timeout/crash behavior, and zero native fit/enrichment
entries in no-fit tests. Test guards with plain data, not fake native models or
sidecars. Test actual installed runner bytes as well as source;
do not duplicate the runner, skip tests or alter packaging to conceal absence.
Sol owns any needed shared seam or registration. This proposed scope requires
its own review and runner preflight; this document authorizes no implementation,
model fit, enrichment call, change to RDY-002/RDY-004, push or publication.
