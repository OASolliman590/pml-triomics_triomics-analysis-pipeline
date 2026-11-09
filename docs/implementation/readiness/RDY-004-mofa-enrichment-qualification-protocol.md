# RDY-004 — protocol for qualifying native MOFA enrichment

**State: proposed; not executed, approved, or an amendment.** This protocol
precedes activation of [D-T06](../../../discovery/spec-v2/packets/D-T06.md).
It qualifies only a narrow synthetic software lane, subject to independent audit.
It neither certifies a biological database or study nor makes D-T06 complete.
The [D contract](../../../discovery/spec-v2/D_ANNOTATION_TARGET_REPORT.md) and
[data contracts](../../../discovery/spec-v2/DATA_CONTRACTS.md) remain authoritative.

## Release observations and proposed scope

Bioconductor identifies MOFA2 1.22.1 as its 3.23 release [MOFA2 release (2026)]; its manual documents a
competitive test, a binary set-by-feature matrix and factor-wise adjustment
[MOFA2 manual (2026), `run_enrichment`]. The earlier
[runtime readback](2026-11-08-native-runtime-readback.md) observed R 4.6.1,
MOFA2 1.22.1 and a C-05 producer runtime with Python 3.12.14/mofapy2 0.7.5. Pure
enrichment of a restored saved model must not launch Python or fit a model.

Local inspection of the installed 1.22.1 `run_enrichment`, `.pcgse`,
`.compute_feature_statistics`, `.pcgse_ttest`, `.createVarGroupList` and
`get_weights` establishes the following **implementation observations**, not
executed enrichment results. For a view, the function divides the selected
factor-weight matrix by its single maximum absolute entry **before** filtering;
its feature statistic is the absolute scaled weight. It removes zero-variance
features, intersects the remaining model features with matrix columns, and drops
sets below `min.size`. The default is `mean.diff`, `parametric`, `sign="all"`,
`min.size=10`, `nperm=1000`, `p.adj.method="BH"`, `alpha=0.1`. The default
test uses a pooled, two-sided t statistic and BH separately for each factor over
the retained sets. A present, all-zero annotation column is in the complement;
a model feature absent from the matrix is outside the denominator. These details
must be verified against native outputs, not inferred from the older
[research note](2026-11-08-optional-methods-evidence.md).

Only the defaults above, one view, at least two factors and two sets, complete
nonconstant data and finite estimable statistics are proposed. Tiny, full or
otherwise non-estimable sets are reported as unavailable with their declared
family counts; do not silently shrink a claim's family after observing weights.
The adapter must reject the entire requested family before native execution if
any member is non-estimable. One-set/one-factor cases, directional signs,
`rank.sum`, correlation adjustment and permutation remain unqualified. In the
observed release, directional modes zero opposite-sign weights and overwrite
negative statistics and their p/q values **after** adjustment. Permutation uses
strict `>` on absolute statistics and divides by `nperm` without a plus-one
correction; neither behavior is endorsed or qualified here.

The mathematical null compares mean absolute weights inside a set with those in
its complement, using a pooled equal-variance t reference. Independent,
approximately normal feature statistics and common variance are assumptions of
that reference, not established by a fitted model. Overlap, correlated features,
learned weights and multiple factors may affect calibration; factor-wise BH is
not correction over factors, views or signs. Numeric agreement proves calculation
for this fixture, not valid type-I error, FDR control or biological enrichment.
Label results exploratory and never pathway activation, causal effect or binding.
The lane, complete-case restriction and fixture rules are proposed choices for
audit; any broader inferential claim or material scientific-contract change needs
the frozen change-record and affected-acceptance process. PCGSE describes the
underlying class of competitive tests [PCGSE (2015)]; it does not validate this
release's modified implementation or this fixture. Before qualification, the
review must explicitly assess the null assumptions and intended exploratory
claim; if the evidence is insufficient, the scientific gate remains blocked.
Study-level calibration needs separately approved, study-relevant evidence and
cannot be supplied by this zero-fit numerical check.

## Bind before execution

Record and verify, locally, all identities before inspecting enrichment outputs:

1. **Runtime/source.** Pin R 4.6.1 and its Rscript SHA-256
   `7004a18d97f2470ffaf51ed4fdfef1785f641f54e1e5c7e30d0f894b6c475d91`;
   MOFA2 1.22.1; the C-05 producer's Python/mofapy2 versions and executable
   hash; package/dependency and platform versions. The previously observed
   producer Python executable SHA-256 is
   `f681deb489625d7f104c4549f7d15f9fdedf7ef17d2c6d7fbaeadd709ca9f739`;
   verify the actual producer rather than assuming it used those bytes. The
   existing deparse of
   `run_enrichment` and `.pcgse` hashes to
   `bfece27b53e05e38fd5d8deda4b37338fd80007badcb8f1c2a82788a981371c9`.
   A separate helper deparse, in the order named above after `.pcgse`, hashes to
   `23b330ea494068ab0c636538fb4a279fa78e7eb3a6476ab1440db73018b0b1d5`:
   for each helper concatenate `### `, its name, newline, R's default `deparse`
   joined by newlines, and a final newline. The observed installed MOFA2 tree
   has 41 files, no symlinks, and manifest SHA-256
   `ad7d0d6c0c6beba6b362e798e59317d6c7b88f294378775abc20f5926593c3b9`;
   manifest rows are UTF-8 `relative/path<TAB>byte-size<TAB>sha256<LF>`, sorted
   by path bytes. This is an installed-tree identity, not a release-tarball hash.
   Capture the exact release archive/binary provenance and checksum if available;
   a mismatch or unidentified dependency blocks execution.
2. **Producer.** Freeze one public-synthetic, C-05-produced native saved model,
   producer commit/config, versioned typed handoff, model file SHA-256, training
   state hash, view/factor/feature/unit identities and ordering, save/reload
   agreement, transforms and versions. Require the actual transformed training
   data and missingness-mask artifacts, not merely their digests; recompute C-05's
   R-serialized SHA-256 for data, masks and their named combined list. Restore the
   native model using `load_model(..., sort_factors=FALSE,
   remove_inactive_factors=TRUE)` and compare native data, dimensions,
   keys, values and masks to the authenticated producer inputs; resolve recorded
   factor permutation/sign consistently for factors and weights or refuse.
   Cross-check the sidecar's canonical MappingEdge hash and producer model hash.
   Hashes or extracted loadings alone are insufficient. If data or masks are not
   recoverable, stop at the C-05 producer/consumer seam and request a scoped
   producer handoff repair; D cannot reconstruct them or refit.

   **Current blocker:** no eligible native model with at least 25 features in a
   view has been identified for this protocol. The existing
   [C-05 runtime fixture](../../../tests/testthat/test-C-05.R) has five features
   per view; the proposed [RDY-002 C-07 fixture](RDY-002-c07-qualification-protocol.md)
   has 20 per view. Neither meets this protocol. Do not relax `min.size`, borrow
   or reconfigure C-07 models. Acquiring a suitable immutable C-05-produced
   model and its data, masks and mappings is a separately budgeted and reviewed
   producer prerequisite. Zero D fits authorizes no new upstream fit.
3. **Annotation and rights.** Before inspecting weights, require at least 25
   eligible finite, nonconstant model features and two retained factors. Select
   the lexicographically first 24 unique feature keys and first two factor IDs.
   Create original, nonbiological synthetic set A from keys 1–10 and B from
   keys 6–15; matrix columns include all 24 keys plus one zero-membership
   synthetic key absent from the model. Keys 16–24 have zero membership in both sets; at least the
   25th model key is omitted. Freeze matrix bytes/hash, ordered names, membership,
   eligible universe, producer mapping and family before outputs. Require a
   unique one-to-one synthetic MappingEdge for each included measured feature;
   retain source-row identities and refuse ambiguous, duplicate or many-to-one
   mappings rather than multiply observations. This is a restricted fixture rule,
   not a general biological mapping policy. The canonical mapping must be bound
   by the C-05 handoff; D may not mint a replacement producer sidecar.
4. The original, nonbiological fixture incorporates no borrowed biological set
   collection. Follow the repository's existing synthetic-resource GPL-3.0 terms,
   as recorded in the [D-T04 fixture](../../../inst/examples/tickets/D-T04.yaml),
   with no new license dedication. A typed ResourceManifest must record canonical
   source, exact snapshot/hash, retrieval time, terms evidence, verified
   rights for the explicit local MOFA-enrichment verification operation, and
   offline cache/network policy. Record redistribution separately as allowed,
   prohibited or unverified, as supported by the
   [resource access contract](../../../R/resource_manifest.R); do not infer it
   from local-use permission. Real biological databases need their own verified
   release-specific rights and remain unavailable until verified. Missing rights
   for this operation block the lane. No private data, paths or identifiers enter
   the public receipt.

## Prespecified oracle, budget and boundaries

Set `RNGkind("Mersenne-Twister", "Inversion", "Rejection")` and seed
`20261109` before each call; construction and the parametric lane are
deterministic, and `nperm=1000` is recorded but unused. The exact call supplies
the reloaded object, frozen view, matrix and two factor IDs with
`set.statistic="mean.diff"`, `statistical.test="parametric"`, `sign="all"`,
`min.size=10`, `nperm=1000`, `p.adj.method="BH"`, `alpha=0.1` and
`verbose=FALSE` (the native verbosity default is `TRUE`). D's fitting budget is
**zero**. Across qualification and packet acceptance, the hard budget is **four
attempted native `run_enrichment` entries total**: (1) direct readback of the
frozen fixture, (2) repeat after independent reload for reproducibility,
(3) one successful packet test-suite path and (4) one synthetic CLI path after
the required shared integration. Failures count; no unplanned retries or extra
native boundary calls. Any extension requires a documented budget amendment and
audit before further calls. Negative tests instrument the entry point and require
zero native calls. Run the packet's named suite and CLI exactly as specified;
record discovered tests, exit codes and skips. A skip is not a pass.

First audit this protocol, then execute calls 1–2 and review the resulting
release/model/input/null/resource evidence independently. Only a passed
qualification gate, the D-T04/D-T05/C-05 dependencies and G6 permit D-T06
activation. Calls 3–4 are subsequent packet/integration acceptance evidence;
they cannot retroactively authorize an ungated implementation.

Construct the independent oracle from the restored model's **actual** keyed
learned weights; do not fabricate a MOFA object, replace weights or fit to the
answer. Compute the view-wide scale from all model features and the two selected
factors, then intersect with the 24-key universe and take absolute values. For
each set/factor let `a` be its 10 values and `b` its 14 complement values:

`sp2 = (9*var(a) + 13*var(b))/22`,
`t = (mean(a)-mean(b))/sqrt(sp2*(1/10+1/14))`,
`p = 2*pt(-abs(t), df=22)`.

Require finite positive `sp2`; otherwise declare the family unavailable. Compute
BH independently by sorting the two p values per factor, applying the reverse
cumulative minimum of `2*p_(i)/i`, capped at one, and restoring set order. Check
native filtered memberships, feature statistics, t, p, q, and alpha-threshold
lists by keys for both factors; use absolute tolerance `1e-10` plus relative
tolerance `1e-8` times the expected magnitude for finite numeric comparisons,
and exact keys/counts/statuses. There are four set/factor tests, but each
factor's declared BH family has exactly the two retained sets.
Save inputs, raw outputs, independent calculations and hashes locally before
evaluation. No arbitrary expected values or results-driven fixture substitutions.

Behavioral tests must show all of the following, without inventing native models:

- Missing, empty, changed or incompatible model/data/mask/map/annotation/rights
  sidecars, versions and hashes fail visibly before native entry; required-route
  failure is incomplete, not biological absence. Verify cache invalidation.
- Baseline readback proves absent/extra columns are excluded, present zero-member
  columns are in the complement, overlapping A/B remain distinct and BH is
  factor-wise; original and retained family counts are explicit.
- Preflight tests use public-synthetic arrays and typed inputs to reject missing,
  insufficiently observed or zero-variance data; small sets, empty/full or
  single-feature complements, zero pooled variance, duplicate keys or mappings,
  and unsupported one-set/one-factor requests. No imputation or silent drop.
  Where no matching C-05 artifact exists, record that end-to-end boundary as
  unproven; a unit guard cannot substitute for a native readback.
- Check the algebraic identity `abs(w) == abs(-w)` on the extracted weights
  without modifying the model; no sign-flipped native execution is claimed.
  Directional requests fail as unqualified and no positive/negative biological
  interpretation is emitted without an independently declared orientation.

## Acceptance and integration gates

The independent reviewer evaluates model/data/mask/map identity and rights
(AC1), direct native readback against the numeric oracle and stated family/null
(AC2), visible fail-closed negative cases and zero D fits (AC3), and the executed
suite, CLI, rights record and limitations (AC4). Save the acceptance evidence
under the packet's local output root; a standalone arithmetic check satisfies none
of these gates by itself. The current annotation application and status handler
deliberately refuse completion of `mofa_native`; packet-owned registration must
request any required CLI/status/namespace changes from Sol. Shared integration,
the synthetic CLI slice, and milestone review must pass before consumers use the
route. Any missing artifact, scientific-null approval, rights evidence or required
shared change leaves the route unavailable and D-T06 unaccepted.

## Sources

[MOFA2 manual, 2026] Argelaguet et al. *MOFA2 1.22.1 reference manual*,
Bioconductor 3.23, [`run_enrichment`](https://bioconductor.org/packages/3.23/bioc/manuals/MOFA2/man/MOFA2.pdf).

[MOFA2 release, 2026] Bioconductor. [*MOFA2 1.22.1*](https://bioconductor.org/packages/3.23/bioc/html/MOFA2.html).

[PCGSE, 2015] Frost et al. [*Principal component gene set enrichment*](https://pmc.ncbi.nlm.nih.gov/articles/PMC4543476/).
