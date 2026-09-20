# D — annotation, target investigation and safe reporting

## 1. Evidence in, never upstream refitting

`annotate_evidence(evidence_bundle, annotation_spec)` consumes full tested/failed
B evidence, registered families, B/C nominations, integration diagnostics,
clinical results, source/feature registries and optional imported evidence.
`render_report(report_bundle, report_spec)` reads immutable versioned exports.
D cannot rerun preprocessing, invent a model, correct p values from selected
exports or reach into native backend internals through an untyped shortcut.

Route on namespace, organism, molecular resolution and explicit capabilities,
not block display name or nomination source. Both B and C feed compatible gene/
reaction and lipid routes. Preserve source row identities even where multiple
features map to the same molecular entity. Report mapping ambiguity, species
inference and unresolved isomers rather than upgrading identification specificity.
GO is gene-product functional annotation, not itself a reaction network.

## 2. Resource and mapping contract

A ResourceManifest declares canonical source, exact release or content snapshot,
checksum, retrieval timestamp, rights/terms evidence, permitted operation,
redistribution status, local cache and network policy. No license is inferred
from an R package's license or from public website access. No raw study uploads.
KEGG is opt-in and entitlement-gated. Other resources also require release-specific
rights and operation checks; a local sanitized fixture can test adapter semantics
without certifying current biological coverage.

Initial local mapping tables have `source_namespace`, `source_id`, `taxon_id`,
`target_namespace`, `target_id`, `relation`, `resolution`, `evidence_ref` and
`resource_id`. One-to-many mappings produce edges and retained ambiguity, not
copied independent observations. Lipid shorthand preserves class and supported
sum composition; unresolved chains/positions/isomers remain unresolved. Approved
aliases can be route inputs with provenance but do not turn all numeric CIDs
into a PubChem retrieval permission.

## 3. Enrichment

D-T04's first fully specified local method is one-sided over-representation by
hypergeometric/Fisher-equivalent tail over a declared measured/eligible mapped
universe. For each set: population N, set members K, nominated n and overlap k
are explicit unique analysis entities. Use P[X >= k], not P[X > k]. Freeze the
universe, mapping/aggregation rule and fixed set-testing family before computing
BH within the declared annotation claim. Never use the selected list as its own
background or every database entry as the automatically measured universe.

Ambiguous mappings require the declared resolution rule; no multi-map multiplication
of successes. A justified targeted panel may define a conditional universe; unknown
or outcome-selected inventory cannot support ordinary enrichment claims without
a qualified method. Preserve unavailable/non-estimable sets and full family counts.
ORA overlap tests also have model/dependence limitations and are not pathway
activation, patient-level scores or causal target inference.

Rank/model-based routes retain their own contracts rather than pretending ORA is
interchangeable. Native MOFA enrichment (D-T06) requires version-pinned model/data/
annotation sidecars and verified `run_enrichment` arguments, null/sign semantics
and reference checks. BioPAN (D-T07) requires lawful local execution, quantitative
lipid input and a compatible comparison; do not feed CLR/delta/loadings and call
it adjusted paired inference. These remain conditional capabilities, not cancelled
or marked complete because unavailable statuses are rendered.

## 4. General target investigation

Enabled target modes: `assess`, `compare`, `discover`. Declare target namespace,
ID, organism and entity type; a pathway/process is not a molecular binding target.
A drug is optional. Discovery requires a bounded versioned target universe and
an unranked enumeration rule. Targets selected with these observations remain
exploratory unless a valid separate confirmation protocol is supplied.

Every target dossier keeps four evidence axes separately:
1. Metabolic/multi-omics compatibility, including support, contradiction, ambiguity
   and unmeasured discriminating features.
2. Functional perturbation evidence, with activity/endpoint/context and controls.
3. Cellular/tissue target-engagement evidence, with the actual assay limitation.
4. Direct binding evidence, with exact compound, target construct, assay, measured
   endpoint/relation/units, source and provenance; external versus this-study status.

Binding, engagement, function and association are not interchangeable endpoints.
No conversion of docking energies, abundance changes or IC50 into an experimental
Kd; no arbitrary composite support score or percentage binding probability.
Protein abundance is not necessarily activity; metabolite concentration is not
flux. Substrate/product direction depends on context and kinetics, so no universal
substrate-up/product-down rule. A non-enzymatic target needs documented regulatory
or perturbational connections; no invented enzyme reactions.

Evidence chains retain intermediate nodes, compartments, direction/unknown sign,
species/tissue/time, original experiment/publication and dependent feature groups.
Duplicate adducts, shared pathways, copied databases and multiple analyses of the
same subjects do not add independent replication. Compare alternative targets
only on explicit comparable evidence axes and coverage; otherwise show noncomparable
profiles or unresolved ties. Missing coverage is not evidence of no effect/binding.

D-T10 builds target requests/identity/ledger; D-T11 produces descriptive named and
alternative-target profiles; D-T12 enumerates unranked exploratory targets; D-T13
renders the dossier. D-T14 requires a separately approved quantitative method,
null/comparison protocol, leakage controls and validation dataset. Its current
state is conditional-unapproved; no estimator is invented to force closure.

## 5. FCN3 / dapagliflozin use case

For this requested application, ask: are the observed molecular and clinical
patterns compatible with the proposed FCN3-related mechanism, and are competing
physiological/known-pharmacology explanations distinguishable? Do not ask the
metabolomic matrix to establish a physical drug-protein interaction.

The 2026 primary paper indexed as PMID 41587770 (DOI 10.1111/bph.70331) reports
FCN3-related drug interactions investigated by SPR. That source belongs on the
external binding axis. This drafting task did not inspect compound-specific raw
sensorgrams, fit models or numerical affinity results; do not populate a
compound-specific Kd from its abstract. A user-supplied assay report can enter as
an explicitly sourced local evidence record after identity/assay review.

The pipeline should preserve any available molecular FCN3 measurement, eligible
complement-related evidence, metabolite/lipid changes and clinical associations
without inventing missing endpoints or a unique metabolic FCN3 signature. Each
connection requires sourced context and coverage. A compatible downstream pattern
is supportive mechanism evidence, not a remeasurement of SPR or proof that FCN3
mediates the treatment association. Dose/time/causal perturbation studies or
orthogonal engagement/function assays are follow-up evidence classes, not results
produced by this software. No extra docking/MD subsystem is added here.

## 6. Clinical report and privacy

Show dictionaries/coverage, registered objectives, effects/intervals/units,
adjustments, population and per-cell counts, missingness, exploratory versus
independent evidence and score lineage. A contrast enrichment is not a patient
pathway score. Clinical-to-target links preserve shared subjects and construction
lineage; they cannot be called independent confirmation by renaming endpoints.

The report is self-contained local HTML plus machine-readable TSV/JSON exports.
Use a Quarto template with embedded assets and no runtime CDN dependencies.
Escape untrusted identifiers/labels, refuse arbitrary executable HTML/JS inputs,
and prevent path traversal or unsafe file links. Direct identifiers, private
paths, raw clinical free text, credentials and source cells are excluded from
public/audience-safe exports. Clinical aggregate release requires an institution-
approved minimum cell-size/complementary-suppression policy; do not invent one.
Private local research reports remain local and clearly labelled.

Required sections: run/config/hash/environment; source/sample/feature flow; assay
QC and preparation; estimands/contrasts; full per-block results; integration
eligibility/variance/selection/stability; evaluation; clinical objectives; mapping/
enrichment coverage; target evidence/alternatives; unavailable work; limitations
and reproducible commands. Heatmaps/volcanoes/PCA/score plots/forest plots require
correct units and lineage; a decorative figure cannot substitute for evidence.

## 7. Status and close

All routes disabled can be a successful configured run and a useful report. It is
not completed implementation of every adapter or proof of biological absence.
Separate request optionality, execution status, interpretation, and project status.
Required unavailable work yields incomplete run status; optional unavailable work
is prominent with reasons. Native/conditional methods and D-T14 remain listed in
the project close, with an explicit scope disposition rather than a silent tick.
