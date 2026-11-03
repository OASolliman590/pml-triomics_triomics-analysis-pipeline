# M7-024 — native general target identity and descriptive profiles

Frozen design scope only. No implementation until mapped predecessors and D05
verify AND this scope passes Astra. The read-only design audit may run earlier
without releasing implementation. New bounded cycle, never another repair of
the historical exhausted D11 cycle. User authorizes finishing required Stage D
with Luna and Astra; M7/G7 remain closed.

Current CLI discards supplied drug/construct identity, lacks general mechanism
publication and special-cases SYNTH_FCN3_LIKE. External assay attachment currently
checks taxon/context/construct/drug but not exact target namespace/ID. These are
implementation defects against the existing target contract, not authorization
to infer affinities, mechanisms or quantitative rankings.

Use existing local protocol_ref/context_ref/drug_ref/mechanism_file and
external_evidence_file seams to provide declared typed target identities,
constructs, selection history, mechanism classes, documented contextual links and
external assays. No config/provenance schema changes. Bind consumed local file
bytes by SHA256 and persist provenance. Identity cannot be learned from the
assay it is meant to verify. Exact target namespace/ID/taxon/entity/construct,
compound namespace/ID and context must match before attachment; missing identity
remains unresolved and mismatches explicit. Legacy assay records without target
identity cannot be grandfathered into a match. Strengthen the matching tests;
never drop scientific guards. Preserve exact endpoint/relation/value/units/source,
assay limitations, external status and publication/dependency groups. No IC50 or
docking conversion to Kd, inferred probability or composite potency score.

General assess/compare profiles must consume supplied mechanism classes and
sourced signed/unsigned contextual paths. Never hardcode a biological class for
a new target. Unknown class/path stays explicitly unassessable; an absent file
cannot manufacture links. Keep synthetic-only legacy fixture expectations where
truthful. Preserve the four evidence axes separately; the existing D11 mechanistic
claim guard remains for omics profiles. Direct experimental Kd in a sourced assay
stays external binding evidence, never sent through an omics-effect conversion.

Molecular observations must resolve to immutable same-run B/C evidence, keeping
source-feature/result IDs, effect/unit, missingness and selection lineage; no
arbitrary supplied numbers called this-study measurements. B/C analyses of the
same observations and duplicate molecular identities must not add independent
confirmation. Preserve support/conflict/ambiguity/unmeasured states and explicit
coverage; alternatives remain descriptive, noncomparable where appropriate and
unranked with no forced winner. No universal substrate/product-direction rule or
invented reaction for a nonenzymatic target. Clinical claims remain limited to
available verified source evidence; no clinical benefit or causal mediation.

Fresh Luna allowlist R/target_request.R, R/target_evidence.R,
R/target_assessment.R, R/target_alternatives.R,
R/annotation_apply.R (target request/evidence seam only),
tests/testthat/test-D-T10.R, test-D-T11.R,
new tests/testthat/test-D-target-shared.R, tickets D-T10.yaml/D-T11.yaml,
public synthetic inst/examples/data/d-target/,
registrations D-T10.json/D-T11.json (accepted false).
Sol-only proposals R/cli.R existing target publication and NAMESPACE/matching
manual documentation. Protein-metabolite compatibility output remains descriptive;
any necessary extra file gets explicit scoped review before editing.

Behavioral RED then focused/native GREEN. Include two ordinary nonspecial targets,
matched and mismatched external assays, different target same construct, absent
identity/affinity, documented mechanism with real same-run molecular read-back,
duplicate/dependent observations, unknown class/coverage, unequal coverage and
equal-evidence ties. Verify no inferred binding/activity/flux/causation, no winning
rank and complete file hash binding. Native B/C/D from clean source; D01 regression.
Initial+two repairs, one scope audit+one focused rereview. After Sol verification
D12 may proceed. No private data, push, schema/statistical/frozen changes, later
reports, conditional-method implementation or acceptance.

Sol independently reproduced the target-identity defect on source8b41879: a
request for TARGET_A accepts a record explicitly labelled DIFFERENT_TARGET_B
when construct/taxon/context/drug match. The diagnostic is saved in
/tmp/triomics-stage-d-evidence/m7-024/identity-probe.R and identity-probe.json.
It is RED evidence, not acceptance. Missing exact identity must stay unresolved;
update old positive fixtures to supply identity rather than weakening that rule.
