# Independent bounded review of D discovery evidence

Reviewed 2026-09-05 by a separate delegated research reviewer who did not author the D note. Target: `discovery/research/D-resources-precedents.md`, especially P-Q02, P-Q04, P-Q06 and proposed conditional behavior item 3. This is **not a full specification review**, a licence review or runtime validation. The D file was not edited. No packages, datasets or services were executed or uploaded.

Disposition: the central corrections to the blanket novelty argument are supported, with two wording/provenance refinements and an enrichment-interface blocker to retain during specification. No evidence found in this bounded review warrants reinstating the original claim that all named alternatives omit lipidomics or stop at feature lists.

## Findings requiring action before specification claims are finalized

### D-R01 — P2: distinguish lipid data acceptance from a dedicated lipidomics block

**Target artifact:** P-Q02, question “OmicsAnalyst has no lipidomics block?” and its classification “Contradicted as a categorical claim.”

**Evidence:** The primary indexed [interactive scatterplot tutorial](https://pro.omicsanalyst.ca/resources/data/tutorials/2_3Dscatterplot.pdf), “Selected example dataset,” explicitly names lipidomics and RNA-seq and says it retains samples measured in both. Clean and indexed URL direct opens failed in this review; thus this remains indexed primary-source evidence, not a verified PDF download. More recent [OmicsAnalyst 2.0 Platform Overview](https://omicsanalyst.ca/docs/tutorial/PlatformOverview.xhtml), §3, explicitly lists five supported types: genes/mRNAs, proteins, miRNAs, metabolites and microbiome. Lipidomics is not separately listed. Page footer says last updated 2026-04-12; §5 advertises limma, MOFA, DIABLO and functional/network analysis.

**Why it matters:** The tutorial contradicts exclusion of lipid measurements, but does not by itself contradict the narrower assertion that there is no dedicated lipidomics block/profile. The D note already acknowledges this distinction in its prose; the headline classification should be equally precise.

**Corrected wording proposed:** “An official tutorial documents analysis of lipidomics plus RNA-seq, so categorical exclusion of lipid data is incorrect. Current 2.0 documentation lists metabolites but no separate lipidomics type. Dedicated lipid parsing, normalization and ontology coverage remain unverified.”

**Decision status:** Wording correction recommended. Do not infer dedicated lipid chemistry handling from numeric-matrix acceptance. Current executable behavior, source version and tutorial-to-2.0 correspondence remain unverified.

### D-R02 — P2: the MOI comparison should record its documented lipid interpretation, not only input acceptance

**Target artifact:** “Most consequential findings” first bullet and P-Q04.

**Evidence:** [MultiOmicsIntegrator output documentation](https://moi.readthedocs.io/en/latest/example_outputs.html#lipidomics-analysis), “Lipidomics analysis,” directly says plots are generated with `lipidr = true`. It describes class fold-change displays, enrichment between conditions, saturation distributions and BioTranslator pathway outputs. The overview also lists proteomics, lipidomics and multi-omics integration. Retrieved sidebar version is 0.1.0; page title says 0.1 documentation, live latest branch, copyright 2024.

**Why it matters:** P-Q04 correctly contradicts lipid exclusion. Its summary understates the available documented lipid-specific interpretation, which is stronger precedent evidence than generic input acceptance. A comparison matrix that marks this simply unknown could overstate triomics' difference.

**Corrected wording proposed:** “MOI documents a lipidr-enabled lipid analysis including class enrichment and saturation plots, plus pathway interpretation. These are advertised capabilities; exact implementation, identifier accuracy and study-design validity have not been tested.”

**Decision status:** Enrich the documentary comparison; runtime verification remains pending. Do not copy the source's lipid abbreviation examples as chemical authority: its caption labels CE as ceramides, so those labels need separate chemistry verification.

### D-R03 — P1 design blocker, not a newly discovered implementation bug: selected IDs or ranks alone do not satisfy all proposed enrichment backends

**Target artifact:** P-Q06 and proposed conditional behavior item 3.

**Evidence:** The inspected [MOFA2 upstream `R/enrichment.R`](https://raw.githubusercontent.com/bioFAM/MOFA2/master/R/enrichment.R), `run_enrichment`, requires a native MOFA object and binary feature-set membership matrix. It retrieves observed data, weights and factors, removes zero-variance features, intersects model and annotation features, and applies a competitive test. `.compute_feature_statistics` uses absolute weights; the correlation-adjusted path uses observed-data correlations. Master is unpinned; these are static source findings, not installed-release behavior. [`lipidr::lsea` documentation](https://www.lipidr.org/reference/lsea.html), Usage, Arguments and Examples, consumes DE results, ranks by a configured statistic, and delegates to fgsea.

**Why it matters:** The D note correctly flags insufficiency of selected candidates. Its alternatives “background or complete ranked statistic” still do not fully enumerate the information needed to call MOFA2's native function. Preserving all ranks would not supply the native model/data contract. Default absolute-weight enrichment also cannot be described as signed abundance enrichment.

**Corrected wording proposed:** “Each enrichment adapter must declare its exact inputs and null hypothesis: candidate membership plus an explicit eligible universe for overrepresentation; full eligible feature statistics for ranked enrichment; and model/data/annotation inputs where required by model-based enrichment. A selected candidate list is adequate for some annotation lookups but does not define all enrichment analyses.”

**Decision status:** Retain as an explicit unresolved architecture decision requiring orchestrator ADR/approval. Options include a backend-specific enrichment stage before conversion to the common result contract, or an approved enriched contract. This reviewer adopts neither. Future specification must resolve native-object dependency, sign interpretation, background intersections and annotation losses. No current pipeline implementation exists here to test.

## Confirmations from the remaining source checks

- **BioPAN is not a candidate-ID lookup.** [BioPAN 1, Step 1](https://lipidmaps.org/tool/biopan/doc/step1.html), “How to prepare your data,” requires quantitative CSV columns, at least two conditions and two samples per condition. This supports D-Q10's need for a separate quantitative-data contract. The loading page allows at least two conditions in a file; it alone does not establish how comparisons are selected or whether paired/covariate-adjusted inference is preserved. Those remain unverified. This review did not rerun BioPAN or inspect its calculation source.
- **MOFA2 enrichment exists.** Source inspection confirms the correction to “stops at feature lists.” It does not prove an identifier-resolution service or lipid-specific ontology mapping. The D note's distinction is appropriate.
- **Full ranked data and background are different objects.** LSEA documentation supports retaining DE statistics and rank provenance. It does not validate a specific triomics statistic/default or justify ranking raw unsigned p-values as directional evidence. Candidate selection, feature eligibility, mapping and set-size filtering must be logged independently.

## Inspection record and scope limits

Read the complete D note. Independently inspected five primary pages/source files: MOI output docs, MOFA2 enrichment source, lipidr LSEA docs, BioPAN loading docs, and current OmicsAnalyst 2.0 overview. The legacy OmicsAnalyst tutorial was checked using primary indexed text after direct opens failed. No session-decorated tutorial URL is retained in this artifact. Searches returned other pages, but they are not treated here as behavior verification.

Still unverified: all executable behavior; current source commits and hashes; exact installed dependency versions; database releases and institutional licences; GO/Reactome/KEGG/HMDB/LION claims outside this bounded review; comparison success on actual measured data; correctness of MOI's chemical annotations; complete enrichment null hypotheses and multiplicity policy; and the rest of D's specification package. The absence of additional findings within this scope is not approval of those items.

Next action: orchestrator should reconcile D-R01/D-R02 wording, carry D-R03 into the decision/ADR queue, then obtain independent review of the eventual complete D specification and executable evidence when approved. This note does not authorize any contract change.
