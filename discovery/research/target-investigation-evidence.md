# Evidence for a general target-investigation extension

Date: 2026-09-06. Status: research supporting a proposed Spec D extension; not implementation, user approval, or an Opus-reviewed amendment. No study data were used. Six primary papers or official documentation sources were consulted below. Resource releases, licenses, redistribution terms, and executable adapters have **not** been approved or verified for production use.

## Findings and limits

### 1. Metabolic responses can generate mechanism hypotheses

Zampieri et al. developed a reference-compound metabolomic profiling strategy in *Mycobacterium smegmatis*, including metabolic and nonmetabolic drug mechanisms. They then investigated compounds with unknown mechanisms. Some inferred mechanisms were experimentally validated; proposed new mechanisms were investigated using resistant-mutant sequencing and compound–proteome interaction analysis. This is a concrete precedent for using metabolomics to propose and compare mechanisms, with orthogonal follow-up. It does not establish a universal target-identification algorithm for human observational studies or arbitrary targets. [Zampieri et al., 2018, *Science Translational Medicine*, DOI 10.1126/scitranslmed.aal3973](https://pubmed.ncbi.nlm.nih.gov/29467300/).

**Design inference:** offer both a specified-target investigation and an explicitly bounded candidate-discovery mode. A signature match is hypothesis support in its reference context. Without a suitable reference panel, do not imply the same validated predictive performance. Record species, tissue, perturbation, dose, time, and reference-panel provenance; expose alternative mechanisms instead of producing only a favored target.

### 2. Abundance is not a flux or inhibition measurement

Park et al. measured absolute metabolite concentrations and fluxes with isotope tracers, integrated nutrient uptake/secretion and isotope data, and related reaction free energies to concentration and forward/backward flux ratios. They explicitly describe LC–MS signal dependence on ionization efficiency, which prevents raw signal from directly representing absolute concentration. [Park et al., 2016, *Nature Chemical Biology*, DOI 10.1038/nchembio.2077](https://pmc.ncbi.nlm.nih.gov/articles/PMC4912430/).

**Design inference:** fold changes alone must not yield measured flux, target inhibition, enzyme activity, or absolute substrate/product ratios. Even an expected substrate-rise/product-fall pattern is conditional mechanistic compatibility, not a direct assay. Preserve whether inputs are intensities, relative abundance, absolute concentration, or validated flux estimates. Directional expectations require a cited model and declared assumptions. Optional proteomic abundance provides a separate measured axis, not an enzyme-activity substitute.

### 3. Reaction mapping needs explicit direction semantics

Rhea documents undefined, forward, reverse, and bidirectional versions of reaction records; the left/right layout alone does not mean substrate/product. Its links to other resources, including UniProtKB, respect supported direction representations. [Rhea: reaction sides and directions](https://www.rhea-db.org/help/reaction-side-direction).

**Design inference:** an enzyme/reaction route can connect identified metabolites with candidate molecular functions, but must carry the exact reaction record, mapping provenance, ambiguity, direction state, and context assumptions. Do not silently convert graphical layout into reaction direction in the user's tissue. Distinguish chemical participation from catalysis, transport, regulation, and drug binding. Rhea is a candidate resource, not a blanket-approved production dependency.

### 4. Nonenzymatic targets require typed regulatory paths

Reactome's official details model exposes reaction inputs/outputs, catalysts or active complex units, regulators, cellular compartments, literature, and species-inference labels. A human event may be inferred from another species, and electronic inference is separately represented. [Reactome: details panel](https://reactome.org/userguide/details-panel).

**Design inference:** receptors, signaling proteins, complexes, and other nonenzymatic targets can be investigated through documented regulatory paths. A path to changed metabolites is mechanistic context; it does not establish a direct target effect or unique causal explanation. Keep intermediate nodes, edge types, source citations, path length, compartment, species, and inferred-versus-experimental provenance. Missing paths or weak assay coverage must produce an explicit “not assessable” result. A large knowledge graph must not make heavily annotated targets look validated merely because they have many connections.

### 5. Bioactivity evidence must remain assay-aware

ChEMBL distinguishes binding assays from functional assays and other assay classes. Its assay-to-target confidence describes target assignment, including molecular specificity and homolog mapping. Its pChEMBL field can summarize multiple potency/response/affinity measurement types; it is not a universal binding-affinity measurement. [ChEMBL: assay and activity questions](https://chembl.gitbook.io/chembl-interface-documentation/frequently-asked-questions/chembl-data-questions).

**Design inference:** optional drug evidence must retain assay description/type, target assignment/type, organism, endpoint, value, units, relation, validity flags, and original reference. Do not relabel functional or phenotypic activity as direct binding; do not pool heterogeneous endpoints into a purported affinity. Confidence in database target assignment is not a calibrated probability that the user's drug engages that target in this study. ChEMBL and other drug/target resources require their own release, license, and adapter decisions before use.

### 6. Engagement evidence is an additional experimental layer

Martinez Molina et al. introduced CETSA to evaluate drug binding in cells and tissue using ligand-associated target-protein thermal stabilization, with validation in selected clinical targets. This is an example of a purpose-built engagement experiment, distinct from routine abundance profiling. [Martinez Molina et al., 2013, *Science*, DOI 10.1126/science.1233606](https://pubmed.ncbi.nlm.nih.gov/23828940/).

**Design inference:** accept separately supplied engagement-assay evidence with experimental context, controls, assay-specific interpretation, provenance, and limitations. Routine metabolomics or total-protein abundance cannot manufacture this evidence. Do not treat any thermal change, docking score, or pathway association as unconditional proof of direct binding. The proposed extension should recommend discriminating experiments where appropriate, without claiming to execute them.

## Concrete constraints for Spec D

The following are proposed design requirements derived from the distinctions above, not claims that an existing method already satisfies them:

- **General interface, bounded assessability:** allow any resolvable target query, but distinguish assessable, partly assessable, and not assessable. Accept a target ID, organism/context, analysis/contrast, and optional drug; discovery mode must declare its candidate universe and resource coverage.
- **Evidence ledger:** preserve each observation's originating A/B/C feature or result, identity uncertainty, direction/effect uncertainty, and all edges leading to a target. Keep measured omics, database relationships, model predictions, and supplied engagement experiments distinct.
- **Balanced conclusions:** report supporting, contradictory, ambiguous, unmeasured, and unavailable evidence separately. Missing evidence and nonsignificance are not target refutation. Compare plausible alternatives and expose dependent evidence reused across pathways or omics branches.
- **Statistical boundary:** exploratory target ranking is neither a binding probability nor automatically a calibrated significance result. Any target-level test requires a defined null, eligible universe, selection-aware design, dependence handling, multiple-testing family, and validation. Post hoc target discovery must be labeled exploratory.
- **No circular confirmation:** B and C outputs derived from the same samples are complementary descriptions, not independent replication. A target chosen from these data cannot be “confirmed” by rescoring the same evidence. Reference-signature matching and predictive claims require suitable held-out evaluation.
- **Separate deliverable:** emit a target evidence report with coverage, causal limitations, conflicting explanations, source versions, and suggested follow-up. It extends interpretation; it must not silently change the protected B/C nomination routes, BlockProfile interfaces, or existing IntegrationResult contract.

This extension is feasible as a reusable investigation framework. The evidence supports qualified target hypotheses and transparent uncertainty, not a promise that metabolomics can identify or prove binding to every target.

## Root supplement: human-cell precedent

Root opened the abstract and data/code-availability sections of [Schuhknecht et al., 2025, A human metabolic map of pharmacological perturbations reveals drug modes of action](https://www.nature.com/articles/s41587-024-02524-5), DOI 10.1038/s41587-024-02524-5. The abstract reports profiling a large drug panel in human cancer cells and follow-up confirmation of selected receptor agonist and enzyme inhibitor predictions. This extends the precedent beyond bacterial metabolic targets. It does not establish transfer to observational human biofluids, arbitrary tissues or every queried target. Full methods and benchmark performance were not reviewed by root in this pass; no reference data/software was downloaded or approved. This is an additional seventh source, distinct from the researcher's six-source review above.
