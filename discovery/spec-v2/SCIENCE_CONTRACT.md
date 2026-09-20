# Scientific contract

## S1. Claims and units

The primary input is an identified/quantified molecular matrix, not a raw spectrum.
A measured row is not necessarily a unique molecule: protein groups, isomers and
adducts retain distinct measured-feature keys. Stable sample/subject/visit links
come from authoritative metadata. Equality of labels or counts is not linkage.
Each analysis declares its estimand, unit, eligible population, representation,
contrast direction and inference/selection limitations before fitting.

Molecular abundance differences are not binding affinity, enzyme activity, flux,
causality or a clinical diagnosis. Loadings are not fold changes. Three statistical
views of the same observations are not three independent confirmations. A
published binding assay is external evidence, never a new assay run by triomics.

## S2. Shared, not universally identical, representations

B and C use identical molecular values and maps **within** a Study/Analysis/
preparation context. A primary non-imputed analysis, an explicit imputation
sensitivity analysis, a mean-retaining analysis and a fold-specific prediction
analysis have different context IDs and lineage. This preserves common inputs
without forcing primary univariate inference to consume a predictive imputation.
C never repeats a B decomposition. An adapter's documented deterministic centering
or scaling is recorded as model state, not an unrecorded preprocessing replacement.

For paired change, delta = target visit minus reference visit, once per subject.
For independent groups, treatment-minus-control follows declared coefficients.
For between-person questions, retain levels/means. A delta's age coefficient can
represent modification of change, not a within-person age main effect. Prediction
may use only information available at its declared deployment time.

## S3. Assay policies

Use acquisition, measurement quantity, units, input scale, upstream processing,
standards and missingness evidence. A profile label routes to registered policy,
not an assumption that every proteomic omission is MNAR or every metabolomic
omission is MAR. Shared identity/linear algebra utilities are allowed; one hidden
scientific policy for every assay is not. Unknown mechanisms stay unknown.

No universal normalization method is implied. Internal-standard/sample-amount
normalization, sample offsets, PQN and compositional transformations answer
different questions. Within-class closure removes class-total information; it is
not a mandatory lipid correction. Molar percentages require quantitatively
appropriate inputs. Sum intensities only with the explicitly declared quantity
and coverage; never label a signal sum as an absolute concentration.

## S4. Inference, selection and multiplicity

limma is the default continuous molecular-response framework. DEqMS is a
qualified count-dependent extension, not automatic because a numeric count exists.
Per-feature rank, residual support, missingness and model eligibility must be
checked. Exact ordinary and moderated uncertainty are separately defined.

The default BH family is fixed per Study/Analysis/Block/scalar contrast/claim scope.
Failed members use internal p=1 when that family is otherwise valid; published
failed p/q remain unavailable. Species, classes and derived quantities are not
partitioned opportunistically after seeing results. A predeclared alternative
family is allowed when its claim is explicit. No union-wide FDR promise follows
from combining separately corrected lists. Adding a block must not change another
block's q. Invalid selection, model or dependence assumptions cannot be repaired
with imputation, p=1 placeholders or a newly named analysis.

Post-BH effect-size filtering is a reporting rule, not testing a minimum-effect
null. A treat-style thresholded null requires an explicitly enabled method and its
own validation. Declared q/effect thresholds are study choices, not universal facts.

## S5. Evaluation

Subject-disjoint nested resampling is required for supervised evaluation; temporal
restrictions apply to future-outcome prediction. All learned filtering, reference
normalization, imputation, scaling, feature selection, tuning and score selection
are training-only. A held-out mutation must not change trained state. Do not
report training separation as accuracy or association with a training outcome as
independent clinical validation. Failed folds remain in the evaluation ledger.

Strong planted controls test software. Null, weak, confounded and missing-view
regimes test limitations; non-recovery is not a universal failure. Method validation
requires a prespecified protocol and real measurement evidence when appropriate.
No claim of superiority is made from a single synthetic seed.

## S6. Target and clinical limits

Clinical fields require dictionaries, units, visit semantics and approved linkage.
Unsupported outcome families return explicit ineligibility, never a convenience
correlation. Target outputs separate metabolic compatibility, functional
perturbation, cellular engagement and direct binding, preserving negative evidence,
unknown coverage and alternative explanations. No arbitrary confidence percentage,
composite binding score or reconstructed Kd is permitted.

Sources and adopted decisions are traced in evidence/sources.json. These rules are
project contracts grounded in that evidence; numeric engineering tolerances and
study-config choices are labelled separately rather than attributed to a paper.
