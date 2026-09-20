# triomics

A multi-block molecular-assay study analysis domain, spanning per-block inference, cross-block structure and molecule-aware interpretation.

## Language

**Study**: A collection of molecular measurements and metadata linked by explicit sample identities and a stated experimental design.

**Block**: A feature-by-sample molecular assay matrix with its own measurement properties and feature identifiers. The block name is a label, not a definition of processing behavior.

**Sample**: A measured specimen or observation. A subject may contribute more than one sample.

**Subject**: The individual or experimental unit linking repeated observations. Independent designs need not declare subjects.

**Analysis unit**: The entity represented by one observation in a resolved analysis, such as a sample or a paired subject difference. It must not be confused with the original sample identity.

**Feature**: A measured or explicitly derived molecular quantity, identified within an identifier namespace and its originating block.

**Identifier namespace**: The system giving a molecular identifier its meaning; identical strings in different namespaces do not establish identical entities.

**Block profile**: The declared assay-processing policy for a block, including validation, filtering, transformation, normalization, missingness handling and optional derivation.

**Design**: The declared relationship between observations, predictors, repeated measurements and the contrast or association being estimated.

**Decomposition**: An explicitly defined separation or transformation of measured variation into the signal used by downstream analyses.

**Candidate feature**: A feature nominated by a statistical or integration result, with its source and selection rule retained. Nomination does not establish biological truth.

**Annotation route**: A molecule-identifier-aware path for mapping and interpreting candidates, conditional on resource coverage and permitted use.

**Synthetic ground truth**: Values deliberately planted in a simulated study. They establish what the simulation generated, not guaranteed recoverability by every method.
