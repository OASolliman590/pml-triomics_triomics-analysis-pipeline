# Configuration contract and migration

The canonical machine schema is `schemas/config.schema.json` (Draft 2020-12,
version 2.0). Its schema $id is an identifier, not a hosted service. Runtime
validation is local and may not fetch schemas. The schema defines every accepted
key; omitted optional method arguments are null, not hidden scientific defaults.
`methods.json` fixes method eligibility. Unknown keys and duplicate YAML keys fail.
A-T01 must reject duplicate mappings **before** a YAML parser overwrites them.

## Input rules

Resolve source and metadata paths relative to the config file. Output root is
supplied by the CLI, not embedded private paths in version-controlled templates.
Source hashes supplied by the analyst must match; absent hashes are computed from
actual source bytes on ingest and frozen in the resolved manifest. Original
workbook conversion has a separate manifest/hash contract. No file existence is
inferred from the presence of a string in JSON Schema.

Required scientific choices: quantity/scale/units and upstream steps; missingness
evidence and filtering rationale; transform/normalization/imputation policy;
exact design/contrast/analysis unit; q/effect units and family; integration design
and tuning; clinical dictionary/objectives; licensed resource snapshot and target
context. Numerical values in synthetic examples are **fixture choices**, not
recommended clinical or statistical defaults. Unknown real-data decisions remain
explicit unavailable requests until the relevant evidence/choice is provided.

## Semantic validation beyond JSON Schema

A-T01/02 implement stable uniqueness and referential checks: study/block/source/
analysis IDs, sample columns, feature keys, factors/levels, source_refs, block_ids,
context compatibility and one declared role per numeric input column. File
contents, row counts and order are checked at ingest, not JSON Schema validation.

Reject a repeated design lacking subject/visit identity; a non-delta paired change;
a delta target requiring a removed level; identical target/reference visits;
missing factor references; undeclared contrast columns; non-estimable weights;
a composition without denominator; already-log input plus log transform; VSN plus
another log; PQN on negative/log values; mechanism-specific imputation without
qualified assumptions; future-data predictors in baseline prediction.

For transform.none, reference/log-base/zero-value must be inapplicable/null unless
required by a declared zero policy. For normalisation.none, reference='none' and
all method-specific refs are null. PQN/median_shift require a resolved training or
supplied reference appropriate to that method. A non-none imputer must satisfy its
parameter and MethodQualification contract where required. It cannot impute an
entire absent view or learn from held-out units. A-T03 owns this semantic registry.

For each integration request, resolve exactly the declared blocks. Require >=2,
compatible outcome type and eligible training/evaluation units. DIABLO/block.spls
require explicit named symmetric design matching the predictor block set,
consensus_mean, tuning grid and native prediction rule. `keepX`/components are
bounded by training features/rank in every fold. Continuous block.spls additionally
requires scalar Y, keepY and response scaling in its native options contract.
MOFA requires convergence/factors/seeds and a qualified environment; no supervised
outcome is supplied. PCA has native scores and explicit scaling/selection.
Nested CV requires a non-none metric, feasible outer/inner folds and qualified
inductive preprocessing/adapter. Schema-valid is not statistically eligible.

Clinical.enabled requires dictionary/observation/link files and modelable objectives;
disabled clinical has no silently fitted objectives. Annotation resources require
rights and operation evidence for requested use. Target.enabled requires identity,
context, evidence/mechanism and protocol; discover additionally requires a bounded
universe. No target is hardcoded in package defaults. `quantitative_method_ref`
opens no D-T14 capability until its approval/validation gate passes.

## Backward compatibility

The v1 keys `blocks[].name/file/id_namespace/annotation_route` keep their meaning.
The former scalar `normalisation` migrates into `preprocessing.normalisation.method`
with an explicit report of the conversion, never a silently chosen reference.
Former single `design` is promoted into one named analyses[] record only when
representation/contrasts can be supplied explicitly; missing semantics fail with
a migration diagnostic. Display names do not become profile dispatch rules.
No worker must support every inconsistent historical draft as a second live schema.
A source using schema_version=1 is refused with a precise migration message until
an explicit validated migration is requested; silent v1 acceptance is prohibited.

## CLI and output tree

```
Rscript --vanilla inst/exec/triomics --config CONFIG --stage A --output OUTPUT --validate-only
Rscript --vanilla inst/exec/triomics --config CONFIG --stage all --output OUTPUT
Rscript --vanilla tools/check-package.R
```

Those R commands are **implementation acceptance commands**, not currently
implemented executables. Stage grammar is A/B/C/D/report/all; no `--mode` alias.
Validation-only runs no fit and creates no analysis checkpoint. It may emit a safe
validation summary. It checks the requested stage, not unavailable future stages.
No implicit interactive installation or credential prompts inside a run.

Output under the selected root: `run.json`, `resolved-config.json`, `sources/`,
`A/`, `B/`, `C/`, `D/`, `clinical/`, `report/`, `logs/` and `provenance/` as requested.
Each stage writes immutable envelopes/payloads and statuses. A later B score request
uses a new request_id and does not overwrite earlier feature results. Reports use
hashed references. All study artifacts are local ignored outputs by default.
Exit semantics and error fields are in DATA_CONTRACTS.md. Request completeness
and whole-programme implementation completeness are different quantities.
