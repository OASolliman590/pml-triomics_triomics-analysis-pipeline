# triomics discovery record

## Language approved; Spark coding lane — 6 September 2026

[R core plus Python and the execution routing are approved](operations/language-and-execution-approval.md). Actual Spark completed [A-T01 ticket preparation](reviews/spark-a-t01-handoff-response.md); the [bounded dispatch handoff](tickets/A-T01-dispatch.md) incorporates root corrections. The user now requests Spark for implementation/tests while ZCode is configured. AIU inventory is complete but provisioning and original production/publication prerequisites remain. No production code or implementation tests have run. This checkpoint supersedes historical language-pending text below.

## Current checkpoint — 6 September 2026

[Shared design settled](operations/contract-settlement.md) by actual Opus–Sol debate with [Astra confirmation and final recheck](reviews/astra-settlement-recheck.md). [41 individual implementation tickets](tickets/README.md) now contain concrete future commands and acceptance. [AIU inspection](research/aiu-hpc-environment-audit.md) found a usable but incomplete managed R environment. Language confirmation, coherent environment completion, scientific validation and original publication gates remain. Historical status entries below describe their dated checkpoints.

Clinical-association scope approved by the user on 6 September 2026. [Approval and remaining dependencies](operations/clinical-scope-approval.md). Earlier proposal/review entries below are historical checkpoints.

Status: **goal blocked pending required user input**, 5 September 2026. Four provisional specification kits are drafted, independently reviewed and revised. Scientific/contract decisions and executable validation remain open. No package is implementation-ready; this directory is not a Git backup. See the [blocked audit and resume inputs](operations/blocked-audit.md).

The two supplied attachments are preserved byte-for-byte under `inputs/`. The execution prompt owns the goal and milestone gates; the supplied architecture context is an input to reconcile. Older handovers, specifications, execution-host details and the exact Notion project location have not yet been located. No production code has been written, no project dependency installed, no fleet configuration changed, and no implementation test has run.

## Three-study review checkpoint — 6 September 2026

The user requested a design review with Claude Opus for three projects under the supplied study-data directory. A read-only structural audit inspected nine workbooks/49 sheets. Actual Claude Opus 5 completed the first review; root revised all four kits and produced a [concrete confirmation proposal](spec-drafts/three-study-revision-proposal.md), [shared source contract](operations/source-contract-proposal.md) and [response to findings](reviews/opus-review-response.md). The [raw review](reviews/opus-three-study-design-review.md) preserves its original claims and coverage limits. The second pass and final residual dispositions are recorded below and in progress.md.

The [second Opus pass](reviews/opus-three-study-design-recheck.md) has now completed: architecture suitable for owner confirmation, with residual details subsequently corrected and locally checked by root as recorded in the response. The new source folder establishes data location, not historical handover/repository/Notion/host or R approval. All original scope and implementation gates remain. Structural audit records are local and not automatically publishable. Historical hashes refer to prior snapshots; the current A–D amendments have separate provenance.

## New target-investigation proposal

The user's subsequent request generalized drug-target investigation to arbitrary targets. [The new D extension](spec-drafts/D-target-investigation-extension.md) proposes assess/compare/discover modes and an evidence dossier covering support, contradiction, alternatives, coverage and separate binding/engagement evidence. [Primary-source research](research/target-investigation-evidence.md) supports the distinctions and records resource/validation limits. This addition is outside the earlier Opus checkpoint; its own review/status is recorded in progress.md. No implementation or resource approval is implied.

## Clinical metadata association proposal

The [clinical extension](spec-drafts/clinical-molecular-association-extension.md) adds Study-specific dictionaries/visit links, granular objectives and one B-owned association service for clinical–feature/score relationships, with C evaluation and D reporting. The [source audit](research/clinical-metadata-design-audit.md) documents newly available Fasting clinical sources and unresolved clinical-table locations for the other two projects. [Research evidence](research/clinical-association-evidence.md) distinguishes within/between-person relationships, missingness, confounding and score circularity. The new extension's separate review/status is recorded in progress.md; earlier Opus reviews do not approve it. Local audit aggregates are not automatically publishable; no patient data belong in code or delegate prompts.

## Scope retained

| Stable component | Owned stages | Supplied historical sections | Default implementation milestones |
| --- | --- | --- | --- |
| A | Ingest, configuration, block profiles, synthetic study generator | Architecture context §§5,10 | 1–3 |
| B | Design resolver and per-block statistics | §§6–7 | 4 |
| C | Concatenation baseline, mixOmics, MOFA2 and common result contract | §8 | 5–6 |
| D | Namespace-based annotation router and rendered report, conditional resource routes | §§4,9 | 7–8 |
| Cross-cutting | Reproduction, vignettes, provenance, reviews, milestone publication | §§9–12 | 9 and every milestone |

No earlier component numbering has been supplied; this table does not invent a historical crosswalk. Extend it after reading the handover.

The product remains a design-agnostic CLI/report pipeline operating on quantified feature-by-sample matrices, with N blocks and three built-in profiles. It is neither a raw-data processor nor a new statistical method. Both statistical branches feed both molecule-type annotation routes. Names are display labels, not dispatch conditions. FDR remains separate by block. No real study data may enter version control.

The BlockProfile and IntegrationResult interfaces in the preserved inputs remain the owner contracts. Evidence-based amendments require documented decisions; this record does not amend them. The distinction between authoritative shared matrices, analysis-unit identity, statistical transformations and adapter scaling needs explicit resolution during grilling.

## Draft packages and review record

Each package contains the ten requested kit categories, including evidence, proposed contracts/configuration, validation plans, two illustrative designs and bounded tickets. Their unresolved sections are substantive gaps, not completed verification.

| Component | Draft | Evidence | Independent review / revision verification |
|---|---|---|---|
| A | [Ingest and profiles](spec-drafts/A-ingest-profiles.md) | [A evidence](research/A-preprocessing.md) | [Review](reviews/A-spec-review.md), [revision verification](reviews/AD-spec-rereview.md) |
| B | [Design and statistics](spec-drafts/B-design-statistics.md) | [B/C evidence](research/BC-methods.md) | [Review](reviews/B-spec-review.md), [re-review](reviews/B-spec-rereview.md) |
| C | [Integration](spec-drafts/C-integration.md) | [B/C evidence](research/BC-methods.md) | [Review](reviews/C-spec-review.md), [re-review](reviews/C-spec-rereview.md) |
| D | [Annotation and report](spec-drafts/D-annotation-report.md) | [Resources and precedents](research/D-resources-precedents.md) | [Review](reviews/D-spec-review.md), [revision verification](reviews/AD-spec-rereview.md) |

Read the [decision frontier](operations/decision-frontier.md), [cross-component review](reviews/cross-component-review.md) and [shared contract proposal](operations/shared-contract-proposal.md) before adopting any local schema. The proposal reconciles keys, preparation contexts, run states, output roots and milestone order for review; it is not an accepted ADR.

The subsequent [runtime compatibility investigation](research/runtime-compatibility.md) proposes R 4.6.1/Bioconductor 3.23 and distinguishes released MOFA2 1.22.0's Python 3.12.12/mofapy2 0.7.3 pins from development versions. Its [independent source review](reviews/runtime-compatibility-review.md) is separate from the original draft snapshots. No runtime or dependency choice is adopted by the proposal.

## Records

- `operations/progress.md`: verified actions and next steps.
- `operations/pending-validation.md`: unrun checks with dependencies.
- `operations/synchronization.md`: exact external updates awaiting a verified destination.
- `operations/handover.md`: restart entry point.
- `research/`: 39 component/precedent questions across three ledgers plus three runtime questions in a source-verification supplement. Independent evidence reviews qualify A and D findings; exact installed behavior and full source/resource pinning remain pending.
- `reviews/`: independent draft reviews, author responses, targeted re-reviews and the cross-component review. Historical hashes identify their actual snapshots.
- `operations/artifact-manifest.json`: final local file hashes; administrative integrity evidence, not a software test or remote backup.

## Readiness rule

All four design packages must be complete and independently reviewed before production implementation. A complete conditional D design remains required even if particular external routes cannot open. No blocked or conditional package is labelled ready. Synthetic checks establish specified software behavior, not real-measurement validity or guaranteed biological discovery.

Clinical extension review: [Opus findings](reviews/opus-clinical-extension-review.md), [root dispositions](reviews/opus-clinical-extension-response.md), [run provenance](operations/clinical-extension-review-provenance.json). Final root revisions are locally checked; implementation remains pending.
