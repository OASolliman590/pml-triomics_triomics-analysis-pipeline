# Bounded implementation tickets

## Current delegation lanes

Use the [current user-approved fleet](../operations/active-delegate-fleet.json): Sol High manages; Grok 4.6 medium implements features; agy/Gemini handles bugfixes and docs; planning, debate, Spark and read-only reviews use their configured lanes. This supersedes older mandatory Gemini/Sonnet/Opus sequencing below. [Standing authorization](../operations/task-authorization.md) and this ticket's prerequisites still apply. Model allocation does not bypass a provider permission denial or expand an approved payload.


## Standing authorization

User-approved workflow: use the current configured lanes above, with Sol High management and minimal Astra acceptance. Read [task authorization](../operations/task-authorization.md) before dispatch. Do not re-request settled workflow approval. The approved A-T01 provider file list and Root/Sol AIU SSH/setup permissions are scoped there; they do not automatically authorize this ticket's unlisted files for external upload. Preserve ticket prerequisites and runtime approval enforcement.

Date: 2026-09-06. All 41 existing ticket identities are preserved; C-07 remains an evaluation evidence item, not a newly invented ticket. Each card provides concrete future test and CLI commands, a synthetic fixture deliverable, predecessor edges and acceptance. No code or test is claimed to exist or run.

Read [command contract](../operations/implementation-command-contract.md), [Astra settlement](../reviews/astra-design-arbitration.md) and [AIU readiness](../operations/aiu-hpc-readiness.md). G0/runtime gates apply to every production card.

The predecessor graph is acyclic. A precedes B, B precedes C, C precedes D. A-T09 clinical ingest precedes A-T08 handoff; B-T08/09 precede B-T07 handoff; C-08 precedes C-06 handoff. C-09 and conditional D methods require explicit completion or scope disposition before final report/vignette closure. Shared files are assigned sequentially. D-T04 has D-T01 as core predecessor and explicitly requires D-T02, D-T03, or both according to its declared method. D-T05 core status handling depends only on D-T01; enabled adapters carry separate conditional gates. D-T08 can render unavailable optional routes without successful licensed enrichment. Completing reports does not complete unavailable adapter tickets or waive whole-project scope.

| Ticket | Deliverable | Predecessors |
|---|---|---|
| [A-T01](A-T01.md) | Validate and explain a synthetic Study configuration | G0 |
| [A-T02](A-T02.md) | Generate and ingest explicitly keyed synthetic multi-Block data | A-T01 |
| [A-T03](A-T03.md) | Assemble a Study without silently changing its Samples | A-T02 |
| [A-T04](A-T04.md) | Run the approved proteomic policy with complete provenance | A-T03 |
| [A-T05](A-T05.md) | Run the approved metabolomic policy with an explicit reference | A-T04 |
| [A-T06](A-T06.md) | Preserve lipid chemistry and endpoint meaning end to end | A-T04 |
| [A-T07](A-T07.md) | Apply approved preprocessing inside a held-out split | A-T04, A-T05, A-T06 |
| [A-T08](A-T08.md) | Reproduce, extend and hand off a complete A stage | A-T07, A-T09 |
| [A-T09](A-T09.md) | clinical source/dictionary/linkage | A-T02 |
| [B-T01](B-T01.md) | Independent two-group inference slice | A-T08 |
| [B-T02](B-T02.md) | Declared contrasts and continuous covariates | B-T01 |
| [B-T03](B-T03.md) | Paired inference and shared representation | B-T01 |
| [B-T04](B-T04.md) | Multi-visit and trajectory inference | B-T02, B-T03 |
| [B-T05](B-T05.md) | Count-dependent precision slice | B-T01, B-T03 |
| [B-T06](B-T06.md) | Feature families, thresholds and sensitivity | B-T02, B-T03 |
| [B-T07](B-T07.md) | Reproducible B CLI/report handoff milestone | B-T04, B-T05, B-T06, B-T08, B-T09 |
| [B-T08](B-T08.md) | one clinical association family | A-T09, B-T01 |
| [B-T09](B-T09.md) | longitudinal clinical association | B-T08, B-T03 |
| [C-01](C-01.md) | common result and baseline PCA slice | B-T07 |
| [C-02](C-02.md) | supervised baseline and evaluation | C-01, A-T07 |
| [C-03](C-03.md) | DIABLO categorical slice | C-02 |
| [C-04](C-04.md) | continuous block.spls slice | C-03 |
| [C-05](C-05.md) | MOFA slice | C-04 |
| [C-06](C-06.md) | comparison and D handoff slice | C-05, C-08 |
| [C-08](C-08.md) | score/clinical association handoff | C-05, B-T08, B-T09 |
| [C-09](C-09.md) | incremental clinical prediction, conditional | C-08, C-02 |
| [D-T01](D-T01.md) | Explain all candidates with every route disabled. Milestone 7. Blocked by approved A/B/C identity/result contracts, the preceding pipeline milestones and D decisions. | C-06 |
| [D-T02](D-T02.md) | Map ambiguous lipids locally. Milestone 7. Blocked by D-T01 and approved parser/resource licences. | D-T01 |
| [D-T03](D-T03.md) | Map gene products and reactions locally. Milestone 7. Blocked by D-T01 and chosen resource/ChEBI review. | D-T01 |
| [D-T04](D-T04.md) | Perform one explicitly specified enrichment analysis. Milestone 7. Blocked by relevant D-T02/03, universe/rank ADR and method/multiplicity decisions. | D-T01 |
| [D-T05](D-T05.md) | Prove optional-route failure and required-route completeness. Milestone 7. Blocked by D-T01 and each adapter included. | D-T01 |
| [D-T06](D-T06.md) | Native MOFA enrichment, conditional. Milestone 7 conditional subroute. Blocked by D-T04 plus model-input ADR, pinned backend and independent null/sign review. | D-T04, D-T05 |
| [D-T07](D-T07.md) | BioPAN comparison, conditional. Milestone 7 conditional subroute. Blocked by D-T02/05 plus local rights, compatible quantitative/design contract and calculation review. | D-T02, D-T05 |
| [D-T08](D-T08.md) | Full-pipeline offline, safe report. Milestone 8 only, after milestone 7 disposition. Blocked by approved A/B/C report exports, D-T01/05 and all adapters chosen for milestone. | D-T05 |
| [D-T09](D-T09.md) | Reproducible paired and independent two-group walkthroughs. Milestone 9 only, after milestone 8 and preceding pipeline milestone disposition. Blocked by D-T08 and approved runnable pipeline/fixtures. | D-T13, D-T15 |
| [D-T10](D-T10.md) | target request/evidence account, milestone 7 | D-T03 |
| [D-T11](D-T11.md) | named target and alternatives, milestone 7 | D-T10 |
| [D-T12](D-T12.md) | descriptive discovery, milestone 7 | D-T11 |
| [D-T13](D-T13.md) | target dossier, milestone 8 | D-T08, D-T12 |
| [D-T14](D-T14.md) | one quantitative target method, conditional milestone 7 | D-T11, D-T12 |
| [D-T15](D-T15.md) | clinical association report | D-T08, C-08 |
