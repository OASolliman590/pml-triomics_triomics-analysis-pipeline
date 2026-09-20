# triomics implementation plan

**Goal:** implement the audited contracts with test-backed increments and honest
qualification; do not run a new discovery programme to delay coding indefinitely.
**Architecture:** one R package; A preparation, B models/clinical fits, C integration/
evaluation, D annotation/report. Artifact scheduling, not cross-module callbacks.
**Stack:** R/Bioconductor, isolated Python MOFA, YAML, SE/MAE, testthat, renv, Quarto.
**Spec:** this directory's A/B/C/D contracts and machine schemas.

## Global constraints and highest-risk seams

Preserve six profile methods, simulator signature, six IntegrationResult fields,
Study-scoped source identity and the accepted shared context semantics. No study
data in Git. All statistical choices without a general correct default stay
explicit. The most important adversarial seams are unobserved/selected input
universes, aliased repeated designs, train-test leakage, native score/variance
adaptation and target-binding claim inflation. They have explicit owners/tests
below and in VALIDATION.md, not a generic instruction to test later.

## Authoritative packet table

41 implementation tickets; C-07 is evaluation evidence within C-06, not a ticket.
Nine separate Terra documentation packets do not change that historical count.

| ID | Deliverable | Milestone | Dependencies | Scope |
|---|---|---|---|---|
| [A-T01](packets/A-T01.md) | Package skeleton, strict configuration and stage-aware CLI | M1 | G0 | required |
| [A-T02](packets/A-T02.md) | Synthetic generator and faithful keyed source ingest | M2 | A-T01 | required |
| [A-T03](packets/A-T03.md) | Study assembly and common preparation-state infrastructure | M2 | A-T02 | required |
| [A-T09](packets/A-T09.md) | Clinical dictionaries, typed observations and sample/visit linkage | M2 | A-T02 | required |
| [A-T04](packets/A-T04.md) | Proteomic profile and provenance-qualified missingness | M3 | A-T03 | required |
| [A-T05](packets/A-T05.md) | Metabolomic profile with explicit frozen normalization reference | M3 | A-T03 | required |
| [A-T06](packets/A-T06.md) | Lipid profile, molecular resolution and quantity-preserving derivation | M3 | A-T03 | required |
| [A-T07](packets/A-T07.md) | Fold-local preparation and held-out mutation invariants | M3 | A-T04, A-T05, A-T06 | required |
| [A-T08](packets/A-T08.md) | Complete A export, reproducible examples and profile extension handoff | M3 | A-T07, A-T09 | required |
| [B-T01](packets/B-T01.md) | Independent two-group limma inference and result envelope | M4 | A-T08 | required |
| [B-T02](packets/B-T02.md) | Multigroup contrasts, continuous terms and mean-retaining analyses | M4 | B-T01 | required |
| [B-T03](packets/B-T03.md) | Paired delta and difference-in-change analysis | M4 | B-T01 | required |
| [B-T04](packets/B-T04.md) | Qualified multivisit and trajectory fixed-subject inference | M4 | B-T02, B-T03 | required |
| [B-T05](packets/B-T05.md) | Count-dependent precision with DEqMS | M4 | B-T01, B-T03 | required |
| [B-T06](packets/B-T06.md) | Fixed testing families, nominations and rank sensitivity | M4 | B-T02, B-T03 | required |
| [B-T08](packets/B-T08.md) | Clinical molecular/score association service | M4 | A-T09, B-T01 | required |
| [B-T09](packets/B-T09.md) | Longitudinal clinical change and interaction associations | M4 | B-T08, B-T03 | required |
| [B-T07](packets/B-T07.md) | B CLI/export and Markdown reproduction handoff | M4 | B-T04, B-T05, B-T06, B-T08, B-T09 | required |
| [C-01](packets/C-01.md) | Common integration contract and concatenation PCA | M5 | B-T07 | required |
| [C-02](packets/C-02.md) | Supervised concatenation baseline and nested evaluation service | M5 | C-01, A-T07 | required |
| [C-03](packets/C-03.md) | Categorical DIABLO with explicit design and native prediction | M6 | C-02 | required |
| [C-04](packets/C-04.md) | Continuous block.spls adapter | M6 | C-02 | required |
| [C-05](packets/C-05.md) | MOFA2 training, reload and stability | M6 | C-02 | required |
| [C-08](packets/C-08.md) | Integration endpoint lineage and B-owned clinical handoff | M6 | C-03, C-04, C-05, B-T08, B-T09 | required |
| [C-09](packets/C-09.md) | Matched incremental clinical prediction for one declared endpoint | M6 | C-08, C-02 | conditional |
| [C-06](packets/C-06.md) | Cross-backend comparison and full D handoff | M6 | C-03, C-04, C-05, C-08 | required |
| [D-T01](packets/D-T01.md) | Evidence routing and full accounting with routes disabled | M7 | C-06 | required |
| [D-T02](packets/D-T02.md) | Local lipid mapping with explicit structural ambiguity | M7 | D-T01 | required |
| [D-T03](packets/D-T03.md) | Local gene-product and reaction mapping | M7 | D-T01 | required |
| [D-T04](packets/D-T04.md) | Declared-universe local over-representation analysis | M7 | D-T02, D-T03 | required |
| [D-T05](packets/D-T05.md) | Optional/required resource failure, status and completeness | M7 | D-T01, D-T02, D-T03, D-T04 | required |
| [D-T06](packets/D-T06.md) | Native MOFA enrichment under a qualified model/input contract | M7 | D-T04, D-T05, C-05 | conditional |
| [D-T07](packets/D-T07.md) | Conditional BioPAN quantitative lipid comparison | M7 | D-T02, D-T05 | conditional |
| [D-T10](packets/D-T10.md) | Target identity, request ledger and external assay evidence | M7 | D-T03 | required |
| [D-T11](packets/D-T11.md) | Named-target and alternative mechanism evidence profiles | M7 | D-T10 | required |
| [D-T12](packets/D-T12.md) | Bounded unranked exploratory target discovery | M7 | D-T11 | required |
| [D-T14](packets/D-T14.md) | One separately approved quantitative target method | M7 | D-T11, D-T12 | conditional |
| [D-T08](packets/D-T08.md) | Safe offline full-pipeline Quarto report | M8 | D-T05 | required |
| [D-T13](packets/D-T13.md) | Auditable target dossier report section | M8 | D-T08, D-T12 | required |
| [D-T15](packets/D-T15.md) | Clinical objectives and association reporting | M8 | D-T08, C-08 | required |
| [D-T09](packets/D-T09.md) | Reproducible paired and independent walkthroughs and final system check | M9 | D-T13, D-T15 | required |

Every card includes exact allowed/forbidden files, concrete acceptance, future test
commands, synthetic fixture, risk, repair budget and stop condition. All are high
risk for review prioritization because correctness affects scientific conclusions;
this is not a requirement for 41 separate Astra calls. Sol inspects every diff,
and Astra audits the nine milestone aggregates, plus an early risky shared-seam
review only when it cannot be covered before a dependent starts.

## Dependency changes from the historical graph

A-T03 now owns generic profile/state machinery, so A-T05 and A-T06 depend on it,
not on A-T04's proteomic policy. A-T04/05/06 have disjoint R/test/fixture files.
A-T09 can run beside A-T03 after A-T02. C-03/04/05 each depend on common C-02,
not on one another; continuous regression and MOFA do not depend on categorical
DIABLO code. Shared consensus helpers must be exposed through C-01/C-02 or handled
as an exclusive bounded upstream repair, not duplicated or edited concurrently.
C-06 explicitly waits for all three adapters and the score handoff. D-T04 waits
for both local mapping owners for the complete initial route set. D-T05 qualifies
core status after mapping/enrichment; conditional D-T06/07 are separate leaves.

B-T07 retains its complete B handoff. D-T08 remains milestone 8; final QMD vignettes
remain milestone 9. Required review gates are additional dependencies, not decorative
labels. A conditional scientific requirement actually needed by a consumer must
pass; marking it optional elsewhere cannot release that consumer.

## Legal waves before concurrency/resource limits

1. A-T01.
2. A-T02.
3. A-T03, A-T09.
4. A-T04, A-T05, A-T06.
5. A-T07.
6. A-T08.
7. B-T01.
8. B-T02, B-T03, B-T08.
9. B-T04, B-T05, B-T06, B-T09.
10. B-T07.
11. C-01.
12. C-02.
13. C-03, C-04, C-05.
14. C-08.
15. C-06, C-09.
16. D-T01.
17. D-T02, D-T03.
18. D-T04, D-T10.
19. D-T05, D-T11.
20. D-T06, D-T07, D-T08, D-T12.
21. D-T13, D-T14, D-T15.
22. D-T09.

These are graph-ready candidate sets, **not permission to start a later milestone
without its previous review gate**. The scheduler selects ready packets within
the accepted gate frontier, max 3 Luna / 2 Terra / 4 total; fewer if the host is
smaller. Code writing can be parallel while expensive training jobs use a single
compute lease by default. Do not spawn idle agents to wait for dependencies.

## Milestone and documentation review

G0 = actual Astra low spec audit + checked hash freeze. M1–M9 each produce one
immutable integrated Git snapshot, command/test evidence, Terra milestone note
and one fresh-context Astra low audit. Terra can draft from the contract while
coding proceeds, but may finalize only from actual packet receipts/dispositions.
No same-worktree concurrent edits; Sol cherry-picks one worker commit at a time.
A worker owns only its card's paths; shared package/CLI/lock integration is serial.
No independent worker regenerates NAMESPACE/man files across the whole package.

## Execution loop

Restore actual Git/ledger -> select ready disjoint packet -> failing behavior test
-> minimal bounded implementation -> actual named tests/CLI slice -> compact
receipt -> Sol diff/test verification and serial integration -> Terra final note
-> Astra milestone audit -> targeted correction if needed -> accepted gate.
No agents mark their own work accepted. A previous accepted packet is not reopened
for speculative improvements; a demonstrated regression creates an explicit scoped
repair and invalidates affected acceptance until rechecked.

## Acceptance and closure

Each required packet needs substantive executed tests and actual outputs. Required
runtime scientific qualification is not satisfiable by a mocked stub. Conditional
packets remain separate blocked/deferred/accepted records; milestone core acceptance
may proceed with an explicit conditional disposition when no consumer requires it,
but does not label that capability complete. Full-programme LOOP CLOSED requires
all retained required scope accepted, plus explicit user-accepted disposition of
any retained conditional scope, successful final checks and no owned workers left.
Otherwise close with LOOP OPEN, precise remaining owners/evidence and a resumable
handover, then stop. Do not erase scientific scope to obtain a green dashboard.
