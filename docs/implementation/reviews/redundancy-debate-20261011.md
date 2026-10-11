# Redundancy debate before freeze — 2026-10-11

Workflow: `triomics-redundancy-debate`.
Role: Astra low, read-only. Requested label `gpt-6-astra` is not available on this host.
Effective model for all four scanners: `grok-4.7`. No scanner edited the kit.

Reviewed content hash: `f0d482db62f16c8e4c802c99c1fd88e501f29959bc475d67f934b40c6f6ae5d2`.
`git show HEAD:<path>` matches every frozen content file. The committed kit is the candidate.

## Scanner results

All four scanners returned `findings: []` after reading the named contracts. The debate phase did not open, because there was no finding to argue.

- Authority: live dispatch is spec-v2. `discovery/tickets/README.md` keeps the old cards as a historical crosswalk. `discovery/spec-drafts/B-design-statistics.md` still says it is not implementation-ready. The old B-T08 card names a retired lane and filter; the live packet does not.
- Clinical seam: B-T08 fits the continuous association, B-T09 fits the longitudinal and interaction estimands, C-08 hands immutable endpoints to that B fit, C-09 is a conditional prediction comparison, and D-T15 reports. Write sets do not overlap.
- Targets and resources: `R/resource_manifest.R` stays with D-T01. D-T04 owns declared-universe over-representation. D-T06 and D-T07 stay conditional. D-T14 is the only quantitative-target packet and stays conditional.
- Acceptance and paths: V01 and A-T01-AC1/AC3 agree. All 41 `files_allowed` lists are disjoint. The nine documentation packets own only `docs/implementation/milestones/M1.md` through `M9.md`.

Sol checked the same path claim independently: 41 packets, 0 shared write paths, 0 overlap with documentation allow-lists.

## Disposition

No P0 or P1 redundancy. No spec edit. No new content hash.
Historical tickets and drafts stay as provenance.
G0 remains the accepted hash above. Implementation may continue from that freeze.

Working-tree edits to `AGENTS.md`, `CLAUDE.md`, and `.gitignore` are uncommitted and are not part of this hash. The spec validator fails on the dirty tree for that reason and passes against `HEAD`.
