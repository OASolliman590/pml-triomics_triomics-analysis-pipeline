# Spec kit progress, 19 October 2026

This note updates the 18 October re-evaluation. It does not change a frozen
scientific requirement, the approved content hash, or `packets.json`. Live
packet status stays in `docs/implementation/execution-ledger.json`. The status
field inside `discovery/spec-v2/packets.json` is the candidate-time field and
remains part of the frozen hash.

Head reviewed: `2f4e8e4` on `spec/harden-v2-20260920`. The worktree also has
uncommitted C-02 registration edits. Those edits are not accepted.

## What is accepted

G0 through G4 are accepted. All nine Stage A packets and all nine Stage B
packets are accepted. C-01 is accepted on synthetic software evidence: 109
focused expectations, stage C CLI exit 0, and a clean package check. Only
concatenation PCA is enabled.

B-T04's production route is accepted. Its ordinary 23/400 one-cell diagnostic
remains a disclosed miss against the 0.08 upper bound. The bound was not
relaxed.

## What is not accepted

C-02 has three commits (`a3a1cfd`, `249e2d0`, `2f4e8e4`) and is
`final_repair2_active`. The 18 October review found eight Important findings.
Later commits repair some of them. Acceptance is not claimed. G5 is not
released. C-03, C-04, C-05, C-06, C-08, and conditional C-09 stay behind that
gate. Every D packet stays behind C-06 and G6.

## Requirement coverage

The 18 October count was 7 verified, 15 partial, and 7 missing. C-01 acceptance
moves these rows and no others:

| ID | 18 October | 19 October | Why |
|---|---|---|---|
| R08 | partial | verified | C-01 is now accepted with the assigned B packets. |
| R14 | missing | partial | The common integration result exists for concatenation PCA only. |
| R15 | missing | partial | Concatenation PCA is accepted. DIABLO, MOFA, and the other backends are not. |

Coverage is now 8 verified, 15 partial, and 6 missing. This is synthetic
software coverage, not a biological result.

## How the Matt Pocock skills fit this repo

The skills assume an issue tracker, triage labels, and a glossary. This repo
already has a stricter tracker: 41 packet cards, a frozen contract, and the
implementation ledger. Use the skills on top of that tracker.

| Skill | Use here | Do not use it to |
|---|---|---|
| `implement` / `tdd` | Build the next open packet from its card, red tests first. | Reopen an accepted packet or loosen a calibration bound. |
| `implement-spec` | Run the ready frontier in parallel worktrees. | Start C-03 or any D packet before its review gate. |
| `code-review` | Standards plus spec review after a packet diff. | Replace the Astra milestone review. |
| `diagnosing-bugs` | A packet test that fails for a reason the bound does not name. | Rerun B-T04 seeds until the 0.08 bound passes. |
| `handoff` | Pass a worktree and the exact failing command to the next session. | Summarize away the ledger. |
| `domain-modeling` | Only if a new code name disagrees with `SCIENCE_CONTRACT.md`. | Add a second glossary. The 18 October review found the domain terms sound. |
| `to-spec` / `to-tickets` | Not needed for this build. The spec and 41 tickets already exist. | Create a parallel ticket set. |
| `wayfinder` | Not needed. The route from C-02 through M9 is already in the implementation plan. | Rediscover the pipeline. |

The engineering-skill setup wants an `## Agent skills` block in `AGENTS.md`.
That file is inside the frozen hash, so this note records the tracker choice
instead: issues are the packet cards plus `docs/implementation/execution-ledger.json`.
GitHub Issues are not the tracker. The public `main` branch is still only
`c8f0eec`.
