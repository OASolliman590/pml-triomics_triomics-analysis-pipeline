# Tri-omics progress

**D-T08 is accepted on synthetic software evidence.** The goal remains to finish the target dossier, clinical objectives and installed-pipeline walkthroughs, then release G8 and G9. M7/G7 remain the accepted annotation baseline.

The pipeline now renders a real offline Quarto report from saved A, B, C and D results. The report preserves effect units, uncertainty, counts and source hashes. It distinguishes completed evaluation from unavailable single-fit evidence, and keeps unavailable routes visible. B can publish its registry without replacing A's record.

Independent verification passed 1,640 assertions with one expected rejection warning and no failures or skips. A fresh synthetic A→B→C→D→report run passed, followed by 1,729 saved-output checks. A fresh local Sol review found no blocker. This establishes software behavior on fixtures, not biological or clinical performance in the three studies.

D-T13, the target dossier, is now being implemented in one isolated Luna worker; D-T15 follows. M8 still needs its documentation, clean-archive package check and Astra review before G8. D-T09 and M9 follow later. Conditional D-T06/D-T07/D-T14 retain zero credit; C-07 remains unrun, C-09 unqualified and the frozen-kit audit pending. Needs Omar: none. Sync: local only; no private-study analysis, push or Notion sync.

| Outcome | Evidence state |
|---|---|
| B/C provenance and required M7 annotation/targets | Accepted on synthetic product fabd6fa |
| D-T08 offline report | Accepted on synthetic product f85fa6d |
| D-T13 target dossier | Initial implementation running; not yet accepted |
| D-T15 clinical objectives | Not started; follows dossier |
| D-T09 installed walkthroughs | Not started; awaits G8 |
| Conditional scientific routes | Blocked or unqualified; no completion credit |

Receipt: [D-T08 acceptance](implementation/reviews/2026-11-04-d-t08-sol-acceptance.md). Literal resume: join the existing D-T13 Luna worker, then review and verify its dossier; do not rerun D-T08 or reopen M7.
