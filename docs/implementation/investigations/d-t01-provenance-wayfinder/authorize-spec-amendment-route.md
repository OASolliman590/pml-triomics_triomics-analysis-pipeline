# Define the spec amendment and acceptance route

Label: `wayfinder:task`  
Parent: [Scientific provenance boundary for Stage D](MAP.md)  
Status: closed
Assignee: `/root`

## Question

Which frozen spec-v2 contracts and acceptance gates must change, what exact
change record and checksum review are required, and what evidence must pass
before implementation can start? Produce the bounded authorization text for a
future implementation cycle; do not implement it in this map.

## Resolution

[M7-002 — Stage Handoff Provenance and atomic generation publication](../../change-records/M7-002-stage-handoff-provenance-amendment.md)
is the proposed normative change record. It layers on the approved frozen hash
without rewriting historical freeze records, preserves all 41 packet IDs, and
reopens only the B-T07/C-06 public handoff seams plus unaccepted D-T01.

The initial read-only Astra-low audit returned three Important findings. One
documentation repair closed two findings, but the single focused rereview still
returned `changes_required` with one Important closed-contract defect. That
repair and rereview allowance is exhausted. M7-002 is blocked, implementation
is prohibited, and Stage-D successors remain closed. A future attempt requires
Omar's explicit authorization of a new bounded change record.
