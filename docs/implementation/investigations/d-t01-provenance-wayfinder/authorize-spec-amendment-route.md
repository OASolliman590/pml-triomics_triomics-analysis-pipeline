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

Implementation remains prohibited until one fresh read-only Astra-low audit of
the immutable candidate returns `pass`. A pass lets Sol accept the change record
and begin its bounded three-phase implementation route; changes required permit
one documentation repair and one focused rereview only. Stage-D successors remain
closed until D-T01 and M7 are accepted.
