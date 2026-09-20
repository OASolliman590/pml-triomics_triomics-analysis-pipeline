# Implementation tickets: canonical v2 routing

All **41 historical ticket IDs** are preserved in
[../spec-v2/packets.json](../spec-v2/packets.json). The canonical cards now live in
[../spec-v2/packets/](../spec-v2/packets/). The
[implementation plan](../spec-v2/IMPLEMENTATION_PLAN.md) defines current dependency
and milestone review gates, ownership, acceptance, conditional scope and parallel
execution. **C-07 is an evaluation-evidence requirement, not an extra ticket.**

The older cards in this directory remain a historical crosswalk. Their fleet
assignments, shared-file lists, superseded blockers and artificial serial edges
must not be used to dispatch v2 work. Follow root AGENTS.md: Sol high, multiple
Luna max coding workers, Terra max documentation workers, Astra low milestone
reviews. No worker is permitted to edit the canonical specification unilaterally.
