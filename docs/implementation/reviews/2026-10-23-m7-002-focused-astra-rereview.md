# M7-002 focused Astra-low rereview

- Date: 2026-10-23
- Reviewer: `/root/m7_002_astra_audit`
- Role: fresh-context, read-only Astra-low amendment auditor
- Immutable candidate: `366f284ba81bc595faa42f2017f00aa754ff8c6c`
- Verdict: `changes_required`
- Findings: Critical 0, Important 1, Minor 0
- Implementation authorization: **no**

## Focused result

The repair closed the pointer-publication finding: atomic rename is the
linearization point, and ambiguous acknowledgement is handled by validating
whichever complete generation the pointer names. It also closed the ownership
finding: Luna may return only a D-owned registration request, while Sol alone
performs shared registration.

One Important closed-contract finding remains:

1. `ScientificEntityDescriptor` does not declare `block_id` or `method_id`, but
   the required cardinalities group descriptors by those fields. Adding them
   violates the declared closed shape; omitting them makes the cardinalities
   uncheckable.
2. The role registry has no closed `entity_type` registry or role-to-type
   mapping, although the verifier is required to reject role/type disagreement.
3. The lineage rules do not explicitly exclude the Stage Handoff Provenance
   payload or envelope from its own produced-handoff inventory, leaving a
   self-reference ambiguity.

These points are evidenced at lines 86-113, 129-159, and 186 of the immutable
candidate. The publication repair is evidenced at lines 304-324 and 354-360;
the ownership repair is evidenced at lines 388-398.

## Disposition

The initial amendment audit and its single permitted documentation repair and
focused rereview are consumed. Under M7-002 sections 8 and Current disposition,
the remaining Important finding blocks this change record. M7-002 is not
accepted, no implementation phase is authorized, D-T01 remains unaccepted, and
M7 plus every Stage-D successor remain closed. A future attempt requires Omar's
explicit authorization of a new bounded change record; it cannot be presented
as another repair of M7-002.

The reviewer edited no files and accessed no private study data.
