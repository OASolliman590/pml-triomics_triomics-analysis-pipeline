# M7-022 implementation audit — changes required

Fresh read-only /root/m7_022_implementation_astra reviewed immutable
225529ab1cc9cd022224ed171a641b0590ddebe0 against c78555a. Requested model
gpt-6-astra, effort low; effective settings were not independently exposed.
Initial implementation audit: **audit_pass: no**. No edits, fits or tests run.

P1: R/annotation_apply.R protocol validation uses setequal at protocol, claim
and set object boundaries without rejecting duplicate keys. Sol's no-fit probe
accepted duplicate protocol_id, block_id and members declarations. Exact frozen
protocol meaning cannot depend on the first duplicate key selected by R.
The same boundary recursively flattens/coerces members via
triomics_ora_characters: named objects, nested arrays and numeric/boolean values
can become entity IDs. Validate original JSON shapes and types before normalizing.

Repair requirements: unique object names at every protocol object; scalar string
identities; arrays of claim/set objects; flat string member arrays with explicit
empty-array support. Test conflicting duplicates at all three levels, malformed
members/identities, positive empty sets, independent claim families and native
invalid-protocol failure preserving the current-D pointer. Existing hypergeometric
and BH behavior remains unchanged. No additional material blocker identified in
background reconstruction, inventory qualification, unique mapping, family
separation or SHA256 bindings. This is a bounded implementation review, not
scientific or milestone acceptance.

Reported focused evidence was102pass/0fail/0warn/0skip; D01/02/03/foundation
regressions were pending during audit. Sol sent one combined repair2 to the same
Luna, to begin after those regressions finish. This exhausts M7-022's two-repair
allowance; one focused Astra rereview remains. M7/G7 and D05 release remain closed.
