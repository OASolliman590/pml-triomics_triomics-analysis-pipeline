# M7-026 — truthful generation wording before milestone acceptance

Frozen design scope only under Omar's authorization to reconcile the records and
finish required Stage D. No implementation until D12 and all earlier required
components verify, and this scope passes Astra. This is a new bounded cycle,
not reopening an exhausted historical repair. M7/G7 remain closed.

R/annotation_router.R currently publishes and requires the literal limitation
`C-06 remains unaccepted.` Every future generation would keep that assertion even
after Sol accepts the current C06 revision. A runtime generation is not an
acceptance receipt, and it must not decide or permanently misstate milestone
acceptance. Current C06 remains unaccepted while final gates are pending.

New generations must use the neutral statement:
`This publication is not a C-06, M7, or G7 acceptance receipt.`
The generation validator must require either this exact statement or the exact
historical C06-unaccepted statement, together with the unchanged required C07
not_run and C09 unqualified/conditional limitations. Reject missing, unsupported
or acceptance-claim substitutes. Preserve historical immutable artifacts; never
rewrite their text or hashes. Recognizing historical wording alone grants no
legacy-evidence eligibility: all existing identity, immutable producer lineage,
source/predecessor, canonical-byte, full hash, member-inventory and envelope
checks remain mandatory and unchanged. No schema fields, profile versions or
canonicalization rules change.

Only Sol's separate receipt can accept the tested/audited immutable product
snapshot after the required packet, source/archive and fresh Astra gates. No
registration is accepted, no M7/G7 release occurs and no conditional work receives
completion credit because this wording patch or its tests pass.

Fresh Luna-max allowlist: R/annotation_router.R limited to the emitted governance
statement and its validation predicate; tests/testthat/test-D-T01.R limited to
matching generated-message assertions and additive old/new/missing/false-claim
validation regressions. No other source, schema, tests, numerical expectations,
frozen kit, B-T04 bound/seeds, C07 or C09 changes. Sol owns ledger, registration,
handover and final acceptance documents; Luna must not edit them.

Behavioral RED first, then preserve all source/provenance/tamper assertions. Show
new native publication carries the neutral statement, historical wording remains
structurally recognized without bypassing byte/lineage validation, and missing
C07/C09 qualifications or false acceptance claims fail. Run D01, B07 and C06
regressions from clean committed source with named warnings and no skipped required
checks. Sol independently reviews the two-file diff and native readback before
integration. Initial plus two targeted repairs; one scope audit plus one focused
rereview. Final uninterrupted source suite, clean git-archive Status: OK and fresh
Astra review must follow this product change before Sol accepts M7/G7.
No private study data, push, AIU, fasting run or later report work.
