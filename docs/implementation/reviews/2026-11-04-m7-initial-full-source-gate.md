# Initial final M7 source gate — failed

Immutable48ca0c4ad50c6d24001d91d849d718eb6f8dbc6c, clean checkout, one uninterrupted
full source run:2026-11-04 00:43:21–01:09:54UTC. Exit1; PASS7280 FAIL3 ERROR1
WARN15 SKIP0,491blocks. All conditions and per-file counts retained in
../evidence/2026-11-04-m7-final-initial/source-receipt.json. No assertion was
suppressed or skipped. Exact command is recorded there; complete raw log remains
/tmp/triomics-stage-d-evidence/final/source/full-source.log.

Two failing test names account for all failures/errors:
- CLI startup routes qualified absolute-path DEqMS and explicit fallback
  (test-B-T05.R): both expectedexit0 commands return2 B_EXPORT_INCOMPLETE.
  b05_cli_config writes temporary YAML under the package examples/tickets source
  directory, making the executing checkout dirty. The source-revision guard is
  correct. Do not weaken it, fake working_ref, or accept partial B results.
- the synthetic target keeps its profile and other targets are skipped
  (test-protein-metabolite.R): one failed length assertion and subsequent indexing
  error. It expects the retired SYNTH_FCN3_LIKE identifier shortcut to invent a
  profile without prepared evidence. The approved general target route requires
  explicit declarations. Replace this obsolete fixture with positive supplied
  declarations and negative no-fabrication cases, preserving scientific guards.

Warnings are named/disposed:11 from C-05 real MOFA2 fit preserves scattered
missingness and reports seed sensitivity and enrichment hashes;3 from C-05 real
MOFA2 fits and extracts a missing sample-view capability cell;1 from D-T01 refuses
stale paired signs and preserves the old pointer (expected exit2). They are native
small-fixture diagnostics and expected rejection, not these four failed conditions.

The932-file git archive and exported stamp match48ca0c4; tar SHA256
7e4c6706c30153ea422f0e3c7d1da5416fb1c9bf62e75d614df3a9b6c6e15a9a.
Package gate, final native8packet runs, Terra DOC-M7 and final milestone Astra did
not run. No acceptance or G7 release. New bounded M7-028 scope under standing user
authorization addresses only the two test fixtures; prior verified product cycles
are not reopened and this failed source receipt remains historical evidence.
