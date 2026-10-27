# M7-016 initial candidate review and repair1

Candidateac98db3191bf8a5854144f5e017329038e9170e4, not integrated.
Fresh Sol-high `/root/m7_016_sol_review` requestedgpt-6.1-sol/high, fork none;
settings accepted by host, separate effective runtime not exposed. Spec:
Critical0 Important4 Minor0; quality: Critical0 Important3 Minor0. First three
findings overlap across axes; four distinct findings total. Full review and
reproducer scripts/logs are in evidence/2026-10-27-m7-016-review/.

1. C native-state symlink ancestry escapes the same-root boundary.
2. Exported routing API accepts missing producer proofs.
3. Canonically resealed generation readback accepts conflicting member types
   and predecessor provenance descriptors.
4. Required native D scientific-lineage/path/ambiguity probes are incomplete.

Parent Sol reran both reviewer scripts on public synthetic copies: unsafe
native-state reader/executor returned evidence_bundle/complete; proof-less
exported API returned complete; conflicting resealed generation returned complete.
These are counterexamples, not passing gates. No private study inputs were used.

The independent affected suite atac98db3 was stopped after these blockers;
owned R PID22217 and descendants23744/23745 received SIGTERM, session49026
completed exit143. Partial log retained. No aggregate counts/GREEN claimed and
no unrelated process stopped. No code edits happened during that run.

Same Luna `/root/m7_016_luna` received repair1 of at most2 for all four findings,
within the unchanged ten-file allowlist. Native producers/schema/methods and
numerical policy remain untouched. Review and independent gates must rerun on
the repaired immutable candidate. C06unaccepted, C07not_run, C09unqualified;
M7/G7closed, downstream D closed. Full/archive/final Astra gates not yet run.
