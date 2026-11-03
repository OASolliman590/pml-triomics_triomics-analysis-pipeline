# M7-027 target comparison — Sol verification

Candidate **eed7cb251c229399de0b5c110f516c4013197314**, clean source in
/tmp/triomics-m7-027-luna. Scope11085b1 passed fresh Astra. Preserved M7-024
candidate6e73239 remains a blocked historical cycle, not retrospectively accepted.
M7-027 initialf9abc42 used one combined repaireed7cb2; one repair remains.
Initial implementation Astra no and its exact two findings are in the 3November
implementation review. Its single focused rereview is pending at this receipt.

Sol inspected the bounded source diff. The assay guard reconstructs the typed
ledger/profile against the actual request before treating experimental labels as
external evidence. The guard copy retains attributes and arbitrary metadata;
original external values remain unchanged. Per-axis coverage includes unavailable
and unresolved records and fails closed instead of falling back to one measured
flag. No statistical method, provenance schema or numerical threshold changed.

Sol independently ran on committed eed7cb2:
`Rscript --vanilla /tmp/triomics-stage-d-evidence/run-focused.R /tmp/triomics-m7-027-luna 'D-T10|D-T11|target-shared' /tmp/triomics-stage-d-evidence/m7-027/sol-focused.json`
Exit0; **PASS359 FAIL0 ERROR0 WARN0 SKIP0**,54blocks. This replaces the worker's
precommit test attribution for acceptance evidence. Both appended fixture tests
ran. Independent no-fit probes also confirm Kd/Ki/function-activity records attach
and compare; this does not convert any one experimental endpoint into another.

Worker ran both exact public D-T10.yaml and D-T11.yaml commands through native
B, C, then D on fresh roots; all six exits0. Full commands/commit/artifact hashes
are in ../evidence/2026-11-04-m7-027/native-receipt.json. Sol separately read the
persisted artifacts with /tmp/triomics-stage-d-evidence/m7-027/sol-target-readback.py:
exit0, **53 D10 +91 D11 =144 assertions**. This was independent read-back of worker
outputs, not a claim that Sol reran all six producers. All four B predecessor
hashes match C; D manifest/member hashes and byte sizes match publication. Profiles
retain real B effects/units and C loadings/selection, original measured features,
source-file hashes and shared dependency groups. Repeated B/C observations do not
add independent confirmation. The fixture has no measured affinity: value:null
remains unavailable and external, exact target matches stay distinct from target
mismatches, four evidence axes remain separate, and no winning target is selected.

D generation SHA256:
- D10:347c84a1d32fb1e5d9869a5345a4ac40ac43d479ef97d51d0baaf406fb43f75c
- D11:71109a0e8b6188388b31232e8c795f76cb5381244c15f5e99964c7038ead5316
Consumed mechanism SHA256:93434242b0f8ab69023cc66e27f98f56797c5840026b23040a9f27bb750b79fa
Consumed assays SHA256:a59bc918cdb17d96d1c7c4d958f6da51087da580e792c342e8c1b8d221f18804
Every predecessor/member hash is retained in the adjacent receipts.

These are public synthetic software proofs only. D12 still waits for the focused
Astra pass and Sol integration. Final same-snapshot packet/full-source/archive,
Terra documentation and fresh milestone Astra gates remain mandatory. C06 is
unaccepted, C07 not_run, C09 unqualified; conditional D06/07/14 have no completion
credit. M7/G7 stay closed. No private data, push, AIU or fasting run.
