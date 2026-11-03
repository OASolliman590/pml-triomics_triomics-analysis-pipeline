# M7-025 discovery verified and integrated

Sol reviewed immutable d80d23d04e5102c67cc01c15ff3a250f608a90e7 and integrated
shared cc4f3ba, initial a1df98a and repair1 d80d23d as 4e02a9f/1d78c35/94b2b91.
R/tests/inst/man/NAMESPACE/DESCRIPTION trees match tested candidate. One repair
used; one remained unused. Initial Astra no; one focused code rereview yes.

Clean candidate D12/shared PASS44, FAIL0, ERROR0, WARN0, SKIP0 (10 blocks).
D10/D11/D01 PASS652, FAIL0, ERROR0, WARN1, SKIP0 (65 blocks). Warning test:
D-T01 refuses stale paired signs and preserves the old pointer: expected native
status2. No new warning. Commands used external run-focused.R with filters
D-T12|D-discovery-shared and D-T10|D-T11|D-T01 respectively. The first regression
wrapper lost its returned process handle; no duplicate was launched, and its
complete durable JSON records clean-commit counts. Earlier repair1-focused.json
was precommit evidence at dirty a1df98a, NOT clean a1df98a acceptance; the clean
d80d23d rerun supersedes that ambiguity without rewriting history.

Native ticket inst/examples/tickets/D-T12.yaml: B,C,D each exit0 at fresh root
/tmp/triomics-stage-d-evidence/m7-025/native/D-T12. Sol independent readback
passed107 assertions: full same-root B/C/D predecessor and generation hashes,
member sizes, 3 universe members (2 included/1 excluded), original dispositions,
actual B effects/C loadings/source hashes/feature identities/dependency groups,
original external unavailable affinities and no selected winner. Both published
profile copies agree. Unequal annotation coverage produces noncomparable
alternatives, no ranking/winner. Exact commands and full hashes are in evidence.

D generation SHA256:
a2ea7befa6a65cca46a2a93d22e779be25fdc424b5d6249ad28b5d75b113d16f
Universe SHA256: ca64a78b01d037d0567978b462fa2d9dbc83c4c9496091cd838a7c8d8585db99
Protocol SHA256: 39f649395952c77d93fee11c715e70cc6ec315816e4e67ac8393ccb1284aa5a5

Receipt set: ../evidence/2026-11-04-m7-025/. Public synthetic fixtures only;
no biological validation, private data or push. D12 accepted remains false.
M7-026 dependency verified; final same-snapshot gates remain. M7/G7 closed.
