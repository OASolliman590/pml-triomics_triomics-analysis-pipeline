# M7-021 — native mapping verified for downstream ORA

Sol reviewed candidate `f173f38b79b74ae048dad55fc5566b7e53e28eaa` and its four
commits c82a7e8,58e1009,0588b42,f173f38 against base4e8c1c2. All14 changed files
are allowlisted. Product change: ticket-relative local resource paths resolve
against the ticket directory while preserving the leaf path for symlink rejection.
No numerical/statistical/schema changes or resource-service changes. The fixture
and tests exercise real B/C nominations, not invented predecessor artifacts.

Scope Astra passed4e8c1c2. Sol's early diff review caught path normalization that
would erase a resource symlink; final0588b42 preserves the path and its regression
rejects the link. Warning suppression was removed before final tests. Initial
worker attempt completed; no returned parent repair cycle used. SSD interruption
was recovered with original Git metadata and the same worker; its failed run is
not counted as GREEN.

Worker atf173f38: D-T02 **87**, D-T03 **62**, shared foundation **80** passes;
all failure/error/warning/skip counts zero. Sol independently ran:
`Rscript --vanilla /tmp/triomics-stage-d-evidence/run-focused.R /tmp/triomics-m7-021-luna 'D-T02|D-T03|D-foundation-shared' /tmp/triomics-stage-d-evidence/m7-021/sol-focused.json`
Exit **0**, **PASS229 FAIL0 ERROR0 WARN0 SKIP0**,21 test blocks. Clean source before
and after. No new warning disposition needed.

Sol independently ran native packet proof for D-T02.yaml and D-T03.yaml on fresh
persistent roots. Each exact public command was `Rscript --vanilla inst/exec/triomics
--config inst/examples/tickets/<packet>.yaml --stage <B|C|D> --output <fresh-root>`.
All six exits **0**. Full commands, paths and SHA256s are stored in
../evidence/2026-11-03-m7-021/mapping-native-receipt.json.
Each publication has3 measured observations and3 mapping edges, preserves
one-to-many ambiguity, retains unmapped rows without multiplying observations,
and records offline resources with no network or study upload. D03 preserves
both B and C nominations for one measured gene without independent-confirmation
credit. Lipid structure stays at sum composition; no invented chains/isomers.

Independent read-back matched all four B predecessor bytes inside C and verified
D manifest, members and sizes, predecessor profile/envelope hashes and resource
content hash. D generation hashes:
- D-T02: f4242ac3dc0b1830e0bda6ab26f77ea64320191b8f1bc798eceb9d7b644ff85e
- D-T03: 4cce634f1f3c2f6732f0634de7b9c2211d4bf82f43912ec99383f3ee564a8108

Disposition: integration and downstream D04 implementation are permitted after
this read-back. This establishes synthetic software behavior only. Registrations
remain unaccepted pending final same-snapshot packet/milestone receipt reconciliation.
M7/G7 closed. D04 scope already has independent Astra pass ate59b515. D05 follows
D04; later target/discovery work remains. C07 not_run, C09 unqualified, conditional
D06/07/14 no completion credit. No private study data, push, AIU or fasting run.
