# Fresh Astra-low audit — M7-018

Snapshot: d660879ab7d6dbf4e652aa8957452c02ec5e1cdb
Requested gpt-6-astra / low accepted by native delegation; effective runtime settings not independently exposed.
audit_pass: no. P0: 0; P1: 1; P2: 2; P3: 1.

P1: Clean installed-package gate fails. R/export_b.R:1193,2151,2799 require a repository revision obtained through Git; the archive lacks .git. Native exports fail B_EXPORT_INCOMPLETE. Installed fixture-path failures also occur in test-B-T01.R:615,698,772. Status: 1 ERROR, 1 WARNING, 1 NOTE; FAIL45 WARN42 PASS5266 SKIP0.

P2: Delta numeric API regressed. R/integration_endpoints.R:232–236 converts named numeric visit weights to a named list. test-C-08.R:716 still requires numeric c(V0=-1,V1=1). Source: PASS6280 FAIL1 WARN15 ERROR0 SKIP0. JSON names do not justify changing the established in-memory API.

P2: Installed warning inventory incomplete. CheckReporter reports42 without full test-name inventory. The15 source warning names cannot establish disposition of the installed set.

P3: man/triomics-api.Rd omits provenance_context from B export usage; R/provenance_contract.R:431–432,529–530 uses unqualified setNames. Exact Status: OK is unmet.

The narrow R/cli.R:700 null repair is consistent with scope. Native evidence B/C/D0/0/0,21 assertions and explicit null; focused1176pass0fail0error0skip with1named expected warning. Inspected predecessor validation and pointer-last publication/recovery code yielded no additional concrete finding.

Limits: read-only inspection, no reruns or independent artifact rehashing. Candidate774bbcc and integrated product/test equality are coordinator-attested. Historical AGENTS/CLAUDE frozen-hash drift neither repaired nor waived. No biological validity established. C06unaccepted,C07not_run,C09unqualified,M7/G7/laterDclosed.

Recorded by Sol from /root/m7_018_astra_final final response. Astra edited no files and did not accept M7.
