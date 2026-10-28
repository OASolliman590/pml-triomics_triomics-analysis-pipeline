# M7-018 immutable integrated snapshot audit

Snapshot d660879ab7d6dbf4e652aa8957452c02ec5e1cdb, clean gitarchive
/tmp/triomics-m7-018/archive, tarSHAf5ad40dd46fd22d0495f88971179856ee610707e5328a3f93dbf77d797db03d3.
Product/test blobs match repaired candidate774bbcc5f7ad3152e6daf1d57d1972cf99519963.
Parent verified fresh native B/C/D0/0/0,21pass, explicit persisted SYN_12 time:null,
no excluded score/training row, B/C/D hashes and four B predecessor bindings.
Focused1176pass0fail0error1expectedwarning0skip/74blocks. Terra6.1-high RED on
6456055 found19pass2fail, reviewed one-line C NULL fix with no scoped findings.

Full source ran once uninterrupted:6280pass1fail15warn0error0skip/370blocks.
Failure: test-C-08.R, C-08 preserves mean and delta AnalysisUnitMap lineage and
rejects incompatible times: delta_endpoint$samples[[1]]$visit_weights is list,
expected named numeric c(V0=-1,V1=1). Introduced in prior c8b309e (integrated
4c421a4), R/integration_endpoints.R232-236 coerces delta weights to named list
for JSON sign names. Null fix changes onlyR/cli.R700; no opportunistic test edit.
Package pretest diagnostics: WARNING undocumented provenance_context in B export
Rd usage; NOTE unqualified setNames in provenance validator. Final package result
will be supplied with dispatch. Do not infer Status:OK.

Audit this combined provenance phase, not full canonical M7 acceptance. Read
accepted M7-002/M7-003 and bounded M7-016/M7-017/M7-018 records, relevant diff,
full/focused/native evidence and known failure. Check native predecessor closure,
canonical/value-vs-byte semantics, unit/representation/time/exclusion identities,
D pointer-last publication and failure/recovery, enabled/disabled routes and
limits. Verify null fix and preserve existing numeric API and JSON lineage needs.
C06false,C07fourcontrolsnot_run,C09unqualified,M7/G7/laterDclosed regardless verdict.
No private data/push. No biological-validation claim. Historical spec validator
fails AGENTS/CLAUDE operational-doc hash drift fromb585b26; other90match; no new
frozen file change or hash relaxation. Disclose, do not repair.

Fresh Astra-low read-only, no children/edits/acceptance. Report audit_pass yes/no,
distinct severity counts, exactsnapshot, concrete findings and evidence limits.
No implementation or rerunning heavy suites. Parent owns final disposition.

Final package result: Status: 1 ERROR, 1 WARNING, 1 NOTE. Installed testthat FAIL45 WARN42 SKIP0 PASS5266. The clean archive lacks .git; many native producer tests reject absent repository revision. Others have installed fixture path errors; C08 list-vs-numeric failure persists. Full transcript /tmp/triomics-m7-018/package/testthat.Rout.fail and 00check.log. Installed CheckReporter reports42 warnings but does not enumerate their test names; do not infer these are the previously named source warnings. Warning inventory incomplete, another acceptance blocker. Source15 warning names are in /tmp/triomics-m7-018/warning-inventory.md. No further repair authorized in this cycle.
