# M8 documentation, package gate and independent review

User authorizes continuing M8 from `4cba2581d4e70558981b8a02efcb4653cc9a5b01`. D-T08, D-T13 and D-T15 are accepted; G7 is released. Preserve their candidates, receipts and all attempts. Do not reopen or redispatch D13. No product implementation or private-study work is included.

One fresh Terra (`gpt-6.1-terra`, max requested) owns only `docs/implementation/milestones/M8.md`, under DOC-M8's one documentation-repair limit. Sol reviews and integrates its local commit, then freezes an immutable candidate containing the note and all three accepted packet records. Effective model settings are recorded only if the host exposes them.

Run `git archive` of that candidate into a new internal-APFS directory. Run the candidate's `Rscript --vanilla tools/check-package.R --root <archive> --output <new-logs>` with the existing isolated MOFA runtime and Quarto available. No product edits during the check. Require exact `Status: OK` in `00check.log`, captured testthat counts, no skipped required tests, every warning named and dispositioned, and unchanged archive bytes. The package check naturally includes D13 among the full installed tests; no separate D13 repair or packet rerun is scheduled.

Current qualified package warning baseline is 15: eleven and three diagnostics under the two named C-05 native MOFA tests, and one intentional CLI status-2 warning under `D-T01 refuses stale paired signs and preserves the old pointer`. Compare names, multiplicities and diagnostic categories; do not substitute historical M6's 22-warning inventory or suppress warnings.

After the gate passes, one fresh read-only Astra (`gpt-6-astra`, low requested) reviews that same immutable snapshot, DOC-M8, the bounded M8 product diff, original packet acceptance evidence, and the new package evidence. M8 permits one initial review and at most one focused rereview; the audit does not accept or release anything. Sol alone may accept M8 and release G8 after every applicable gate passes. Keep G9 closed and do not start D09 in this run.

C-07 scientific controls remain not_run. C-09 remains unqualified, synthetic_fixture_only, benefit_guaranteed=false. D-T06/D-T07/D-T14 remain blocked without completion credit. Frozen-kit independent_audit_status is a separate pending audit. Preserve the frozen statistical methods, B-T04 bound and seeds. No GitHub push, AIU execution, Notion sync or private study inputs.

Stop with a receipt containing the documentation/candidate commits, clean archive digest, exact package status/counts/named warnings, Astra verdict, Sol disposition and literal resume instruction. Reconcile every owned worker before closing.
