# D13 extra6 closed blocked

Sol candidate `397d7537585b42db3258e17b06aa0360eedf3b24` and Luna `b93859482b839de3609c8365bc23a754123025e6` are preserved, unintegrated. The original compound/using and plural-colon reproducers now pass, and the known cases are consolidated into one named behavioral suite. One remaining count-formatting regression prevents D13 acceptance; [fresh Astra](2026-11-06-d13-extra6-astra.md) returns **audit_pass: no**. G8/G9 remain closed.

The extra6 repair changes only `R/report_targets.R` and `tests/testthat/test-D-T13.R`. The named suite at line 527 covers 384 unique inputs through producer, profile, both lineage columns and HTML. Inputs are deduplicated; previous expectations and exact source case, numeric lineage, shared subjects and non-independence assertions remain. The independent oracle consolidates historical 127+9 probes and the paired twelve-case review, earlier cases and current reproduced boundaries into 199 unique cases (109 retain/90 reject). It passes 199/199. Original manifests and intermediate comparisons are retained locally.

Independent focused D13/D08/D15 tests on this commit report **3,084 pass, 0 fail, 0 error, 0 warning, 0 skip**, 60 blocks (D13 2,804; D08 148; D15 132). Mixed numeric lineage passes. Luna independently reports the same final source counts. There are no named testthat warnings or skips. Seven worker commits preserve four RED observations and intermediate GREENs. Sol found and reproduced two intermediate count-clause/token regressions before handoff; those were corrected inside the active repair. Intermediate passes are not final acceptance evidence.

## Spec

**changes_required**, one P2: the new plural count rule requires whitespace after `:` and treats a sentence-ending period as identifier content. `subjects:32; features selected` and `subjects: 32. Features selected` were retained in lower/title case before this repair but now disappear from both lineage columns and HTML. Uppercase variants still disappear. Sol's paired actual-archive probe is baseline eight passes/four failures and candidate six passes/six failures of twelve, both exit 1. Source values and overlap flags remain intact. This is an ordinary preservation defect under section 6; it does not require a new scientific preference.

## Standards

**pass**. No documented-standard violation or actionable smell finding. Scope and existing behavioral assertions are preserved. The reviewer reports 20/20 targeted synthetic probes, versus 16/20 on the review base; Sol did not claim those as a separate parent-run gate.

## Disposition

Astra independently reproduced the final failure. The existing local-review guard therefore prevents native/installed acceptance checks; full-source and package checks were not run on this candidate. Historical native HTML and installed1,606 results on 6781319 receive no current-candidate credit. The current spec-validator invocation exits 1 because its Python runtimes lack jsonschema/PyYAML; no install or frozen-spec edit occurred and the frozen-kit independent audit stays pending.

The [machine receipt](../evidence/2026-11-06-d13-extra6-close/receipt.json) records commands, all commits, counts and hashes. Archive SHA-256 `9012d68569051efcc630755653d1b93d8bb8216c81bb52df030518a58a5a6e4f`; all 1,169 frozen source hashes verified. Protected dirty files and both clean candidate worktrees remain unchanged. Original attempt1, original repairs2 and explicit extra1..6 remain recorded: totalrepairs8. No post-handoff repair is authorized. All owned workers/processes are terminal; no run-owned sidebar chats or polling. No private study reads, server run, Notion sync or push. Accepted D08/D15 and M7/G7 remain unchanged; C07 not_run, C09 unqualified, D06/D07/D14 blocked without credit.

Literal resume: **D13 extra6 is closed blocked at Sol397d753 and Luna b938594, base6781319. Preserve original attempt1, repairs2 and consumed extra1..6 (totalrepairs8), all seven current commits and prior receipts. Source3084 and known199 pass with numeric/shared-subject preservation; final paired12case candidate6pass6fail and Astra:no leave P2 D13-EXTRA6-LINEAGE-01 open. Do not integrate or redispatch under this consumed grant. Any separately authorized follow-up must preserve the consolidated suite and repair the count-clause formatting boundary without losing complete-token privacy, then pass independent verification and fresh Astra. Keep accepted D08/D15 and M7/G7 unchanged; G8/G9closed. No M8 close or D09 in this cycle.**
