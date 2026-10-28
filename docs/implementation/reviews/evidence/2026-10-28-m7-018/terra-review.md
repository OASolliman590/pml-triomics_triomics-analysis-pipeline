# M7-018 frozen diff review

Verdict: no findings in scoped read-only review; severity: none. Commit 774bbcc5f7ad3152e6daf1d57d1972cf99519963 against 6456055d394057baf029eba7321c45ad90a3ec21; clean checkout. Requested gpt-6.1-terra/high; effective runtime settings not independently exposed.

The only diff is R/cli.R's named `time = if (absent_all_missing_time) NULL else time` within the trajectory composition list. The condition requires no represented blocks, source exclusion, the absent-all reason, and missing non-NaN time. Composition retains the named list element; C08 preserves it as a list-column in the validated unit map; the unchanged atomic JSON writer uses `null = "null"`. A no-fit jsonlite serialization/readback probe confirmed that this named list-column writes `"time":null` and reads back a present `time` field with NULL. Actual native pipeline proof is pending the parent run.

Represented-time finite checks, retained/score coverage and excluded-score guards are unchanged. No schema, shared writer, or D consumer changed. The two native assertions are unchanged in /tmp/triomics-m7-018-terra/red-native.R; the repository native readback assertion is unchanged as well. The literal JSON assertion belongs to the retained independent proof script, not a newly committed test.

Review does not constitute test-suite pass, scientific validation, milestone acceptance, or release. C06 false; C07 not_run; C09 unqualified; M7/G7 closed.
