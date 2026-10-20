# C-03 and C-04 Sol acceptance

Date: 2026-10-20. Evidence is public synthetic software evidence only. No
private study data were read, fitted, copied, uploaded, or committed.

## C-03 — categorical DIABLO

- implementation `9758b4a`, repair `826725f`, registration `d5037ec`;
- initial Astra review: zero Critical, four Important;
- focused Astra rereview: all four Important findings closed, with no new
  Critical or Important finding;
- integrated focused evidence: 82 expectations passed with zero failures,
  warnings, or skips; a fresh current-head rerun completed successfully;
- synthetic CLI: 12/12 eligible fixture units predicted, 15 inspected fits,
  balanced error 0; this is not a biological performance estimate.

Sol accepts C-03-AC1 through C-03-AC4.

## C-04 — continuous block.spls

- implementation `a3b9713`, repair and registration `6ea8d20`, installed
  variance-untrace guard `fda190f`;
- initial Astra review required three Important corrections;
- focused Astra rereview found those corrections closed and no new Critical or
  Important defect; a later read-only review passed the installed guard;
- integrated focused evidence: 132 expectations passed with zero failures,
  warnings, or skips; a fresh current-head rerun completed successfully;
- synthetic CLI: 12/12 predictions, RMSE about 0.509, 15 inspected fits; these
  are fixture results, not biological performance.

Sol accepts C-04-AC1 through C-04-AC4.

## Shared package gate

The preserved `fda190f` archive check is `Status: OK`, with testthat `FAIL 0,
WARN 8, SKIP 0, PASS 3985`. The eight warnings are not from C-03 or C-04. They
are the installed B-T07 missing-Git-metadata probes named and dispositioned in
`2026-10-20-fda190f-warning-disposition.md`. Fresh installed-suite isolation
confirmed C-03 and C-04 emit zero testthat warnings. Product files for C-03 and
C-04 are unchanged between `fda190f` and current HEAD.

This accepts two packets, not milestone M6. C-05, C-08, C-09, and C-06 remain
unaccepted; G6 and Stage D remain closed.
