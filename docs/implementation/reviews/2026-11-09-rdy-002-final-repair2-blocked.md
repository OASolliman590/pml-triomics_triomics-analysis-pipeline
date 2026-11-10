# RDY-002 final repair 2 — cycle closed blocked

**Sol does not integrate or accept `2a9c409d037b22a2d5faa206a84ca3a4ea27f25e`.** Both fresh read-only Sol reviews returned changes required. The initial attempt and two targeted repairs are exhausted; no repair 3 is authorized. Candidate history `ee9d905` → `dd29028` → `2a9c409` and all failed probes remain preserved.

## Immutable verification

Archive SHA-256: `2ac3d6462ff88695284b32a81d7cf6a5d1f7ce37e8a2584c08324989f016781b`. All 1,270 extracted files remained byte-identical after review and independent tests. Runner SHA-256: `e3b6bd531041062de3212b35b853b3927ad62cb01063852ccfb415548f819708`; test SHA-256: `e079274b9e69870e4059072dad098856991082422af92032422e53bc2576e894`. Only the two authorized runner/test files changed in the candidate. The fresh installed commit stamp and runner bytes matched this archive.

| Check | Sol's actual result |
|---|---|
| Isolated archive installation | exit 0; no dependency install or model fit |
| Focused source tests | 25 tests; 305 pass; 0 fail/error/warning/skip; exit 0 |
| Focused installed tests | 25 tests; 305 pass; 0 fail/error/warning/skip; exit 0 |
| Standards review | changes_required; two P1 findings |
| Scientific-contract review | changes_required; one P1 finding |
| Scientific qualification / native regression gate | not run; zero qualification fits |
| Fresh final Astra qualification review | not run; preceding Sol gates failed |

The worker's separately recorded source result also reported 25/305 with no failures/errors/warnings/skips. It is not substituted for the independent runs. Exact commands, runtime identities, results RDS, logs and counterexample scripts are retained locally. [Portable receipt and evidence hashes](../evidence/2026-11-09-rdy002-final-repair2/receipt.json) records separate counts and portable command templates. There are **no named testthat warnings** in either focused Sol run. A package `Status: OK` does not exist for this candidate.

## Remaining findings

**RDY002-R2-01 — native-entry evidence is still premature (P1).** Runner lines 2518–2519 and 2539–2540 write the purported entry marker before invoking `adapter$fit`. Lines 2385–2392 treat that marker as an actual entry. A crash between those statements enters no fitter; MOFA can also refuse its runtime before reaching the native call (`R/integration_mofa.R:894–910`, native call at 792). Sol independently reproduced this using the actual frozen JSON, 282-row plan and legitimate mock reservation/release/identity receipts: **zero adapter or native calls**, but PCA and primary MOFA each report **10 attempted, 10 nonrecoveries**, and an exact binomial interval **[0, 0.308497107818761]**. Entry is unknown in that case; recovery evidence must remain incomplete without that interval. The test's 20 mock reservations remain charged; they are not scientific-run reservations or fits. The maximum real qualification budget remains unspent at 2,550.

**RDY002-R2-02 — production worker argv fails its identity check (P1).** Runner line 891 requires a raw runner filename as an argv token. On the qualified host, Rscript supplies `--file=PATH`. The production request at 2979 uses that normal invocation. Sol independently reproduced watchdog exit **1**, with refusal `Ready worker argv does not bind the run, cell and output root.` The ready receipt contains the actual `--file=...` token and no raw filename token. The positive parent-death probe at 2008 supplies an extra positional runner filename, masking this production-path mismatch. All recorded identities from Sol's self-terminating dummy reproduction were subsequently verified stopped; no reviewer or parent signal was needed for it.

**RDY002-R2-03 — actual OS exit is invented from payload status (P1).** The worker is launched with `system2(wait=FALSE)`. At runner line 943 the watchdog maps result status `complete` to exit 0 and other result statuses to 1; the cancellation path at 832 can assign 0 merely because a result file exists. These are not observed OS exit codes. RDY-003 requires actual exit or explicit uncertainty. Sol confirmed this directly in source. The reviewer's live false-zero experiment encountered a separate inspection race, so **a live false-zero result is not claimed**.

Repair 2 did improve the persistent cleanup block, per-PID cleanup behavior and CLI qualification-disposition wiring. The new positive no-fit process tests and the passing 305-assertion suites do not close the three findings above or establish scientific validity. Requested reviewer model/effort were Sol 6.1/high; effective settings were not exposed.

## Stop and next boundary

[AGENTS.md](../../../AGENTS.md), rule 8: “One initial attempt + at most two targeted repair attempts per packet.” It further requires unresolved work to become blocked. No worker is redispatched, no third repair is relabelled as the same attempt, and no scientific cell is run. Any continuation needs explicit authorization of a new bounded cycle based on this preserved candidate and these concrete findings; previous attempts remain recorded.

RDY-001 remains accepted. RDY-004/005 remain protocol-only passes, with the separate producer awaiting a qualified supervisor. The narrowed RDY-006 proposal `e4f2ff2` permits nothing yet: its fresh execution audit awaits a safe immutable candidate and concrete supervision evidence. No full-source or clean-package native regression run occurred. BioPAN source research remains research only. The private fasting structure review is complete, but participant/visit linkage and lab preprocessing remain unverified; no study configuration is executable on inferred identities.

Accepted core M1–M9/G7–G9, all packet/milestone/conditional records, frozen scientific files, B-T04 bound `0.08` and seeds `1001:1400` remain unchanged. No private-study fit, public data upload, GitHub push, AIU job or Notion sync. This is a blocked readiness cycle, not a reversal of core acceptance and not completion of the readiness programme.

Automatic approval review rejected the numeric whole-process-group SIGKILL probe because it lacked per-member identity checks. It was not executed or retried. Later individually revalidated PID probes passed; the remaining findings above independently prevent integration.

Literal resume instruction: **Authorize a new bounded C-07 repair from `2a9c409` for RDY002-R2-01/02/03, preserving the exhausted RDY-002 history. Use a fresh isolated Luna worker, independent Sol verification and the required fresh Astra gates. Do not resume the exhausted worker, alter the frozen scientific protocol or spend native fits before the relevant prerequisites pass.**
