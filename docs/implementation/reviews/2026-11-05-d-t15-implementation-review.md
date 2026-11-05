# D-T15 implementation review — closed blocked

D15 resumed its unfinished initial attempt from `5abc369`. One initial attempt and both allowed repairs are now consumed. No D15 product change is integrated into the canonical branch; accepted remains false and G8/G9 remain closed. See the [bounded close receipt](2026-11-05-d-t15-bounded-close.md).

| Round | Worker / equivalent Sol commit | Source evidence | Review disposition |
|---|---|---|---|
| Initial | `72a1224` / `645d107` | 227 pass; 0 fail/error/warn/skip; 36 blocks | Changes required |
| Repair 1 | `f6e3ee7` / `eee908d` | 255 pass; 0 fail/error/warn/skip; 43 blocks | Partial-member integrity bypass |
| Repair 2 | `e891344` / `518ae4a` | 274 pass; 0 fail/error/warn/skip; 47 blocks | Pinned absence / loaded presence contradiction |

The initial worker changed exactly the five frozen D15 files. Both repairs changed only R/report_clinical.R and tests/testthat/test-D-T15.R. The registration remains unaccepted. Initial native A/C-scheduling/B/C-publication/D/report commands exited zero on the public twelve-person fixture and produced a real HTML report with numerical and identifier readback. Those results belong to the initial candidate, not the final repair.

Initial reviews identified false C-02 clinical identity and borrowed units, B/C transport mismatch, erased producer-qualified independent association categories, and an invalid structured-error call. Sol additionally reproduced the installed fixture path defect and missing B clinical payload-to-D binding. Repair 1 corrected these. Both focused reviewers then found a partial-member path bypassing integrity checks. Repair 2 now checks all present references while preserving partial scientific availability.

Both final reviewers found one remaining P1, D15-REVIEW-ABSENCE-01: a D-pinned absent clinical member can coexist with a loaded clinical artifact, whose values are still displayed. The absence test deletes both loaded fields and misses the contradiction. Sol reproduced an effect of 0.4 displayed as partial/unverified while the pinned B bundle bytes remained unchanged. This is a typed synthetic report-boundary reproduction; native producer reachability for this exact combination was not established. No product files were changed for this probe.

All final snapshot hashes were reverified. Local Spec and Standards review receipts and the probe are in [close evidence](../evidence/2026-11-05-d-t15-bounded-close/receipt.json). Models requested: Luna max and Sol 6.1 high; the host accepted the requests but exposed no effective-setting introspection. No Astra D15 audit or final parent acceptance gate ran after the P1. No repair 3 is opened.

Harness history is retained locally: an initial install probe activated a startup profile and failed before tests; the isolated correction installed successfully and exposed the example-path RED. A report tamper probe initially expected source refusal where the existing-output guard correctly ran first; direct provider probing then demonstrated the missing binding. A repair-2 filter-miss invocation ran no qualifying tests; its raw log was overwritten by actual RED and contributes no evidence. None of these is a final acceptance result. Protected canonical dirt was preserved.
