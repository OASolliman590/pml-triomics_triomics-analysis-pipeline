# M4-005A Unit A — fresh Sol review

- Date: 2026-10-15
- Reviewer: `/root/m4_005a_sol_review` (requested model `gpt-6.1-sol`, high)
- Integration base: `c10e7d8bd0331ff782d5a293d0e25ee3fb153f37`
- Candidate: `97e7c28715581b8dd5c070e41c52f5f4c0bf2523`
- Full local receipt: `.superpowers/sdd/M4-005A-b-t05-result-boundary/task-3-sol-review.md`
- Verdict: **BLOCKED; not approved for integration**

## Accepted implementation evidence

The complete restored B-T05 candidate and result-boundary adapter were reviewed.
The target suite passed 302 assertions with no failures, warnings or skips. A
fresh local package installation succeeded. Five source/installed probes agreed
for unavailable DEqMS, complete and partial explicit limma fallback, ordinary
limma, and qualified DEqMS. Payload, envelope and run-registry generic statuses
agreed; original method status and approved provenance were retained. Fallback
rows were labeled limma, carried no DEqMS statistic fields and no count-prior
references. No production adapter defect, source/installed divergence or false
DEqMS claim was found.

## Blocking finding

The mandatory adjacent run passed 815 assertions but failed three assertions in
`tests/testthat/test-B-T01.R:481-483`. That test requests DEqMS with missing
counts and `fallback = "explicit_limma"`, then expects no fallback, unavailable
status and zero blocks. Those expectations contradict accepted M4-003, B-T05
AC4 and M4-005, which require an explicitly requested limma fallback to run and
remain separately labeled when DEqMS counts are unavailable.

The failure is stale acceptance text, not a scientific or fitting regression.
However, `tests/testthat/test-B-T01.R` is outside M4-005A's allowlist. The M4-005
stop rule therefore forbids repairing it under Unit A. The candidate remains
unintegrated and Unit B remains blocked. No private study data were read and no
biological validation is claimed.

