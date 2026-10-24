# M7-005 — C-05 source-suite acceptance-state repair

- Date: 2026-10-23
- Status: **authorized by the active implement-to-green instruction**
- Scope: one stale source-test assertion block only
- Product/scientific behavior: unchanged

## Trigger

The immutable M7-004 candidate traversed its provenance tests successfully, but
the source-tree full suite failed on two C-05 assertions written before C-05 was
accepted. `docs/implementation/registrations/C-05.json` was changed to accepted
by commit `cad313b`, while `tests/testthat/test-C-05.R` still expects
`accepted = false` and the earlier implementation-status sentence. Neither file
differs between M7-004 review base `221df65` and candidate `65662bc`.

With the approved isolated runtime (MOFA2 1.22.1 and mofapy2 0.7.5), the focused
C-05 source suite has exactly those two failures and the fourteen already named
native small-fixture warnings. This record does not suppress or relabel those
warnings.

## Allowlist and behavior

One fresh Luna-max worker may edit only `tests/testthat/test-C-05.R`. It must:

1. reproduce the two registration-state failures using the approved isolated
   MOFA runtime;
2. update only that test block's name and expectations to the tracked accepted,
   synthetic-software-only registration state;
3. preserve assertions for applied shared files, enabled CLI registration,
   MOFA2/mofapy2 dependencies, implemented route status, known gaps, no real or
   biological validation, and no false `not implemented` claim;
4. rerun focused C-05 with the same runtime and record all fourteen warnings by
   test and disposition; and
5. return one clean one-file commit for fresh read-only Sol review.

One implementation plus at most two targeted repairs is allowed. The worker may
not edit C-05 product code, registration JSON, package metadata, provenance code,
frozen spec, ledger, handover, or any other file. This test repair does not
accept M7-004, D-T01, M7, or any Stage-D successor and does not use private data.
