# M4-010 initial Astra audit

- Date: 2026-10-16
- Candidate: `133fe646fd3bc458ea9970f06a79909f28f9d722`
- Role: fresh Astra low, read-only
- Verdict: **FAIL**
- Findings: P0 0, P1 1, P2 0

## P1 — permitted test surface was insufficient

The record initially permitted only the fixture-copy helper to change. That
cannot produce installed-context GREEN because `b08_run_source_cli()` separately
requires `pkgload::load_all()` at a checkout-relative root and invokes
`inst/exec/triomics`, while `.Rbuildignore` intentionally excludes `inst/exec`
from the installed package. Fixing resource lookup alone would expose this next
checkout-only harness failure.

The record otherwise preserved scientific semantics, all required assertions,
privacy boundaries and the historical initial attempt plus two consumed repairs.
The authorized next transition is a record-only correction permitting narrowly
bounded source-versus-installed CLI harness selection in the same test file,
followed by the single focused Astra rereview before worker dispatch.

No implementation, acceptance, B-T07 release, literature search or private-data
access occurred in this audit.
