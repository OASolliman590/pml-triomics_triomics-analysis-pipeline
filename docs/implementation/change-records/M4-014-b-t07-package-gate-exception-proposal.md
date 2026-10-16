# M4-014 — proposed B-T07 package-gate exception

- Date: 2026-10-16
- Status: **awaiting explicit Omar authorization; no edits authorized**
- Trigger: `docs/implementation/reviews/M4-013-package-check-gate.md`
- Astra classification: package/test infrastructure only; no demonstrated
  scientific regression; no mandatory gate may be waived
- Attempt accounting: B-T07's initial attempt and two repairs remain consumed.
  If authorized, this is one exceptional package-gate correction, not a reset.

## Exact proposed scope

One fresh Luna-max worker in one fresh isolated worktree may modify only:

- `tests/testthat/test-B-T07.R`;
- `R/export_b.R`;
- `man/triomics-api.Rd` (Sol-owned mechanical documentation correction may be
  applied serially by Sol instead of the worker).

The correction must:

1. make B-T07's public synthetic ticket fixtures resolve from both a source
   checkout and an installed package by copying/rebasing them into temporary
   test layouts using packaged `inst/examples/data` resources;
2. make `t07_cli_smoke()` use the real source executable in a checkout and the
   installed package's actual `triomics_main()` dispatcher in a clean child R
   process after installation—never simulate output or bypass CLI error mapping;
3. preserve every B-T07 scientific, hash, restart, privacy and reproduction
   assertion and add no skip;
4. replace the literal em dash in `R/export_b.R` with `\u2014`, preserving the
   rendered Markdown, and qualify the existing `setNames()` call as
   `stats::setNames()`;
5. consolidate the duplicate `resume` Rd argument without changing either API.

No model, threshold, seed, family, matrix value, artifact schema, acceptance
criterion, dependency, fixture data, private input or public claim may change.

## Required gates if authorized

Fresh Sol-high review; source and installed B-T07 parity; full local suite; clean
Git-archive package check with no ERROR, WARNING, NOTE or required skip; then one
immutable M4 Astra review. B-T07, G4 and Stage C remain unreleased until all pass.
