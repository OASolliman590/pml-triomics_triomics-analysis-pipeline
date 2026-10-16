# M4-014 package-gate authority — Astra-low adjudication

- Date: 2026-10-16
- Adjudicator: `/root/m4_014_package_gate_astra`
- Snapshot: `bd7da66ba58bbce571e90ab1ca76017c8e269d78`
- Verdict: **BLOCKED pending explicit Omar authorization**

The package failures are packaging and test-infrastructure defects with no
demonstrated scientific regression. The minimal correction touches B-T07-owned
`tests/testthat/test-B-T07.R` and `R/export_b.R`, plus the Sol-owned API manual.
The installed-test correction must cover both fixture resolution and the latent
checkout-only CLI runner assumption. The source corrections preserve text and
behavior by using `\u2014` and `stats::setNames()`; the manual may consolidate its
duplicate `resume` entry mechanically.

B-T07 has exhausted its two repairs. M4-010 authorized only the earlier B-T08
installed-test exception and is not transferable. Therefore the source/test
correction requires one explicit Omar exception; consumed counters remain.
No package, test or milestone gate may be waived. B-T07, G4 and Stage C remain
unreleased.
