# M4-005B coordinator package-check gate

- Date: 2026-10-16
- Candidate: `0c84428`
- Complete local suite: 3,169 passed, 0 failed, 0 warned, 0 skipped
- Package build: succeeded
- Package check: failed; installed tests 3,156 passed and 3 failed; one package-structure NOTE
- Failure classes: source-only B-T04 fixture paths in an installed-composition test; source-root assumption in a global-idempotence subprocess; `.superpowers/` absent from `.Rbuildignore`
- Disposition: integration remains blocked; package-context reconciliation is M4-008, pending independent Astra audit
- Privacy: public synthetic fixtures only; no private study data were accessed

