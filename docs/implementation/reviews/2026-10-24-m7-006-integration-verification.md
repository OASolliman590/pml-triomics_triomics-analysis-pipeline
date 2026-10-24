# M7-006 integration verification

- Date: 2026-10-24
- Reviewed candidate: `5242609518886b26e60ea76863aa9c0badfd360d`
- Immutable integrated product snapshot:
  `e4aed594877ef53ace6958d9a757cffb471ba53a`
- Fixed point: `5dcc25f`
- Disposition: **software gate passed; Astra acceptance audit pending**

## Integration identity

Sol cherry-picked the independently reviewed candidate as `e4aed59`. The
working-tree blobs for all three M7-006 allowlisted files equal the candidate
blobs exactly:

- `R/provenance_contract.R` — `873e637ed7a1899746b11322830423a080d60acb`;
- `tests/testthat/test-M7-003-provenance-contract.R` —
  `c6e292e484601ae8d7e2e21164543abeb71ad3b9`; and
- `docs/implementation/registrations/M7-003-contract-foundation.json` —
  `6646fc3cb0212d7701fdd4d15530f64f482bd8b7`.

No frozen specification, schema, package registration, private-data path, or
Stage-D product file was changed by M7-006.

## Focused verification

Sol ran the integrated public-validator suite with:

```text
Rscript --vanilla -e 'testthat::test_local(filter =
  "M7-003-provenance-contract", reporter = "summary")'
```

It exited zero with `DONE` and no failure, warning, or skip. This covers the two
new rejection behaviors and their positive multiple-context controls.

## Full source-suite verification

The original `/tmp/triomics-mofa-lib` and `/tmp/triomics-mofa-py` prefixes had
been cleared by temporary-directory maintenance: MOFA2 had no `DESCRIPTION` and
Python could not import `mofapy2`. That attempt is invalid environment evidence
and failed only the C-05 runtime availability assertions. A subsequent valid
run was interrupted before producing a result and is also not acceptance
evidence.

Sol rebuilt fresh isolated prefixes and verified their versions before the
successful gate:

- MOFA2 `1.22.1` in `/tmp/triomics-mofa-lib-m7-006`;
- mofapy2 `0.7.5` through
  `/tmp/triomics-mofa-py-m7-006/bin/python`.

The uninterrupted full source suite against snapshot `e4aed59` then exited zero
and ended with `DONE`: failures 0, errors 0, skips 0, warnings 14. The summary
reporter did not emit a numeric PASS total. All fourteen warnings are the
previously accepted C-05 native small-fixture diagnostics:

- four low-feature-count warnings;
- four factor-1/average-expression correlation warnings;
- four `no non-missing arguments to max; returning -Inf` warnings; and
- two inactive-factor warnings.

No warning was suppressed and no new warning name appeared. The local raw log
is `docs/implementation/logs/m7-006-full-source-valid-rerun/testthat.log`; it is
not part of the immutable product snapshot.

## Boundary

This is software-contract evidence from public synthetic fixtures. It is not a
real-study fit, biological validation, migration execution, D-T01 acceptance,
M7 acceptance, or release of downstream Stage D. No private study data were
read, copied, fitted, or delegated. The single fresh Astra-low acceptance audit
required by M7-006 remains the next legal transition.
