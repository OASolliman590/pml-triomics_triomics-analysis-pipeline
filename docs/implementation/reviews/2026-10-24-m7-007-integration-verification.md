# M7-007 integration verification

- Date: 2026-10-24
- Candidate commits: `0b1fcbdbb321a8e6caa002d19e769d693d53d2bd`,
  `b34df9f5d93ca60d74f690a2e86e5f416547fec6`
- Immutable integrated product snapshot:
  `b29c3aa5693b29ac232f26344987877e4e79511d`
- Product fixed point: `e4aed594877ef53ace6958d9a757cffb471ba53a`
- Disposition: **software gate passed; Astra acceptance audit pending**

## Integration identity

Sol integrated the implementation as `ea1407b` and the registration-only repair
as `b29c3aa`. The integrated blobs equal candidate `b34df9f` exactly:

- `R/provenance_contract.R` — `82973c009e0273551c74e23c8763afc99e018827`;
- `tests/testthat/test-M7-003-provenance-contract.R` —
  `9a96ba05ddd133c875230f187e149ad58592de2f`; and
- `docs/implementation/registrations/M7-003-contract-foundation.json` —
  `29d873bc59014b8042ec15c5ef7154553c9a5663`.

No schema, frozen specification, package/CLI registration, producer/D seam, or
private-data path changed.

## Focused verification

The integrated public-validator suite ran with:

```text
Rscript --vanilla -e 'testthat::test_local(filter =
  "M7-003-provenance-contract", reporter = "summary")'
```

It exited zero with `DONE` and no failure, warning, or skip. The escaped-U+001C
collision profile rejects, while a valid fully covered escaped-identifier
multiple-context profile passes.

## Full source-suite verification

Before the gate, Sol verified MOFA2 `1.22.1` and mofapy2 `0.7.5` in the isolated
runtime. The uninterrupted full source suite against snapshot `b29c3aa` exited
zero and ended with `DONE`: failures 0, errors 0, skips 0, warnings 14. The
summary reporter did not emit a numeric PASS total.

All fourteen warnings are the accepted C-05 native small-fixture diagnostics:

- four low-feature-count warnings;
- four factor-1/average-expression correlation warnings;
- four `no non-missing arguments to max; returning -Inf` warnings; and
- two inactive-factor warnings.

No warning was suppressed and no new warning name appeared. The local raw log
is `docs/implementation/logs/m7-007-full-source/testthat.log`; it is not part of
the immutable product snapshot.

## Boundary

This is public synthetic software-contract evidence, not a real-study fit,
biological validation, migration execution, D-T01 acceptance, M7 acceptance, or
downstream Stage-D release. No private study data were read or delegated. The
single fresh Astra-low audit remains the next legal transition.
