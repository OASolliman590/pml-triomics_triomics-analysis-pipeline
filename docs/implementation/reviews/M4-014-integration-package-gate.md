# M4-014 integrated verification and package gate

- Date: 2026-10-16
- Tested integrated commit: `d2a2231f9299fd2f529aa8a66f2f5c0872d8e908`
- B-T07/M4 disposition: software gates passed; immutable Astra review pending
- Private study data: not read, fitted, copied, uploaded or committed

## Fresh integrated evidence

1. Source-checkout B-T07 on the actual repository path containing spaces:
   124 expectations passed; zero failures, errors, warnings or skips.
2. Clean temporary installation from `git archive` of the tested commit:
   installation completed successfully. Installed B-T07 passed 124
   expectations with zero failures, errors or skips. Eight disclosed warnings
   were the expected unavailable-Git-metadata probes in an archive without
   `.git`; no path, fixture or CLI warning occurred.
3. Complete local suite: 3,462 expectations passed; zero failures, errors,
   warnings or skips.
4. Clean Git-archive package gate:
   `R_PROFILE_USER=/dev/null Rscript --vanilla tools/check-package.R` completed
   successfully and the native check log ended with `Status: OK`. Installed
   tests, examples, documentation, namespace, non-ASCII and dependency checks
   all passed with no ERROR, WARNING or NOTE.

## Evidence hashes

- `build.log`: `67f790bc312d6e11d85921379f7ffdaba2f7cf05c9f44686a931c06028067ff6`
- `check.log`: `75c491ae9d3d149e9073e33632e864e9353644a83cbb0253ad6f284e6b376c8b`
- built source archive: `5826fbd7260aea32b6e11fde56cbfda703929f84b1f61ffbad2ad9483b7850ae`

These are synthetic software and packaging checks, not biological validation.
B-T07 and G4 remain unreleased until the independent immutable M4 Astra review
passes and Sol records the acceptance disposition.
