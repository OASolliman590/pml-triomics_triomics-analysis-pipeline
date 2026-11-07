# D09 Quarto packaging diagnostic and bounded coordinator scope

Sol checked immutable initial D09 candidate `0fc0890ccab2b4118cf49ddf6cb1f9e27fcbfe23` with a packaging-only diagnostic. `R CMD build --no-build-vignettes` exited 0. `R CMD check --no-tests --no-examples --no-manual --no-build-vignettes` also exited 0, but its log says **Status: 1 WARNING, 1 NOTE**. No tests or examples ran; this is not an acceptance gate.

The warning names `independent-study.qmd` and `paired-study.qmd`: files in `vignettes` without `inst/doc` output, no Sweave sources and no VignetteBuilder. The note repeats that the directory contains no registered R-package vignettes. The transcript and command receipt are local under `m9-20261107-01/packaging-layout-probe/`; archive SHA-256 `df49400aeb4ec2525dffbd734dafc03008b01e94eb006a50b39ec4d6b0af7f71`.

Sol owns a narrow shared packaging change: exclude exactly these two source-run Quarto tutorials from the R package tarball via one `.Rbuildignore` rule. They remain tracked in the immutable Git archive and must both execute against that archive's installed package at final verification. No checker flags, tests, vignette contents, product behavior, dependencies or scientific contract change. This is packaging metadata, not permission to bypass required walkthrough execution or to reset Luna's attempts. The runtime report still must be self-contained and offline.

The canonical checkout has pre-existing dirty `.Rbuildignore` content. Do not stage that file from the working tree. Prepare the rule against the clean committed blob, inspect/test it separately, and integrate only that exact indexed hunk while preserving the working bytes and existing user changes. `.gitignore`, `renv/activate.R`, AppleDouble files and other user dirt remain excluded.

Status: scope recorded before edit; metadata diagnostic/independent review pending. Existing Luna remains on targeted repair 1 of 2, limited to its two files. D09/M9 unaccepted; G9 closed. Final full source, exact-OK clean archive, native walkthroughs/clinical smoke and fresh Astra remain mandatory. M8/G8 and D13 are preserved.

## Verification and integration

The isolated metadata diagnostic now says **Status: OK**, with build/check exit 0. Tests/examples were deliberately skipped in this diagnostic; it earns no M9 gate credit. The original warning/note transcript is retained. The exact patch SHA-256 is `2f08561306a8fa4cd7420f52eb6b79fdd4a833e13b9f500aa2bb730ab233479f`; diagnostic log SHA-256 `f7073254e8bd6365ca3628280c1e8ab8b2dadbd813e6283d94a373ab8489c0be`. The source archive keeps both tutorials; the built package retains all three product report templates.

Independent Sol Standards review: `appropriate_bounded_metadata_change`, with no findings. Review SHA-256 `720b6d749ed25453eba78674efef782ede5df8df6d8c3ee3bf5558f669f8b59d`. Only the clean indexed hunk is integrated; canonical protected working bytes remain unchanged. Final verification still requires both tutorial renders, full source suite, exact-OK full archive gate, native/clinical read-back and fresh Astra on the final product. Luna remains repair1/2; G9 closed.
