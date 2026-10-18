# C-01 integrated verification and Sol acceptance

Date: 2026-10-18  
Integrated candidate: `b5527ef`  
Integrated repair: `1cf1632`  
Sol-owned registration: `40caa9f`

## Shared registration

Sol applied the packet registration request without changing the scientific
contract:

- exported `integrate_blocks()`, `make_consensus_state()` and
  `apply_consensus_state()`;
- enabled stage-C execution while leaving D, report and all unavailable;
- wired the C-01 CLI through ingestion and the accepted B resolver using the
  declared unit map and keyed matrices;
- enabled only `concat_pca` and rejected disabled backends before resolver work
  with `C_SCORE_BACKEND_UNAVAILABLE`;
- wrote an immutable keyed integration payload, envelope and run registry;
- documented the public interfaces and excluded local audit evidence from the
  package archive.

## Fresh integrated evidence

- `Rscript --vanilla -e "testthat::test_local(filter='C-01', stop_on_failure=TRUE)"`
  passed 109 expectations with zero failures, warnings or skips.
- `Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/C-01.yaml --stage C --output artifacts/acceptance/C-01`
  exited 0 and wrote one complete `concat_pca` result.
- The C envelope validated against the package contract; the payload retained
  study, analysis, preparation-context, representation, unit, feature, matrix,
  selection, required-view and backend lineage.
- `git diff --check` passed.
- Clean local-volume `R CMD build` and `R CMD check` on the final registered code
  ended `Status: OK` with no ERROR, WARNING or NOTE. Durable command logs for
  this run were produced under
  `/private/tmp/triomics-c01-package-check-20261018-3`.

The first package-check attempt correctly detected missing export documentation,
an unqualified `head()` import and accidental inclusion of the local `.audit`
folder. Sol fixed only those mechanical registration defects; a second and final
clean archive check passed.

## Acceptance

C-01-AC1 through C-01-AC4 are **accepted on synthetic software evidence**.
C-01 is accepted and releases C-02. This does not accept C-02, M5 or G5, and it
does not release C-03/C-04/C-05. M5 still requires accepted C-02 followed by its
immutable-snapshot Astra review.

Private study data were not read, fitted, copied, uploaded or committed. Real
study analysis and biological validation remain pending.
