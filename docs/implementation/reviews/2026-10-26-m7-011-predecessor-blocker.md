# M7-011 predecessor-supply blocker

Sol inspected the worker worktree at `c03756d46738b58d3c2c350cb58242fd767a8947`
on `draft/m7-011-c06`. Completed Luna-max session:
`01a17732-ef8f-70c1-bd83-f7cf391d2961`. Its sole dirty file was
`docs/implementation/registrations/C-06.json`; no product or test changed.
Only its `m7_011_native_provenance_attempt` block is transferred to integration.
`accepted` remains false. M7-011 is **blocked_before_implementation**, not complete.

Preserved worker evidence:

- `Rscript --vanilla -e "testthat::test_local(filter='C-06', stop_on_failure=TRUE)"`:
  FAIL 0 / WARN 0 / SKIP 0 / PASS 290. This is the old suite, not new GREEN.
- `Rscript --vanilla inst/exec/triomics --config inst/examples/tickets/C-06.yaml --stage C --output artifacts/acceptance/C-06`:
  exit 0 and a D handoff, but no B directory and no provenance payload/envelope.
- Sol independently inspected that retained output: only C, D and registries
  are present. `D/handoff-manifest.json` exists; the provenance pair does not.
- `tests/testthat/test-C-06-shared.R` manually writes synthetic B block-results
  and candidate nominations. It never invokes the native B producer.

The missing provenance files are the verified RED condition. A successful
legacy CLI exit and 290 old assertions do not establish the new requirement.
The first worker library override hid testthat and failed before discovery;
the valid baseline preserved the ordinary R libraries. No test was rerun merely
to recreate this preserved receipt.

Omar authorized M7-012 as a separate, bounded predecessor-supply continuation:
define and audit same-output-root native B production before a fresh worker.
The C writer must not fabricate B artifacts. No M7 acceptance, G7 release,
Stage D implementation, private-data access or GitHub push is authorized.
