# M7-019 — source and installed-package compatibility

Authorized 2026-10-28; new bounded cycle based on c9a5259, preserving d660879
product baseline, 774bbcc null repair, 6456055 and cfe9160 historical candidates.
Implementation awaits fresh Astra-low scope audit. One fresh Luna-max worker,
initial attempt plus at most two targeted repairs; no recursive delegation.

## Required behavior

1. Restore the established named numeric delta visit_weights API. Preserve visit
names and signed values at the C endpoint serialization boundary only. Explicit
SYN_12 time:null, retained-time/score/exclusion guards and native assertions remain.
Do not change the shared global JSON writer or weaken any expected value.
2. Native producer provenance must work from a clean installed package using
truthful immutable build-source identity. A Git archive export-subst stamp may
carry its actual full commit into the installed package; unexpanded/missing or
malformed stamps must fail closed. Never invent a commit, consult an unrelated
working-directory repository, initialize fake Git history, or accept a user/env
claim as verified lineage. Resolve source identity from the executing package or
its actual source root. Keep existing SHA-256 predecessor binding and profile
schema unchanged. Explain the stamp trust boundary: source/build metadata is not
a signature or proof against a malicious repackager. If a truthful compatible
identity cannot be supplied without a schema/contract change, stop with evidence.
3. Fix installed test-resource and CLI paths so the same synthetic fixtures and
assertions execute from source and installed package. No test skipping, fixture
science changes, fake provenance, or numerical expectation changes.
4. Capture every testthat warning with test name and message in the original test
run, preserving normal reporter counts and output. Save that artifact before
package-check cleanup. No warning suppression or extra heavy rerun to inventory.
5. Correct Rd usage for provenance_context without roxygen; qualify setNames.
Exact archive Status: OK required; an exit code alone is insufficient.

## Ownership and allowlist

Luna may edit R/integration_endpoints.R, R/export_c.R, R/export_b.R,
R/provenance_contract.R; tests/testthat.R; tests/testthat/test-C-08.R,
tests/testthat/test-C-08-shared.R, tests/testthat/test-B-T01.R,
tests/testthat/test-B-T07.R; existing tests/testthat/helper*.R for resource paths;
new tests/testthat/test-package-source-identity.R; .gitattributes and a narrowly
named inst source-revision stamp file. Record each helper/stamp path before edit.
Sol exclusively applies shared tools/check-package.R and man/triomics-api.Rd
changes from Luna's separate proposed patch. No product changes in the live
integration checkout before reviewed candidate integration. No lockfile,
NAMESPACE, DESCRIPTION or stage-registration change without a revised scope.
If another existing test needs only the identical resource-path repair, identify
it for Sol scope reconciliation before editing; do not silently expand.

## Proof and order

Astra scope audit on frozen record first; one documentation correction/rereview
if needed. Luna writes/reproduces failing behavioral tests, implements, verifies
focused C08 and source identity/resource paths. Parent inspects all diffs and
applies shared patch serially. Freeze repaired candidate. Parent verifies native
synthetic B/C/D null and full predecessor hashes, focused B-T07/C06/C08shared/D-T01
and C08 standalone. Integrate only after these pass. Run uninterrupted full source
suite and clean git-archive package check on integrated candidate with isolated
MOFA2 1.22.1 / mofapy2 0.7.5; record all counts and warning names. Repair failures
only within scope and attempt budget, rerunning affected and final gates after
product changes. Fresh read-only Astra-low audits immutable final snapshot and
actual gate evidence; Astra never edits or accepts milestones.

## Exclusions and acceptance

No private data, push, later D packets, methods/schema changes, B-T04 bound/seed
changes, or invented uncertainty/clinical validation. C06 accepted false,
C07 not_run, C09 unqualified. M7/G7 remain closed until all required gates pass;
this cycle alone does not override unmet canonical milestone dependencies.
Historical AGENTS/CLAUDE frozen operational hash drift is recorded, not silently
rebaselined. New scientific-contract conflicts require an audited amendment.
