# M7-001 — proposed D-T01 provenance-boundary repair exception

- Date: 2026-10-23
- Status: **proposed; explicit Omar authorization required**
- Trigger: `docs/implementation/reviews/2026-10-22-d-t01-shared-sol-review.md`
- Astra audit: `docs/implementation/reviews/2026-10-23-d-t01-blocker-astra-audit.md`
- Attempt accounting: D-T01's initial implementation and two targeted repairs
  remain consumed. Authorization would permit one exceptional repair only; it
  would not reset any counter or waive any review or acceptance gate.

## Exact proposed scope

One fresh Luna-max worker in one fresh isolated worktree based on rejected
candidate `71c0a1b` may modify only:

- `R/evidence_bundle.R`;
- `tests/testthat/test-D-T01-shared.R`;
- `tests/testthat/test-D-T01.R`, only if lower-level coverage is required.

Sol may serially apply the corresponding shared CLI lifecycle repair only to:

- `R/cli.R`.

The exceptional repair must:

1. reconcile every currently ingested source, metadata, feature-registry, and
   representation identity with the immutable predecessor B/C lineage before D
   publishes anything;
2. reject changed assay labels or values and changed consumed metadata while
   preserving the unchanged-source positive path;
3. reject every symlink ancestor before registry rollover, rename, or write,
   leaving outside files and predecessor output bytes unchanged;
4. require any supplied B envelope `artifact_id`, `preparation_context_id`, and
   `representation_id` to agree exactly with verified producer/bundle/payload
   identity, even after all hashes are recomputed;
5. validate config and dependencies before registry rollover so invalid input
   leaves the current registry unchanged;
6. if structured selection comparison is repaired, treat reordered object keys
   as equivalent while altered keys, extra fields, values, and ordered arrays
   still reject.

No model, statistic, threshold, seed, family, selection result, scientific
claim, frozen contract, global serializer, producer/B/C logic, public adapter,
namespace, schema, dependency, lockfile, or private input may change.

## Required gates if authorized

- RED then GREEN public synthetic probes for every defect above, including all
  consumed sources rather than only `block_1`;
- exact D-T01 named suite with no required skip;
- fresh public Stage B to C to D execution with actual-byte envelope/input hash
  read-back, disabled-route accounting, and no-upstream-refit traps;
- standalone B/C and sequential registry preservation;
- D rerun refusal with every existing byte unchanged;
- one focused read-only Sol-high rereview of the immutable repaired diff;
- serial integration and fresh integrated verification before D-T01 acceptance.

D-T01, M7, and every direct or transitive Stage-D successor remain closed until
all gates pass. Synthetic software evidence cannot establish biological or
clinical validity.

## Authorization phrase

`I authorize M7-001.`
