# B-T05 dispatch reopening investigation

- Date: 2026-10-15
- Candidate inspected: `efd830884b894d79441af5a948fa3b600ff8e4db`
- Scope: public synthetic fixtures and repository code only
- Status: root cause reproduced; no implementation performed

## Observed failure

The candidate packet suite passes 63/63 assertions, with no failures, warnings or
skips. A missing-count, explicitly-fallback-enabled level request nevertheless
returns only:

```json
{
  "status": "unavailable",
  "fallback_applied": false,
  "blocks": [],
  "effects": null,
  "limma_version": "3.68.5"
}
```

This omits the requested DEqMS identity, the named
`count_metadata_missing` reason and the declared limma fallback required by
B-T05-AC4.

## Root cause

`triomics_b05_dispatch_fit()` decides whether to enter B-T05 by calling
`triomics_b05_has_count_source()`. For a level request with missing counts and
`fallback: explicit_limma`, that predicate is false. The request is delegated to
the older generic fitter before `triomics_b05_fit()` can classify the missing
counts or apply the explicit fallback.

The downstream B-T05 behavior already exists. Calling `triomics_b05_fallback()`
with the dispatcher's captured predecessor returns `fallback_complete`, method
`limma_explicit_fallback`, requested method `DEqMS`, a DEqMS unavailable reason
of `count_metadata_missing`, and explicit fallback provenance. The defect is
therefore the dispatch gate, not the DEqMS or limma calculations.

## Repair invariant

For a request whose method is DEqMS, dispatch must depend on whether the analysis
lane is supported by B-T05, not on whether the required count input is already
valid. Within supported B-T01 level and B-T03 delta lanes, missing or invalid
counts must reach `triomics_b05_fit()`, which then returns named unavailability
or the separately identified explicit limma fallback. Unsupported B-T02 and
other lanes remain outside B-T05.

## Required regression probes

1. Level request, missing counts, fallback `none`: named DEqMS unavailable with
   reason `count_metadata_missing`.
2. Level request, missing counts, fallback `explicit_limma`: separately labelled
   limma fallback with DEqMS unavailability provenance.
3. Valid count metadata: unchanged DEqMS 1.30.0 execution and reference fields.
4. Non-DEqMS requests and unsupported lanes: unchanged predecessor dispatch.
5. Source and installed CLI paths: the same AC4 status/provenance behavior.

The private study directory was not read, fitted or uploaded.
