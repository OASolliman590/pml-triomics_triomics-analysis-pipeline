# M4-005 — reconcile the shared Stage B result and dispatch contracts

- Date: 2026-10-15
- Trigger: the exhausted B-T05 and B-T06 final reviews each found a blocker in
  shared integration code outside the authorized packet repair allowlist.
- Design approval: Omar approved the architectural design on 2026-10-15.
- Status: **proposed; independent Astra audit and Omar execution acceptance
  required**
- Inputs:
  - `docs/implementation/reviews/B-T05-final-repair-review.md`
  - `docs/implementation/reviews/B-T06-final-repair-review.md`
  - B-T05 final candidate `b6d397072db09b67b765e9e6cf0e2454798f2d12`
  - B-T06 final candidate `086d604de8ba481e437d3c9109a1780839104810`

## Decision

Create one bounded shared-integration programme with two sequential acceptance
units. This is not repair 3 for either packet and does not reset their attempt or
review counters. It may reconcile only the two shared seams demonstrated by the
final reviews. Packet statistics, estimands, thresholds, fixtures and frozen
controls remain unchanged.

The first unit makes the existing B-T05 method outcome representable at the
Stage B artifact boundary. The second unit replaces implicit source-order
composition with one idempotent Stage B registration path so B-T02 through
B-T06 coexist. B-T07 and every later consumer remain blocked until both units,
fresh integrated verification and the independent M4 Astra gate pass.

## Scientific and data-contract decisions

1. Artifact completion and statistical method outcome remain different fields.
   The envelope vocabulary stays `not_requested`, `not_started`, `running`,
   `complete`, `partial`, `unavailable` and `failed`. M4-005 must not add
   `fallback_complete` or `fallback_partial` to `R/contracts.R`.
2. At serialization, `fallback_complete` maps to artifact `complete` and
   `fallback_partial` maps to artifact `partial`. The original method outcome is
   retained as `method_status`; it is not discarded or relabelled as DEqMS.
3. Stage B payloads expose, when present, `requested_method`, actual `method`,
   `method_status`, `unavailable_reason`, `deqms_status`, `deqms_version`,
   `fallback_applied` and `fallback_provenance`. A named DEqMS unavailability
   reason is also represented in envelope reason codes without changing a
   successfully produced fallback artifact into an unavailable artifact.
4. An explicit limma fallback is a limma result. It never acquires a DEqMS method
   label, statistic name or count-prior claim.
5. Dispatch composition is ordered and idempotent: B-T02, B-T03, B-T04, B-T05,
   then B-T06, with the B-T04 Stage B hook preserved. Source and installed
   execution must produce the same lane choice.
6. No private study input, empirical tuning, new analysis lane, new statistical
   method or change to B-T04's frozen calibration controls is authorized.

## Unit A — B-T05 shared result boundary

### Preparation and write authority

Sol creates a fresh isolated worktree from the then-current integration commit
and restores these four files from B-T05 final candidate `b6d3970` unchanged in
a preparatory commit:

- `R/statistics_deqms.R`
- `tests/testthat/test-B-T05.R`
- `inst/examples/tickets/B-T05.yaml`
- `docs/implementation/registrations/B-T05.json`

One fresh Luna max worker may then modify only:

- `R/results_b.R`
- `tests/testthat/test-M4-005-B-T05-integration.R`

`R/contracts.R`, the restored B-T05 files, canonical specification, package/CLI
registration and all other packet files are forbidden. Sol alone updates the
ledger and integrates an accepted commit.

### Required failing tests before implementation

- Missing counts with fallback `none` must reach B-T05, produce a valid
  `unavailable` envelope and payload, and preserve requested/actual method plus
  the named reason.
- Missing counts with `explicit_limma` must produce a valid `complete` or
  `partial` artifact, preserve `fallback_complete` or `fallback_partial` as
  `method_status`, and expose explicit limma provenance and DEqMS
  unavailability.
- Ordinary limma output must remain unchanged and must not acquire DEqMS or
  fallback provenance.
- Source and installed CLI paths must agree. B-T01, B-T03 and B-T04 adjacent
  fixture checks must continue to pass.

### Unit A acceptance

- B-T05 AC1–AC4 pass without skipped required tests.
- Both payload and envelope validate, and serialized fields explain the method
  actually used without conflating fallback with DEqMS.
- A fresh Sol 6.1 high reviewer accepts the complete diff from the current
  integration base, including the restored packet files.

## Unit B — centralized Stage B dispatch composition

### Preparation and write authority

Unit B starts only after Unit A is integrated. Sol creates a new isolated
worktree and restores these six files from B-T06 final candidate `086d604`
unchanged in a preparatory commit:

- `R/multiplicity.R`
- `R/nominations.R`
- `R/rank_sensitivity.R`
- `tests/testthat/test-B-T06.R`
- `inst/examples/tickets/B-T06.yaml`
- `docs/implementation/registrations/B-T06.json`

A fresh Luna max worker writes the RED cross-packet tests only and may modify:

- `tests/testthat/test-M4-005-stage-b-composition.R`

Because root policy reserves shared package/CLI registration to Sol, Sol alone
may then modify:

- `R/stage_registry.R`
- only the `.onLoad`, dispatcher-installation and source-CLI wrapper portions of
  `R/multiplicity.R`

The B-T06 family, nomination and rank-statistical functions are frozen to the
restored candidate. Sol must not alter `R/rank_sensitivity.R` or any B-T04
statistical/calibration code.

### Required failing tests before implementation

- Installed-package startup must expose one idempotent composition that retains
  the B-T02, B-T03, B-T04, B-T05 and B-T06 dispatchers and the B-T04 stage hook.
- Source CLI execution must preserve the same composition rather than capture
  the older B-T01 entry point.
- A qualified B-T04 repeated/trajectory request must still resolve and fit
  through B-T04 after B-T06 is present.
- B-T05 missing-count unavailable and explicit-fallback requests must still
  enter B-T05 after B-T06 is present.
- B-T06 fixed-family accounting and the accepted scale-invariant Wilcoxon
  regressions must still pass.
- Re-running either installer must not double-wrap or change behavior.

### Unit B acceptance

- B-T04, B-T05 and B-T06 packet and cross-packet suites pass without skipped
  required tests; source and installed synthetic CLI probes exit successfully.
- A fresh Sol 6.1 high reviewer accepts the complete diff from the Unit A
  integration commit, including restored packet files and Sol-owned
  registration changes.

## Attempt, review and stop limits

Each M4-005 unit has one initial implementation and at most one targeted repair.
Each has one initial fresh Sol review and at most one focused rereview. These are
new shared-integration counters; the exhausted B-T05/B-T06 packet counters remain
unchanged. A repair may address only findings inside that unit's listed files.
Any required foreign-file or scientific-contract change blocks the unit and
M4-005; it does not authorize another packet repair.

After both reviews pass, Sol integrates serially, runs the named adjacent tests,
the complete local suite and `Rscript --vanilla tools/check-package.R`, records
exact counts/exits/skips, and creates an immutable M4 snapshot. A fresh Astra low
then performs the normal M4/G4 review. B-T07 may start only after Sol accepts
that review. No result from this record is biological validation or a real-data
analysis.

## Alternatives rejected

- A minimal pair of wrapper patches leaves the competing source-order startup
  architecture in place and is likely to regress when another Stage B packet is
  added.
- Expanding the generic artifact envelope with method-specific fallback statuses
  would mix execution completeness with analytical provenance and widen every
  downstream consumer contract.

## User decision

Omar approved this design on 2026-10-15. That approval permits this record and
its independent audit to be prepared. Coding, candidate restoration and new
review allowances require Omar's explicit acceptance after the audit passes.
