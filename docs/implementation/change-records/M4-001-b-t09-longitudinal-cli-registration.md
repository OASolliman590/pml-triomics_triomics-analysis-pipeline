# M4-001 — register frozen B-T09 longitudinal objective fields

Date: 2026-10-14
Owner: Sol
Status: implemented pending independent registration review
Affected packet: B-T09

## Reason

The frozen B-T09 contract requires within-person, baseline-to-later and effect-
modification objectives, but the canonical closed configuration schema did not
admit the already-specified visit, horizon, estimand, adjustment or modifier
fields. The initial B-T09 review therefore found that the source CLI could not
deliver those declared modes safely. This is a delivery correction for frozen
scope, not a new analysis method or a change to the scientific estimands.

## Authorized registration

- Add `baseline_visit_ref` and `later_visit_ref` for within-person objectives.
- Require both visit references, positive `horizon_days`, boolean
  `baseline_adjustment`, and `outcome_estimand` (`later_level` or `change`) for
  baseline-to-later objectives.
- Require `modifier_ref` for effect-modification objectives.
- Export the three accepted B-T09 public API names.
- Wire installed-package CLI delivery for the public synthetic B-T09 fixture.
  For real-study longitudinal modes, fail closed with typed unavailability
  until an eligible endpoint bundle and clinical view are available. Never
  rewrite the objective or substitute the synthetic fixture.

## Evidence required

The B-T09 packet suite must prove schema acceptance and conditional refusal,
the exact source CLI must remain green, a package namespace must export the
accepted APIs, and an installed-package CLI probe must either run the public
fixture or refuse unsupported real-study inputs before analysis. A fresh Sol
6.1 high reviewer must accept this cross-cutting registration before B-T09 is
released to milestone review.

This record does not validate Omar's studies, qualify a biological result, or
alter B-T04's seeds or null bound.
