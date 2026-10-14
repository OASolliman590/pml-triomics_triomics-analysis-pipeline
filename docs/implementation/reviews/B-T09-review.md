# B-T09 packet and registration review

Date: 2026-10-14
Packet: B-T09 — longitudinal clinical change and interaction associations
Initial candidate: `be1b52df2777e4cefbce6b23e9d10e536a6ca082`
Repair candidate: `0ca50369e2fb52c819a54abdfc8ff4480d9efc88`
Reviewer: fresh Sol 6.1 high

## Initial review

Verdict: `changes_required`.

The initial candidate stayed within its four-file allowlist and passed 54 named
assertions plus its synthetic CLI. Independent probes found three defects:
measurement timestamps were discarded, a within-person CLI request could be
reported as between-person, and an omitted outcome estimand crashed rather than
returning typed unavailability.

## Focused re-review

Verdict: `accepted`, subject to Sol-owned schema, namespace and CLI registration.

Repair 1 passed 73 named assertions, four adversarial API cases with 13 checks,
and the exact synthetic CLI. Later predictors and adjustments now fail closed,
horizons remain truthful, and a missing estimand returns typed unavailability.
A schema-valid within-person source-CLI probe exited 2 with
`B_DESIGN_UNSUPPORTED` before analysis, without output or relabeling. The total
packet diff remained inside the four-file allowlist.

## Sol registration evidence

Change record `M4-001` registers the frozen longitudinal fields without adding
scientific scope. The canonical schema now enforces mode-specific fields; the
three public APIs are exported; and installed-package CLI delivery runs the
public fixture while real-study longitudinal modes fail closed until an eligible
endpoint bundle and clinical view exist.

Fresh coordinator checks after integration:

- B-T09 suite: 80 passed, 0 failed, warned or skipped;
- source CLI: exit 0 in a fresh output root;
- temporary installed package: install succeeded; installed CLI exit 0;
- installed acceptance record: `software_fixture_pass`, effect 2.0364 across 11
  subjects;
- installed namespace exposes all three requested APIs.

The Sol-owned cross-cutting registration still requires an independent fresh
Sol review before B-T09 is released to milestone review. All evidence is public
and synthetic; it is not real-data scientific validation.
