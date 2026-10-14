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

## Cross-cutting registration review

Initial verdict: `changes_required`.

The fresh reviewer independently passed 80 B-T09 assertions, 139 B-T08
regression assertions, 25 schema probes, package installation, both public-
fixture CLI paths and the namespace export check. One P1 defect remained:
non-fixture longitudinal modes failed closed in the installed CLI but source-
tree execution bypassed the guard, exited 0 and wrote Stage B output while
silently omitting the clinical objective.

Sol repair 1 moves the non-fixture longitudinal guard outside the installed-
namespace condition while leaving source-fixture preparation to the existing
packet hook. A regression now requires source execution to return
`B_DESIGN_UNSUPPORTED` before analysis when the eligible endpoint bundle and
clinical view are unavailable. The B-T09 suite passes 83 assertions after this
repair.

Focused registration re-review verdict: `accepted`.

The reviewer independently passed 83 B-T09 assertions, 139 B-T08 regression
assertions, and all 13 CLI probes. All six non-fixture source/installed cases
returned `B_DESIGN_UNSUPPORTED`, exit 2, before creating output. Both public-
fixture paths completed with effect 2.036438 across 11 subjects, and ordinary
between-person behavior remained available.

Fresh integrated verification then passed the full 2,404-assertion suite with
no failure, warning or skip. The first package check found only missing Rd
entries for the three new exports; Sol added them to the shared API manual. The
fresh rerun reported `Status: OK`. Build/check log SHA-256 values are
`d8b558aa37e27eca723b52ece7974568c614682c5a24af4ae8297b9f5d4d1ab4` and
`c92dad89ffdb5a342bd8247c7f177d5da21dbdd09520833b2751a54a460b644e`.

B-T09 is accepted as synthetic software evidence. It does not establish a
longitudinal biological association in Omar's studies.
