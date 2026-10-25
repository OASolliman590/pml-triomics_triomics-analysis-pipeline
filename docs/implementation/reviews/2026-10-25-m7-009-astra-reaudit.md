# M7-009 Astra focused re-audit

Snapshot: `87d79c9`  
Date: 2026-10-25  
Verdict: changes required; implementation prohibited

Critical: 0. Important: 1. Minor: 0.

The clarified refusal precedence, `hash_mismatch` preservation, three separate
validation facts, B-T07 scope, and retained AC3 `incomplete` expectation passed
review. One post-write sentence remained overbroad: it prohibited every
incomplete candidate from finalizing, while AC3 intentionally publishes a
self-consistent diagnostic bundle with declared status `partial` before proving
that it cannot be reused.

The required clarification is to distinguish successful publication of an
intentionally partial diagnostic bundle from publication as a complete reusable
handoff. Diagnostic partial publication may succeed after base-pair validation
but must never be reused. A complete/reusable claim requires all member, late,
and provenance evidence and fails with `B_EXPORT_INCOMPLETE` when that evidence
is missing or invalid.

No implementation was authorized by this verdict.
