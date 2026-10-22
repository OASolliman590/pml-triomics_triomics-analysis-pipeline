# M6 Sol acceptance and G6 release

Date: 2026-10-22

Acceptance snapshot:
`2150b4dffd174abd08a38389467f91e9d5c73e54`

Frozen spec SHA-256:
`f0d482db62f16c8e4c802c99c1fd88e501f29959bc475d67f934b40c6f6ae5d2`

## Decision

Sol accepts M6 and releases G6. Stage D is intentionally not started in this
acceptance run.

The decision rests on the previously completed clean archive package gate at
implementation snapshot `76fe5e2` (`Status: OK`; FAIL 0, WARN 22, SKIP 0,
PASS 4787), the canonical DOC-M6 record and durable evidence receipts, and the
single focused Astra-low rereview at `2150b4d`, which passed with
`M6-ASTRA-01: ADDRESSED` and no new blocking finding. The package gate, DOC-M6
work, and Sol document review were not repeated.

## Packet disposition

- C-03, C-04, C-05, C-08, and C-06 are accepted on synthetic software
  evidence within their recorded limits.
- C-09 remains a conditional synthetic fixture only. Its implemented fixture
  is integrated and verified, but the packet is deferred without completion
  credit because no real endpoint/model/deployment and uncertainty protocol
  was supplied. `benefit_guaranteed=false` remains authoritative.
- C-07 controls remain `not_run`; the C-06 handoff truthfully records that
  unavailable evidence and supplies no control metrics.

## Scientific boundary

This acceptance establishes software readiness for the frozen M6 core gate. It
does not establish biological validity, clinical performance, real-study
fitness, unseen-subject MOFA prediction, C-07 control performance, or C-09
benefit. No private study data were read or fitted. G6 release authorizes the
later Stage D transition but does not itself start any Stage D packet.

Evidence: [clean package gate](2026-10-21-m6-clean-package-gate.md), [initial Astra review](2026-10-21-m6-astra-review.md), [focused Astra rereview](2026-10-22-m6-focused-astra-rereview.md), and [canonical M6 record](../milestones/M6.md).
