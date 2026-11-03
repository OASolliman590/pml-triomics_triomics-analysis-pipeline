# M7-027 initial implementation audit

**audit_pass no**, immutable f9abc42713387c3b5e33ba87766eea5d150e7222.
Fresh read-only /root/m7_027_implementation_astra, requested gpt-6-astra low;
effective settings not independently exposed. No edits, fits or broad tests.

1. P1: R/target_alternatives.R:136–148 exempts only binding/Kd. Legitimate matched
   sourced binding/Ki and function/activity still throw D_TARGET_CLAIM_COLLAPSED.
   Sol independently reproduced both; Kd comparison succeeds. Exact probe and
   JSON: /tmp/triomics-stage-d-evidence/m7-027/sol-review/external-endpoints-probe.R
   and external-endpoints-initial.json. This is a diagnostic, not native acceptance.
2. P1: an attribute on the binding axis is erased before scanning at lines137–139.
   Astra confirmed that attr(record$axis, "binding_probability") <- 0.9 survives
   typed-ledger validation and the profile still compares as comparable. The
   endpoint-attribute variant correctly rejects; only the axis bypass is confirmed.

Canonical coverage now fails closed for unequal/incomplete coverage, and unresolved
external records prevent comparability. No other concrete P0/P1 found. Initial
focused341 passed with zero fail/error/warning/skip, including the previously
appended fixtures, but native B/C/D verification is pending. Sol requests one
combined repair1 for these findings within the approved allowlist, then focused
and native proof before the single focused Astra rereview. One repair remains
after repair1; do not reset the cap or accept M7/G7 from this record.
