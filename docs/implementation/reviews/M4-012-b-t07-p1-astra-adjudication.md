# M4-012 B-T07 P1 — Astra-low contract adjudication

- Date: 2026-10-16
- Adjudicator: `/root/b_t07_p1_astra_adjudication`
- Immutable candidate: `a893475`
- Scope: read-only interpretation of the repair-2 Sol finding
- Verdict: **OVERRULE P1; P2 traceability ambiguity at most**

M4-012 authorizes a non-error unavailable disposition when the completed Stage B
result is typed unavailable and contains no exportable public block claims. It
does not require `requested_method`, `deqms_status` or a nonempty method-specific
reason for that non-release path. The candidate verifies the payload/envelope,
configuration, exact returned payload path, both unavailable statuses, empty
blocks and an unavailable aggregate disposition. It writes no Stage C handoff.

After the reviewer's field stripping, the fallback disposition is
`method = "unspecified"`, `status = "unavailable"`, `reason = NULL`; it does not
claim that an unspecified method is DEqMS, produce a silent fallback or release
inference. B-T07 AC2 still separately executes and verifies the actual unavailable
DEqMS smoke. The missing method identity and reason are a diagnostic/traceability
ambiguity, but not a frozen scientific-contract correctness failure in this
guard. No code change or extra repair was authorized by this adjudication.

Coordinator disposition: retain both reviews, carry the P2 ambiguity into the M4
review evidence, integrate the bounded candidate, and continue the complete
shared-diff and immutable milestone gates. B-T07 remains unaccepted until those
gates pass.
