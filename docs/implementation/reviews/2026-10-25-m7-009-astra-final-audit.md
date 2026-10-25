# M7-009 Astra final contract audit

Snapshot: `8d12c67`  
Candidate inspected: `8745f22a2fddaef9a98a49ce646efe57b1f40245`  
Date: 2026-10-25  
Verdict: pass; contract assurance only

Critical: 0. Important: 0. Minor: 0. `audit_pass: yes`.

The final record defines ordered refusal precedence, preserves
`hash_mismatch`, separates physical presence from base validity and required
evidence completeness, permits only a self-consistent explicitly partial
diagnostic publication, and permanently refuses reuse of that bundle as
`incomplete`. Complete/reusable claims require every evidence check and retain
`B_EXPORT_INCOMPLETE` on post-write failure. The change remains bounded to
B-T07 and retains the existing AC3 expectation.

Implementation is limited to `R/export_b.R`, `tests/testthat/test-B-T07.R`, and
`docs/implementation/registrations/B-T07.json`. It must exercise overlapping
hash/evidence failure and complete-claim post-write failure, leave Stage A and
the frozen spec/schema untouched, and preserve every fail-closed validator.
This audit does not establish implementation correctness, accept M7, or
authorize integration without focused verification and review.
