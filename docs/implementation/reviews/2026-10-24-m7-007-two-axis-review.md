# M7-007 two-axis code review

- Date: 2026-10-24
- Product fixed point: `e4aed594877ef53ace6958d9a757cffb471ba53a`
- Candidate commits: `0b1fcbdbb321a8e6caa002d19e769d693d53d2bd`,
  `b34df9f5d93ca60d74f690a2e86e5f416547fec6`
- Reviewers: `/root/m7_007_standards_review`, `/root/m7_007_spec_review`
- Product edits or tests by reviewers: none
- Overall disposition: **ready to integrate**

## Standards

Verdict: **pass**. Critical 0, Important 0, Minor 1.

No documented-standard violation was found. The two worker commits change
exactly the three M7-007 allowlisted files; the worktree is clean. Tests exercise
the public validator, check schema validity, refresh identity, assert the typed
rejection, and preserve a legal escaped-identifier multiple-context positive.
There is no warning suppression, skip, schema restriction, private input,
package registration, claim inflation, or Stage-D implementation. Registration
remains `candidate/review_pending` and `accepted: false`.

The one nonblocking judgement-call Minor is duplicated test setup: the new
`m7_003_test_add_covered_context()` helper repeats role cloning, sorting, and
generated-ID maintenance still present in an older positive test. Refactoring
that pre-existing branch is not required to close this bounded defect and would
add unnecessary churn.

## Spec

Verdict: **pass**. Critical 0, Important 0, Minor 0.

The grouping helper encodes the component count and then, for each exact UTF-8
value, its decimal byte length, a colon, and the bytes. The count fixes sequence
length and each length prefix fixes the next component boundary, so embedded
control characters, digits, colons, or multibyte values cannot create the prior
tuple collision. `enc2utf8()` does not normalize Unicode, and the schema remains
unchanged.

The negative public fixture reproduces the distinct contexts cited by Astra,
refreshes the identity digest, confirms schema validity, and requires
`D_PROVENANCE_INVALID`. The positive fixture retains U+001C and provides every
required role for a second context. Existing context, migration, route,
cardinality, and canonicalization controls remain intact.

## Disposition

Both axes return `ready_to_integrate: yes`. Sol may serially integrate the two
candidate commits, verify exact blobs, rerun the focused suite, and run one full
source suite. This review does not accept M7-004/M7-006/M7-007 or release any
Stage-D gate. A fresh immutable Astra-low acceptance audit remains mandatory.
