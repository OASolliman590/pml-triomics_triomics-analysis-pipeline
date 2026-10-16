# M4-005B post-review coordinator gate

- Date: 2026-10-16
- Candidate: `ada3a17adebc388850321190bfdbcc9fb64b1152`
- Fresh Sol review: **PASS**, zero Critical/Important/Minor findings
- Coordinator disposition: **BLOCKED; do not integrate**

The targeted and adjacent filter passed 732 assertions with zero failures,
warnings or skips. The reviewer independently repeated that result, installed a
clean committed archive, ran eight source/installed CLI probes and verified the
frozen B-T04/B-T05/B-T06 surfaces. Those results remain valid but were not the
complete-suite gate.

The required complete local suite then reported 3,130 passes, six failures,
zero warnings and zero skips. Four failures were in B-T01 and two in B-T03.
With B-T06 finally active, missing selection inventory correctly prevents q
publication, contradicting three predecessor expectations. Three additional
assertions showed B-T06's family validation overwriting established row statuses
with `withheld` rather than retaining `not_estimable`, `descriptive_only` and
`unverified` while withholding only family validation and published q.

Package build/check and integration were not attempted after the failed suite.
No private study data were read. Candidate `ada3a17` remains isolated and B-T05,
B-T06, B-T07 and G4 remain unreleased.
