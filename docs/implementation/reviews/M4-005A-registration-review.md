# M4-005A dependency-registration review

- Date: 2026-10-15
- Reviewer: `/root/m4_005a_registration_review` (requested `gpt-6.1-sol`, high)
- Final candidate: `7876df4`
- Verdict: **PASS after one focused repair and rereview**

The initial review accepted the optional `DEqMS (== 1.30.0)` declaration and its
exact local Bioconductor 3.23 lock record, but found the required ggplot2 chain
incomplete. The bounded lock-only repair added exact installed records for
`ggplot2`, `gtable`, `isoband`, `S7`, `scales`, `farver`, `labeling`,
`RColorBrewer` and `viridisLite`. Recursive non-base Depends, Imports and
LinkingTo closure then reported zero missing requirements.

The focused rereview found the original issue addressed with no new Critical or
Important finding. A clean source archive subsequently passed build/check with
`Status: OK`, installed without warnings, and ran the installed B-T05 CLI with a
valid unavailable envelope that retained the named missing-count reason. No
statistical behavior or private study data changed. This review does not by
itself accept B-T05; the shared M4-005 review remains required.
