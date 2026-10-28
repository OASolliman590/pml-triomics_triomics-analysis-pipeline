# Tri-omics progress

The missing-time defect is fixed: excluded sample SYN_12 now saves its unavailable
time as JSON null. Its excluded status and lack of a score remain intact.

A fresh Luna worker changed one line; Terra reproduced the original failures and
reviewed the repair. Independent synthetic B → C → D commands succeeded, with
21 passing assertions and matching provenance hashes. The focused suites passed
1,176 assertions, with one named warning from an intentional rejection test.

The broader checks found remaining problems. The full source suite passed 6,280
assertions and failed one: paired visit weights changed their in-memory type.
The installed package had 45 failures/errors, including missing Git revision and
fixture-path problems. Its final status was 1 ERROR, 1 WARNING, 1 NOTE. The 42
installed warnings are not fully named in the report and remain uncertified.

Astra did not approve the snapshot. The repair is integrated, but M7 and G7 stay
closed; no later D packet was started. C-06 remains unaccepted, C-07 unrun and
C-09 unqualified. This is software evidence from synthetic fixtures, not scientific
validation on your studies. No private data were used or uploaded.

Next: a new bounded compatibility and installed-package repair cycle. This cycle
has stopped with all workers finished and a resumable handover. Local records only;
no GitHub push or Notion sync.
