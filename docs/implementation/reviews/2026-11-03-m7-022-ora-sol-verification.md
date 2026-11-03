# M7-022 — measured-universe ORA verified and integrated

Sol independently verified immutable worker36abc874da479b4c6397d598056461db730d9c26.
Focused command: Rscript --vanilla /tmp/triomics-stage-d-evidence/run-focused.R
/tmp/triomics-m7-022-luna 'D-T04|D-ora-shared' <sol-focused.json>.
Exit0; PASS122, FAIL0, ERROR0, WARN0, SKIP0,12test blocks. Fresh Astra initial
review required duplicate/type validation; its one focused rereview passed this
candidate. Two implementation repairs consumed. No further M7-022 repair remains.

Luna's prior affected regressions at225529a: foundation80, D01=325, D02=87,
D03=62; total554pass,0fail/skip,1named warning: D-T01 refuses stale paired signs
and preserves the old pointer (expected system2 status2). The final parser repair
is ORA-only; those counts are attributed to their actual commit, not relabelled.

Sol then ran fresh native B,C,D with inst/examples/tickets/D-T04.yaml, each exit0.
Persistent root: /tmp/triomics-stage-d-evidence/m7-022/sol-native/D-T04.
Readback proves10unique measured entities from10original features, complete
three-set family,1estimable/1empty/1ineligible, N/K/n/k=10/3/1/1 and p=q=0.3.
Python independently recomputed the upper tail using exact binomial coefficients.
Persisted universe and set-family canonical SHA256 values were recomputed; actual
protocol/mapping bytes matched bindings. All four B predecessor hashes match C,
and D manifest/member SHA256 and sizes match the pointer. Native generation:
`9fe127efd5e9c5b6ef24cc2eff6d673d8c39a9baef6b7a1b7ae1f0c4a38b3cd0`.

- `B/handoffs/984aa67506ab08f7605a308304d40b900942e996a8104080be1c5e2a3e33b7a0/stage-handoff-provenance.json`: `d619bb49d27a932aa6dfe5df3f5903a4ad04504d518dd47acec1adf46d4543c4`
- `D/stage-handoff-provenance.json`: `521919083a3b0753e56681dba63ddfe6c6266f14b3288b8f0131ebed0356124d`
- `D/generations/9fe127efd5e9c5b6ef24cc2eff6d673d8c39a9baef6b7a1b7ae1f0c4a38b3cd0/generation-manifest.json`: `9fe127efd5e9c5b6ef24cc2eff6d673d8c39a9baef6b7a1b7ae1f0c4a38b3cd0`

Full command/exits/hash inventories and numeric readback are in
../evidence/2026-11-03-m7-022/. Source stayed clean. Allowlisted diff reviewed;
no CLI/NAMESPACE/manual/schema/statistical changes. Integrated through ec8876a;
R/tests/inst/man/NAMESPACE/DESCRIPTION trees must match the tested candidate.
D05 dependency is verified for implementation; D04 and M7/G7 are not accepted.
Final same-snapshot gates still required. C07 not_run; C09 unqualified; no private
data, push or biological-validity claim.
