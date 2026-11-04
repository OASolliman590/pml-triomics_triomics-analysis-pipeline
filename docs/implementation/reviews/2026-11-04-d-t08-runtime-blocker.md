# D-T08 preflight receipt: Quarto unavailable

Source baseline: `45d08c1a1fe3ef2634533b45f5837c03ae434d80` on `spec/harden-v2-20260920`. M7 and G7 remain accepted on product fabd6fa. This is a runtime blocker before implementation, not a failed coding attempt or a milestone audit.

The authorized goal requires an actual self-contained offline Quarto HTML render and explicitly says: "If Quarto is absent, stop and record that. Do not write fake HTML."

Fresh command `quarto --version` exited **127** with `zsh:1: command not found: quarto`. PATH resolution returned no executable. Nine standard system, user and RStudio-bundled executable locations were absent. No Quarto/RStudio/Posit application directory was found in the checked application location. These checks establish that Quarto is unavailable on the checked host; they do not assert an exhaustive search of every custom directory.

[Runtime preflight](../evidence/2026-11-04-d-t08-preflight/runtime-preflight.json) records the command, exit, timestamp, objective hash and actual internal APFS evidence filesystem. Its SHA-256 is `f2ef203752c0f3dcae9906af8ee3cee45f917bd068de71e8f0511863a35a8a22`. [Decision trail](../evidence/2026-11-04-d-t08-preflight/decisions.tsv) records this transition.

D-T08 remains **unaccepted, blocked before implementation**. Initial attempts used: 0. Repairs used: 0. No Luna was dispatched, no test suite or render ran, no HTML was produced, and no product files changed. No test counts or render hashes can be claimed. No M8/M9 Astra audit was consumed. D-T13, D-T15, D-T09 and both milestone closes remain unstarted; G8/G9 remain unreleased. The overall objective remains active and incomplete.

The stop is required by the user objective, not by a new approval rule from a skill. The sequence-verifiable-units principle keeps D-T13/D-T15 behind D-T08. The prove-it-works principle requires the real render rather than a mocked or fabricated output. The existing packet plan is retained; no replacement plan, scientific method or schema was created. The wizard skill was read but no wizard or installer was launched because this run must stop at the absent runtime.

Resume after Quarto is installed or its existing executable is made available: verify `quarto --version`, confirm the unchanged M7 acceptance and current ledger, then dispatch one fresh Luna-max for the exact D-T08 files_allowed and test-first acceptance criteria. Use internal APFS outputs. Continue the existing packet order and stop at each receipt. Do not rerun or reopen M7, read private study workbooks or push.

## Traceability review

Fresh read-only Terra `/root/d08_runtime_receipt_terra`, requested gpt-6.1-terra/max, compared the receipt and decision trail to the raw probe and this turn's transcript. Effective model settings were not independently exposed. The probe exit127, nine missing executable locations, APFS evidence location and zero-attempt state matched. Terra found that the handover said "committed here" before commit; Sol corrected it to "recorded here." This documentation check consumed no M8/M9 milestone audit or implementation attempt.

A broad whitespace check included the pre-existing dirty renv/activate.R and reported its whitespace. Sol left those bytes unchanged; the check scoped to this run's coordinator documents passed. No runtime or product repair was attempted.
