# M7-014 — C provenance seam verified and integrated

## Result and authority

The requested publication split is implemented and independently verified on
public synthetic fixtures. C-08 publishes endpoints and schedules B without
executing it or publishing a D handoff/C provenance. C-06 publishes its D
provenance pair only after a real native B frame in the same output root.
Missing B fails closed with no partial C frame.

User authorized fixture/ticket label and C writer/caller decisions for this run.
Existing Astra PASS at `4e5ad08` was reused as instructed; no repeat audit ran.
M7-011, M7-012 and M7-013 remain incomplete. No milestone acceptance implied.

- Worker: fresh `/root/m7_014_luna`, requested gpt-6-luna/max, no worker commit.
- Worker base: `79003fd166c7aa96c0af270c6fb775cc1178834f`.
- Sol-reviewed immutable candidate: `18b6e9042d1df606bc9d239ef63250638385dd93`.
- Integration: `359a1d0e53bfcf2fa2e836434e63222e9641a679`.
- All eight file blobs at integration match the verified candidate exactly.
- No post-return repair was required; the two authorized Sol repair slots were unused.
- Worktree retained: `/Users/megastore/.codex/worktrees/m7-014-c-provenance-completion/PML MultiOmics Analysis Pipeline`.

## Files and coordinator decisions

Changed: `R/export_c.R`, `R/cli.R`, `tests/testthat/test-C-06.R`,
`tests/testthat/test-C-06-shared.R`, `tests/testthat/test-C-08-shared.R`,
`inst/examples/tickets/C-02.yaml`, `inst/examples/tickets/C-06.yaml`, and
`docs/implementation/registrations/C-06.json`. No extra fixture file needed.
The old M7-013 RED patch was not integrated as a finished change.

- C-02 analysis preparation label now matches its existing `c02_fixture` blocks.
- C-06 uses existing `per_unit_beta` vocabulary. Its declared score slope is
  encoded as `{intercept: 0, continuous.score: 1}`, matching existing producer
  column names; no estimand, numerical expectation, data or threshold changed.
- C writes `registries/run-C.json`, preserving B's immutable `run.json`.
- The writer validates four real B files and their identity/member evidence,
  checks source and prepared C matrices using their respective existing hash
  conventions, and records C preparation/native state and runtime provenance.
- Failed publication cleans its newly created artifacts. Endpoint-only requests
  refuse a conflicting existing D publication before writing C.
- Numerical assertions, including five nominations in the builder fixture,
  remain; writer privacy/lineage assertions now use real native predecessors.

Sol's review caught and resolved during initial worker development the nullable
schema-key deletion, positional method/block scope, integration-state variable
shadowing, matrix hash convention, and empty-directory cleanup defects. The final
source/schema/D/statistical-method boundaries were inspected and unchanged.

## Independent Sol proofs

Run directory: `/private/tmp/triomics-m7-014-sol/proof-18b6e90`.
All commands ran against the frozen candidate in its worktree. No results below
are substituted from Luna's reported passes.

1. Native C-02 Stage B: CLI exit 0; persisted B bundle, both envelopes and
   provenance validated by existing validators. Full hashes are below.
2. C-08-shared: **FAIL 0 / WARN 0 / SKIP 0 / ERROR 0 / PASS 64**.
   Fresh C-08 CLI exit 0; endpoints and request envelopes validate. B contains
   only scheduled clinical-score request files, with owner B and model_execution
   not_started; no B results/handoff/run record, no C provenance or D manifest.
3. C-06: **FAIL 0 / WARN 0 / SKIP 0 / ERROR 0 / PASS 347** (226.6 seconds).
   Fresh native Stage B then Stage C: exit 0/0. The four B file hashes captured
   before C are unchanged after C and equal the C profile's predecessor digests.
   Persisted C JSON passes `triomics_validate_stage_handoff_provenance()`;
   its envelope passes the existing envelope validator.
4. Additional negative CLI proof: C-06 without B exits 2 with
   `C_EXPORT_PREDECESSOR_MISSING`; no provenance pair or D manifest exists.
5. Read-back: four C-07 controls not_run; C-09 not_started; D aggregate partial;
   separate truthful B and C run records. C-06 accepted=false throughout.

Named tests used `Rscript --vanilla`, `testthat::test_local` with filters
`^C-08-shared$` and `C-06`. Sol's runner disables the early failure cutoff,
collects full counts, then returns nonzero for any failure/error/skip.
Native commands used `inst/exec/triomics --config inst/examples/tickets/C-02.yaml
--stage B`, `C-08.yaml --stage C`, and `C-06.yaml --stage B` then `--stage C`,
each with its fresh output directory. Exact scripts and raw logs are in
`evidence/2026-10-26-m7-014/`, with SHA256SUMS. These are focused source proofs,
not a full-source suite, clean package gate, milestone audit or biological validation.

The first Sol C-08 read-back incorrectly required no B directory. The existing
schedule writes its unexecuted request under B; the checker was corrected to
require only those exact request files and no B results/handoff. The initial
checker failure is retained. No product change or passing test was altered for it.

## C-06 exact persisted hashes

Each path is an actual artifact. The first four are the unchanged native B
predecessors found in the validated C profile; the last two are the C pair.

| Artifact path | SHA-256 |
|---|---|
| `/private/tmp/triomics-m7-014-sol/proof-18b6e90/c06-bc/B/handoffs/a69080febe9558c754f28b83c9b10d157f5fe6d78eeeb3a0cd9213a088b38b88/bundle.json` | `932a6e25f3cfc3abf7f99f9dce9e3f0e69fd87cd8559522ada7ed0868ff92e98` |
| `/private/tmp/triomics-m7-014-sol/proof-18b6e90/c06-bc/B/handoffs/a69080febe9558c754f28b83c9b10d157f5fe6d78eeeb3a0cd9213a088b38b88/bundle.envelope.json` | `372a2eeecc571e79aa6dcc604ab905afa382cac5c29ee12c2793bda7e84fc37b` |
| `/private/tmp/triomics-m7-014-sol/proof-18b6e90/c06-bc/B/handoffs/a69080febe9558c754f28b83c9b10d157f5fe6d78eeeb3a0cd9213a088b38b88/stage-handoff-provenance.json` | `c8584706b3e4a2b960ac131fde921b705308b72dc35766035976766e6771a788` |
| `/private/tmp/triomics-m7-014-sol/proof-18b6e90/c06-bc/B/handoffs/a69080febe9558c754f28b83c9b10d157f5fe6d78eeeb3a0cd9213a088b38b88/stage-handoff-provenance.envelope.json` | `02ea6d4fae2672e70d0fa2482ede980b38118c2d83504cf36f61dbd13e142853` |
| `/private/tmp/triomics-m7-014-sol/proof-18b6e90/c06-bc/D/stage-handoff-provenance.json` | `75e83c37328136d80273b3347a725564a164d54739293ca87a84934f2b41bcfa` |
| `/private/tmp/triomics-m7-014-sol/proof-18b6e90/c06-bc/D/stage-handoff-provenance.envelope.json` | `d0e7dfb29f0af4e50d2da99abac42f9e71d43fdd8c5019ebaeb54c37a4b66ea9` |

## C-02 native B proof hashes

| Artifact path | SHA-256 |
|---|---|
| `/private/tmp/triomics-m7-014-sol/proof-18b6e90/c02-b/B/handoffs/00a4131da70afb2181db63a405d4d734672da904b01f1641f664cb241f5e77e1/bundle.json` | `23178c304a754a6287646f615911a9662edcb44f44f9fb4dfd43fa080dfb2a7a` |
| `/private/tmp/triomics-m7-014-sol/proof-18b6e90/c02-b/B/handoffs/00a4131da70afb2181db63a405d4d734672da904b01f1641f664cb241f5e77e1/bundle.envelope.json` | `9c476d731f82e7e1e75c1a6bedde5684a54b973bf8d595fb85dcb15b32cf2dce` |
| `/private/tmp/triomics-m7-014-sol/proof-18b6e90/c02-b/B/handoffs/00a4131da70afb2181db63a405d4d734672da904b01f1641f664cb241f5e77e1/stage-handoff-provenance.json` | `57388cb8abe1e729f854dc27e6b379a368baf041a77b16c0b2df19d58eff815a` |
| `/private/tmp/triomics-m7-014-sol/proof-18b6e90/c02-b/B/handoffs/00a4131da70afb2181db63a405d4d734672da904b01f1641f664cb241f5e77e1/stage-handoff-provenance.envelope.json` | `d15df4cf49987f5938bb5e0eb8d7968d66e26b22a0868d8ec72c6c4e89401a2c` |

## Source-comparison limitation and approval review

Automatic approval review rejected adding a Stage C call to
`triomics_b_provenance_source_files()` because it would reread/hash configured
source and metadata files under the user's no-private-data instruction. The
rejected helper was not added or invoked through another route. The safer
implementation compares already-ingested C block hashes and canonical normalized
metadata/sample registry/factor levels to validated B identities. The B profile
retains its source-file hashes and its exact bytes are a C predecessor.
This proves the consumed parsed metadata agrees, without claiming a fresh raw
C metadata-file byte rehash. Mutation tests cover actual C matrices and metadata.
All actual execution here used repository public synthetic fixture files.

## Close and literal resume

This bounded seam is verified and integrated; M7 itself is not accepted.
C-06 accepted=false; C-07 not_run; C-09 unqualified. M7, G7 and Stage D remain
closed. M7-011/M7-012/M7-013 stay incomplete. No private study data read, no
GitHub push and no Notion sync. No active worker or proof process remains.
The worker worktree is retained to preserve the immutable proof context.

Resume from this receipt and the execution ledger. Do not repeat the completed
label repairs, M7-013 audit, or these focused proofs merely to recover context.
Any next milestone acceptance or downstream Stage D transition needs its own
explicit authority; this run releases neither. The earlier independent spec
validator dependency limitation is unchanged; no new spec-validator pass claimed.
