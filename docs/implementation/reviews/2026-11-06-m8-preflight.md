# M8 preflight and documentation review

Base: `4cba2581d4e70558981b8a02efcb4653cc9a5b01`. D-T08/D-T13/D-T15 accepted records and G7 were restored without rerunning packets. Terra DOC-M8 commit `509fe3dcf05622a802ef95077ed5d070945a4e2c` changes only the owned milestone note; Sol checked its seven links, shell syntax, command prerequisites and evidence scope. Two pre-handoff wording corrections distinguish absent producers from partial sections and propagate failed shell stages. No post-handoff documentation repair was needed. Requested Terra 6.1 max; effective settings were not exposed.

Existing runtime verified: R 4.6.1, testthat 3.3.2, MOFA2 1.22.1, DEqMS 1.30.0, reticulate 1.47.0, jsonlite 2.0.0, Python 3.12.14, mofapy2 0.7.5 and Quarto 1.11.9. Package and scientific dependencies were not changed. A separate local Python environment supplies jsonschema 4.26.0 and PyYAML 6.0.3 for the specification validator.

The raw-checkout spec validator first lacked those dependencies, then encountered an untracked AppleDouble file on the SSD. The clean-worktree invocation actually ran and exited **1**: `Frozen content changed: AGENTS.md`. Enumerating every frozen entry found exactly two mismatches: `AGENTS.md` and `CLAUDE.md`. Both are byte-identical to accepted M7 product `fabd6fa6bb7ae06f6eea1276517010829ea4cde1`; their last change is `b585b2620e3a5d357bf29faf7c92fc61861a06ec`, which appended progress-reporting and completion-handoff instructions. Every other frozen entry matches.

This is an inherited delivery-manifest discrepancy, **not a validator pass**. No frozen file, expected hash or validator was edited. The separate frozen-kit independent audit remains pending. Fresh M8 Astra must assess this disclosed limitation together with the immutable package gate and milestone evidence. Product/scientific contracts are unchanged from the accepted base.

Package gate and Astra outcome are pending at this preflight record. G8/G9 stay closed. No private study inputs, AIU run, GitHub push or Notion sync.
