# RDY-001 — reconcile the current frozen-spec review subject

Status: proposed reconciliation, frozen for one fresh Astra-low audit. No new product implementation or study fitting is authorized by this record alone. The user authorized this bounded readiness programme on 8 November 2026.

## Immutable history and exact discrepancy

- `b56e942` records G0 acceptance of content `f0d482db62f16c8e4c802c99c1fd88e501f29959bc475d67f934b40c6f6ae5d2` after the resource-ownership correction and focused Astra pass. All92 content hashes matched at that commit. This historical audit exists; it is not an audit of every later change.
- `b585b2620e3a5d357bf29faf7c92fc61861a06ec` added progress-reporting and completion-handoff instructions to AGENTS.md and a reporting pointer to CLAUDE.md. It did not refresh their delivery checksums. No statistical method, estimand, threshold or schema was changed by these additions.
- `cf6cef8c2b93e022ba037104fe6f327ac7bb0cea` changed the generic drug/protein binding-slot prose and its master-prompt pointer. Its change record reports a bounded Astra prose review, explicitly not a new G0 audit. It repinned the declaration to `21ebbd04688dfacb058af671d288a85d5910c657ff2e2437d2c6e99e7461aaaf`; the two entrypoint mismatches remained. The method remains external-evidence assessment, not a new binding measurement or D-T14 estimator.
- At accepted record `29ba7d75170a428860a36723847a085115ed2080`, exactly AGENTS.md and CLAUDE.md differ from both frozen content and delivery manifests. The actual92-file content hash is `461f3d14a32a3b84cc765f44d6ef76f8073aa64c147f34d75c7ff38c6158a4b8`.

| File | Declared SHA-256 | Actual SHA-256 |
| --- | --- | --- |
| AGENTS.md | ee206cd77888e430dc37ad6e7461bfd62210adffd2633562d5264cee1bc4d4a5 | e0de9739ef8e3062f977ed221e9cbd9a5fb5c2693df04dbdf0441889c4f71e1a |
| CLAUDE.md | d4b356fae3df49af3e0ff6b602b19c6fe1136c5d1e4d9de7ef50beb8c92e60ec | c7f5cf5bfc72084066d255c1e5d9034b677a1eadc65cdea5485028a00a098df8 |

## Verification and proposed disposition

The first validator attempt lacked Python dependencies; it is preserved without checksum-check credit. Sol installed jsonschema4.26.0 and PyYAML6.0.3 in an isolated verification environment, leaving package dependencies/lockfiles untouched. On the unchanged clean archive, `python tools/spec/validate_spec.py --verify-delivery` exits1: `Frozen content changed: AGENTS.md`. Independent comparison checks every manifest entry and identifies the exact two rows above.

A separately labelled diagnostic copy substitutes only the two original pinned entrypoint blobs from b56e942; its unchanged validator exits0 with all10 structural/delivery checks passing. This is not the current checkout, not the audit subject, and never counts as a current-source validator pass. Both outcomes and exact hashes are preserved in the local evidence receipt.

Retain `freeze.json`, FILE_MANIFEST.json and freeze-approval.json as historical delivery/audit records. Do not silently rewrite old hashes or change the validator to ignore unknown drift. The proposed current review uses RDY-001-review-manifest.json, which binds the actual92 content files, historical delivery records, all current change-record documents as context (not blanket acceptance), accepted milestone/ledger references, and this scope. Its digest is computed over sorted path/hash/role records using UTF-8 compact JSON with sorted object keys. The manifest does not hash itself. Any mismatch in that reviewed set must fail its read-back; no generic entrypoint exception is allowed.

Astra must review the current scientific/config/data/packet contracts plus operative accepted amendments, the binding-slot change and the exact entrypoint additions. The old validator's expected historical mismatch is not concealed. The desired decision is whether this exact current review subject can receive a fresh spec-readiness approval, recorded separately with its immutable commit, actual content/context digests, original failures and qualifications. This proposal does not treat all87 change records as accepted: actual accepted records remain governed by the ledger and milestone/packet receipts; blocked and superseded records are history only. No historical audit verdict is replaced.

## Boundaries and next transition

No R code, tests, NAMESPACE, configuration schema, packet identity, statistical method, B-T04 bound or seed changes in this cycle. G7/G8/G9 remain released. C-07 controls remain not_run; C-09 unqualified; D06/D07/D14 blocked without credit. No private study input goes to Astra. One fresh initial Astra-low audit and at most one focused documentation rereview; do not dispatch Luna implementation until this audit passes. Sol records findings and either the current approval or a precise blocker, then advances the next legal readiness work.
