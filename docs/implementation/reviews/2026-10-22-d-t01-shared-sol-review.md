# D-T01 shared registration review

Date: 2026-10-22  
Base: `ae6762b`  
Candidate: `71c0a1b628c314c03112a4c2a27a760e9ab0d0a1`  
Verdict: `ready_to_integrate: no`

The fresh read-only Sol-high review found zero Critical, three Important, and
two Minor issues. The exact D-T01 suite still passed 204 expectations across 22
test blocks with no failure, warning, or skip. The ordinary fresh B-to-C-to-D
sequence, disabled-route accounting, no-refit behavior, hash read-back, and
unchanged-file rerun refusal also passed. Those positive results do not waive
the reproduced fail-closed boundary defects below.

## Important blockers

1. Stage D re-ingests mutable source data without reconciling the new source
   hashes with predecessor B/C lineage. A public synthetic source label and
   value were changed after B/C; D still exited zero and published the changed
   label beside stale B/C results.
2. Registry rollover validates leaf paths but not every ancestor. Replacing the
   output root's `registries` directory with a symlink let D exit zero while
   renaming and writing run registries outside the output root.
3. Rehashed B envelopes with conflicting nonempty `artifact_id`,
   `preparation_context_id`, or `representation_id` were accepted by both the
   D reader and public CLI. Exact analysis and status conflicts did reject.

## Minor findings

- Structured C selection-rule comparison is key-order-sensitive even when the
  object has identical keys and values.
- Registry rollover occurs before config validation, so a missing config can
  remove the current `run.json` before the command fails.

The eight-file D-only change was otherwise scoped correctly: API, NAMESPACE,
manual, stage registry, `per_unit_beta` fixture coefficient vector, ordinary
artifact identities, disabled-route interpretation, and the reviewed Stage-B
compatibility seam were correct. The worktree remained clean and no frozen
specification or hash changed.

D-T01 is not accepted. Its initial implementation and two bounded repairs are
already consumed, so a further product repair requires an explicit scoped
exception. A fresh Astra-low read-only blocker audit was dispatched before any
such proposal or acceptance decision. No private study data were read.
