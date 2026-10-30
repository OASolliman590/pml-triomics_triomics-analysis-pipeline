# Binding slot: the drug under investigation and a declared protein

- Date: 2026-10-30
- Status: applied on `spec/harden-v2-20260920`. This is not the G0 freeze audit.
- User authority: FCN3 is an example. The spec must say where the operator tests direct binding of the drug under investigation to a chosen protein.
- Review: Astra high, Codex thread `01a18e73-2ea0-73e2-b4d3-0471e9dd5b1c`, `audit_pass: yes` on the replacement prose. Astra did not edit the tree. `independent_audit_status` stays `pending`.
- Old content hash: `f0d482db62f16c8e4c802c99c1fd88e501f29959bc475d67f934b40c6f6ae5d2`
- New content hash: `21ebbd04688dfacb058af671d288a85d5910c657ff2e2437d2c6e99e7461aaaf`

## What changed

`discovery/spec-v2/D_ANNOTATION_TARGET_REPORT.md` section 5 now names the existing `target_investigation` fields as the binding slot: `drug_ref`, a protein in `targets[]`, and `external_evidence_file`. FCN3 and dapagliflozin, with PMID 41587770, remain one filled example.

`discovery/spec-v2/prompts/MASTER_SOL_PROMPT.md` replaces the single FCN3 use-case sentence with the same rule.

`discovery/spec-v2/freeze.json` and `FILE_MANIFEST.json` record the new checksums. No schema field was added.

## What stayed

Requirements R23 and R24, packets D-T10 and D-T13, and D-T13-AC3 are unchanged. D-T14 stays conditional-unapproved. The pipeline still does not measure binding, convert abundance or docking energy into Kd, or invent an affinity from the example abstract.

## Test impact

No new executable test. The slot is the existing configuration object. A missing local assay record stays unavailable, and unavailable is not evidence of no binding. The pre-existing dirty `AGENTS.md` and `CLAUDE.md` were not part of this amendment.
