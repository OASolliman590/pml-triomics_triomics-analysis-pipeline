# A-T01 initial integration review

- Candidate commit: `aa1595f94f0556fb3210702192307b5a4ab7bccb`
- Fixed point: `b56e942b6b2b223074d63f232d73737a82efc4bd`
- Allowlist: pass; all 19 changed paths match the frozen A-T01 write set.
- `git diff --check`: pass.
- Worker receipt: 37 passed, 0 failed, 0 warned, 0 skipped; standalone CLI and native wrapper evidence recorded locally.
- Sol rerun: 36 passed, 1 failed. The clean native package-check fixture returned status 1 after a successful build, so the receipt was not independently reproducible.

## Standards axis

No hard repository-standard violation, private input, credential, or out-of-scope path was found. Two judgement calls were recorded: duplicated atomic-write logic in `R/contracts.R`, and duplicated execution-availability decisions across `R/stage_registry.R` and `R/cli.R`.

## Specification axis

The fresh read-only review found four gaps:

1. P1: scalar YAML values were coerced to arrays before schema validation, violating A-T01-AC1/V01.
2. P1: `blocks[].file` was omitted from stage-A absent-path validation, violating A-T01-AC3/V01.
3. P1: artifact-envelope validation did not enforce status, producer/input structure, referenced hashes, or path-containment rules required by R28 and DATA_CONTRACTS section 1.
4. P2: CLI errors could produce only exit 1 or 2 rather than the section 7 taxonomy (0/2/3/4).

## Disposition

Not accepted. Bounded repair attempt 1 was dispatched to the same Luna max worker. No candidate commit was integrated.
