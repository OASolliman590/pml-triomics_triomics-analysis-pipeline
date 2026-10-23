# M7-002 initial Astra-low amendment audit

- Date: 2026-10-23
- Immutable candidate: `81fdf776f5bdfb9bccc0628c61b30fc7cb58643f`
- Reviewer: `/root/m7_002_astra_audit`
- Requested role/model: fresh read-only Astra low / `gpt-6-astra`
- Verdict: **changes_required**
- Findings: Critical 0, Important 3, Minor 0
- Frozen base manifest: recomputed content hash matched
  `f0d482db62f16c8e4c802c99c1fd88e501f29959bc475d67f934b40c6f6ae5d2`;
  no frozen specification file mismatch.

## Important findings

1. The closed contract omitted role/type enumerations, cardinalities, graph-link
   resolution, conditional descriptor fields, exact generation-manifest fields,
   and an explicit rule excluding self-referential manifest/provenance digests.
2. The pointer contract required old bytes to survive a failure after a
   successful atomic rename. Rename must be the commit point; ambiguous
   postcommit acknowledgement needs recovery of whichever complete generation
   is current, not a fictional rollback.
3. Phase 3 assigned shared CLI registration to Luna, conflicting with AGENTS
   hard rule 5 and the packet registration rules. Sol must retain shared
   registration ownership; substantive shared behavior needs a separate scope.

The reviewer found that the proposal conceptually closes all five M7-001 defects
and preserves scientific methods, privacy, packet identities, and exhausted
attempt history. Implementation may not proceed. The amendment permits one
documentation repair and one focused rereview.

No files were edited and no private study data were read by the reviewer.
