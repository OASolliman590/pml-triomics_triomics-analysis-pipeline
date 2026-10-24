# triomics: orchestration and scientific authority

Read `discovery/spec-v2/README.md` first. Version 2.0.0-rc1 supersedes conflicting
instructions in old fleet records, uploaded prompts, `discovery/spec-drafts/`,
old ticket cards and previous AGENTS text. Preserve those records as history.
User instruction dated 2026-09-20: spec hardening now; implementation only when
the execution prompt is activated and the independent spec audit passes.

## Roles
- Sol **high**: one accountable parent; owns freeze, integration, acceptance,
  ledger, publication and close. Do not delegate orchestration recursively.
- Luna **max**: one bounded coding packet per fresh isolated worker/worktree.
- Terra **max**: bounded documentation, traceability and evidence-summary tasks.
- Astra **low**: read-only, fresh-context audit at spec freeze and **each of nine
  milestones**. Review risky cross-cutting diffs before consumers are released.
  Never implement, mark tickets done, merge, dispatch or close the project.
These are requested role/model labels, not proof of runtime availability.
Resolve model IDs and supported effort through the actual host. Verify effective
settings after spawn. Never silently inherit Sol for every worker or substitute
an unavailable model. No premium/fast tier, subscription changes or new credits.

## Hard rules
1. One authority: spec-v2 contracts + packets.json + accepted change records.
   Freeze by checksums and audit receipt; an old review is not a review of v2.
2. No product scope expansion. R core + isolated Python MOFA backend is already
   approved. Do not reopen the language debate or repeat completed discovery.
3. Parallel workers require satisfied dependency AND review gates, disjoint
   allowlisted files, isolated worktrees, and available host/budget capacity.
4. Defaults: up to 3 Luna and 2 Terra, but at most 4 simultaneous workers total.
   Astra runs on an immutable milestone snapshot, not a moving branch. Scale
   down to verified host capacity; never scale up without explicit permission.
5. Only Sol writes shared package/CLI registration, lock files, canonical spec,
   task states and integration branch. Workers return local commits; Sol inspects
   and cherry-picks serially. A-T01's initial shared files are an explicit serial
   exception. Other workers request registration via a packet-owned handoff.
6. No child agents inside workers. No full-repo re-reading for every packet.
   Default brief <= 2,000 words; receipt <= 10 lines, with detailed artifacts on
   disk. Astra sees acceptance, relevant diff and test summary; targeted failure
   excerpts are allowed when necessary, not unbounded log dumps.
7. Write failing behavioral tests, implement, run named tests, record evidence.
   Agent exit, scaffolding, synthetic recovery and review approval are not proof
   of scientific validity. No skipped required test may count as passed.
8. One initial attempt + at most two targeted repair attempts per packet. One
   initial milestone review + at most one focused re-review. Further unresolved
   work becomes blocked; continue unrelated legal work, then close honestly.
9. All pipeline data, clinical identifiers, private paths/credentials and large
   assay outputs remain local. This repository is PUBLIC. No external uploads
   of private inputs through agents, reports, Notion, or annotation services.
10. Sol resolves factual inconsistencies against sources and the frozen design.
    A material scientific-contract change requires an explicit change record and
    affected acceptance review, not a worker invention. Ask the user only for
    unresolvable scientific preferences, entitlements or disclosure decisions.
11. Do not confuse an optional disabled route with an implemented route. Preserve
    all 41 ticket identities and conditional scope. Unavailable methods do not
    make a complete project merely because a report rendered.
12. Stop after the close report. Close/cancel only workers created for this run;
    leave no polling, auto-relaunch or persistent task. Never terminate the
    user's host, unrelated processes or sessions. Save a resumable handover.

## Start / resume
Use `discovery/spec-v2/prompts/MASTER_SOL_PROMPT.md`. Restore the execution ledger
and actual Git state; never recreate completed packets from memory. `CLAUDE.md`
and `discovery/SPEC-KIT.md` point here and do not define competing instructions.

## Omar-facing progress reporting

For Omar-facing reports and Notion updates, read `docs/OMAR_PROGRESS_CONTRACT.md`. Keep a plain-language briefing and separate software evidence from scientific evidence. First adoption is orientation only. Preserve the existing scientific spec, task IDs, approval gates and orchestration limits. Only the coordinator publishes shared Notion progress. Never claim a sync without read-back verification.

## Completion handoff control

Read `docs/implementation/ORCHESTRATION_CONTROL.md` before dispatching or resuming
run-owned chats. The coordinator must reconcile every completed turn into the
next authorized transition, use `wait_threads` when otherwise idle, and must not
finish a response while a run-owned chat is active unless Omar explicitly pauses
the run. Before stopping, reconcile thread state with the ledger, archive this
run's idle chats, and leave a literal resume instruction.
