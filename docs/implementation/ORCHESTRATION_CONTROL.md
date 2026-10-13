# Coordinator handoff control

This run exposed a coordinator failure: completed worker and reviewer chats were
not reconciled until Omar asked for status. The code results were preserved, but
the completion-to-next-action handoff was late.

For every run-owned chat, Sol must record the chat ID, role, packet or gate,
candidate/base commit, allowed next transition, and current wait cursor. After a
dispatch, Sol must use `wait_threads` while no independent local work remains.
Commentary messages are not completion. A completed turn must immediately cause
one of these transitions:

1. worker result -> inspect diff and dispatch the authorized reviewer;
2. changes required -> send the bounded repair to the same implementer;
3. reviewer acceptance -> integrate serially and run fresh Sol verification;
4. milestone evidence complete -> dispatch the authorized Astra review;
5. Astra result -> record Sol's gate disposition and update the ledger;
6. explicit stop -> record a paused handover, archive run-owned idle chats, and
   leave no polling or new packet active.

Sol must not send a final completion/status answer while a run-owned chat is
active unless Omar explicitly requests a pause. Before every final answer, Sol
must reconcile `list_threads` against the ledger's active handoffs. An idle chat
is not assumed accepted; its latest completed turn must be read. A completed chat
is not left idle with an authorized next transition undispatched.

This protocol changes orchestration only. It does not modify packet attempts,
scientific acceptance, model roles, concurrency limits, or frozen thresholds.
