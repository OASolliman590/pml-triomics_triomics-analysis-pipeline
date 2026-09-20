# Orchestration

Sol owns the spec kit and the close.
Astra audits twice: spec freeze, then risky diffs. Astra never implements, never marks done, never starts the next packet.
Luna executes frozen packets only.

## Models
- Parent thread: GPT-5.6 Sol high. No max/ultra/fast.
- Spec audit / risky diff audit: GPT-6 Astra low. One shot. Fresh context. Then kill.
- Packet work: GPT-5.6 Luna high in a NEW thread. No Astra inside Luna threads.

## Hard rules
1. Spec kit is source of truth after freeze. Do not add features.
2. One job per thread. Title threads `sol / kit` or `luna / <packet-id>`.
3. Do not fork. New slice = new thread.
4. Disjoint files + no deps required for parallel Luna threads.
5. Receipts are ≤10 lines. No log dumps into Sol.
6. Astra never sees test logs.
7. Max 1 Luna subagent inside the Sol thread. Prefer a new Luna thread.
8. If Sol and Astra disagree on a requirement, stop and ask the user.
9. Close the loop only in Sol: tick the kit or list leftovers. Astra does not close.
