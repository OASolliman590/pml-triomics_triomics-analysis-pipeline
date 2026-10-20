# C-09 repair 1 focused rereview — 2026-10-21

The sole focused rereview compared repair `253c6f0da495f0ec61c0c7ec943dca5b2bb2a7c9`
with initial candidate `45a0f9900306f60a6a87a2fbfffbf7dd57adca2e`.

The two original Important findings are addressed: candidate-specific failures
are retained with explicit matched-success coverage/unavailable estimates, and
named failure reasons plus outer-fold dispositions are preserved. The public
fixture now has a packet-owned runtime generator rather than a test-helper
reference. The real endpoint/deployment gate remains open.

One new Important regression prevents integration. The runtime generator calls
`triomics_reset_profiles()`, deleting caller registrations. The reviewer
independently registered a sentinel profile, invoked the generator, and then
observed `A_CONFIG_PROFILE` when looking the sentinel up. Repair 2 must remove
the reset and use a dedicated fixture profile without clearing caller state.

Independent exact suite: 47 passed, 0 failed, 0 warnings, 0 skips. The exact CLI
still exits 2 with `A_CONFIG_UNKNOWN_KEY`, as disclosed. The four-file allowlist
is preserved and the worktree is clean. AC1 remains conditional/open; AC2 and
AC3 pass in synthetic behavior; AC4's original reporting defects are addressed.
The candidate is not ready to integrate. This was the one permitted focused
rereview; repair 2 receives direct Sol inspection and verification only.

