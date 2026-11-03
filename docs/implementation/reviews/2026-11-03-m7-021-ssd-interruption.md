# M7-021 environment interruption — not accepted

PortableSSD disappeared from /Volumes during Luna's native shared proof. Both
coordinator /tmp/triomics-m7-021 and worker /tmp/triomics-m7-021-luna retain their
files, but their .git pointers resolve into the unavailable external repository.
No Git metadata was recreated, no source-identity check bypassed, no push/private
data operation performed. Git clean status cannot currently be reconfirmed.

Last coordinator commit: 8b41879 (records only). Worker candidate commits:
- c82a7e8181b2f61f2b839e2c4909671e58ca21e0: pinned mapping fixtures/tests.
- 58e100922cf5da0d435edae9593d3b09da0c9e8e: resource-path resolution and harness fixes.
Neither worker commit is integrated. M7-021 initial attempt is paused by the
environment; no parent repair attempts have been used. Historical caps stay closed.

Earlier precursor proof at4e8c1c2 remains valid:1087pass/0fail/0error/1namedwarn/0skip,
native B/C/D exits0/0/0 and23assertions, null time and matched hashes. See the
precursor receipt. It is not a pass of the new mapping candidate.

Luna mapper unit checks:145pass/0fail/error/warn/skip, from the initial working
diff. Native RED at c82a7e8:17pass/2fail/1error/2warn/0skip; B/C succeeded and D
exited2 for both packet tickets; expect_gt(info=) harness error also recorded.
Native run at58e1009:32pass/2fail/0error/warn/skip. Both B commands failed
B_EXPORT_INCOMPLETE because executing source Git revision became unavailable.
D mapping was not reached. Do not call this native GREEN.

Sol found a pending path review issue: normalizePath(local_file) can resolve a
resource symlink before the existing service rejects it. Keep the joined declared
path and add a relative-symlink regression. Do not weaken existing guards. Luna
stopped without further edits; the same worker /root/m7_021_luna may resume this
initial cycle after the real Git metadata returns. No new dispatch/cap reset.

Recovery copies: /tmp/triomics-stage-d-evidence/m7-021/ssd-interruption contains
only allowlisted public source/fixture files and SHA256 manifest. External logs,
readback and scope drafts remain in /tmp/triomics-stage-d-evidence. Future drafts
M7-022 ORA, M7-023 status, M7-024 targets and M7-025 discovery are unaudited and
unimplemented. D04 must verify before D05; D11 before D12.

Resume literally: reconnect PortableSSD, verify coordinator8b41879 and worker58e1009
and actual dirty files; resume the SAME Luna initial attempt to preserve symlink
rejection, run final focused/native tests from clean code, inspect and independently
verify before integrating. Continue the authorized audited Stage D cycles. Do not
rerun completed predecessor proofs merely because the drive disconnected.

M7/G7 remain closed; current C06 registration false; C07 not_run; C09 unqualified.
Conditional D06/07/14 have no completion credit. No full source/archive gate or
final M7 Astra review has yet run in this continuation. This receipt is uncommitted
because the external Git metadata is unavailable.

## Recovery in the same run

PortableSSD remounted. Sol verified coordinator8b41879415f2ace796ba48013c139ac2ce50f3d0
and worker58e100922cf5da0d435edae9593d3b09da0c9e8e, with a clean worker checkout.
Main branch remains spec/harden-v2-20260920 at77fbc38. The SAME Luna initial
attempt resumed for the pending path correction and final native proof. No
Git metadata recreation, duplicate worker, source bypass or cap reset occurred.
The failed environment run remains failed evidence, not acceptance.
