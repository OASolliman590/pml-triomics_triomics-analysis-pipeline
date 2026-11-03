# M7-023 initial implementation audit — changes required

Fresh read-only /root/m7_023_implementation_astra reviewed immutable
2715d7d0c474f4366cfa69a66ce4338ade72aee4 against749cf67, including Sol CLI2a022cb.
Requested gpt-6-astra/low; effective settings unexposed. audit_pass: no.
No tests, edits, fits or children; dirty fixture repairs excluded.

P1: ORA protocol access at annotation_apply.R725–728 uses the reference-availability
wrapper. A scientific protocol content mismatch must remain a hard rejection,
with existing D-ora-shared old-pointer/payload assertions retained.
P1: annotation_status.R3–8 translates every D_RESOURCE_HASH_MISMATCH, but that
code also labels a source symlink and cache directory in resource_manifest.R.
Unsafe path/structure must remain hard rejection, separate from wrong regular
reference bytes. Sol reproduced the symlink case at2715d7d: status unavailable,
reason D_RESOURCE_HASH_MISMATCH. Evidence stored locally and copied below.
P2: Add changed-rights-after-success coverage, full route accounting, successful
mapping preservation through partial failure, actual predecessor byte/hash
preservation, and previous-pointer preservation on true rejection.
P2: Replace hardcoded /private/tmp in new tests with canonical platform tempdir.

Native initial tests had two prerequisite fixture failures before D: yaml rewrite
scalarized source_refs, and D05 annotation requests omitted request_id. Those
fixture fixes were already dirty but untested when Sol held repair1 for combined
review. Unit56pass and D02/D03/D04unit187pass are not native GREEN. Repair1 now
combines all findings; one further targeted implementation repair and one focused
Astra rereview remain. M7/G7 and dependent target work stay closed.

R/resource_manifest.R is within the frozen conditional allowlist and is now
explicitly activated for the reproduced unsafe-resource/cache classification
failure. Record before edit: split unsafe source/cache structure from regular
content mismatch at the existing access guard, preserving hard rejection. No
publisher, provenance, schema, statistical or scientific-method change.
