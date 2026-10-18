# C-01 independent Sol review

Date: 2026-10-18  
Packet: C-01 — common integration contract and concatenation PCA  
Initial candidate: `5e43391155643732289a2cfc3d2a27fdbcd3b416`  
Repair 1: `e47715d6f3df20fd4fb2063d3d95e4139b3dce9d`

## Initial verdict

Fresh Sol-6.1-high review returned **changes required** with P0 0, P1 2 and
P2 2. The PCA arithmetic, training-only preprocessing, projection/SSE metric,
scaling, selection, rank lineage and consensus convention passed independent
numeric probes. Integration was stopped for four contract defects:

1. C-01 required identity fields duplicated on each block although the accepted
   B resolver keeps them on the enclosing result.
2. A one-block request incorrectly succeeded as multiblock integration.
3. A strict required-view subset could reach PCA with missing training coverage
   and raise an untyped indexing error.
4. Malformed public request containers could be dereferenced before typed
   validation.

The complete initial review is retained locally at
`/private/tmp/triomics-c-01-sol-review-2026-10-18.md`.

## Focused rereview

Luna repair 1 addressed all four findings without changing the verified PCA or
consensus mathematics. The single allowed focused Sol-6.1-high rereview passed:
P0 0, P1 0, P2 0. Fresh evidence included:

- actual synthetic YAML ingestion -> accepted B resolver -> public C seam;
- a 12-unit by 2-component integration result;
- typed one-block, incomplete-training-view and malformed-container refusals;
- optional duplicate identity agreement/conflict/malformed probes;
- 109 focused expectations with no failure, warning or skip;
- independently assembled scaled/weighted `stats::prcomp` agreement with maximum
  aligned score error `5.55111512313e-16`;
- independent nested block SSE agreement with maximum error
  `7.77156117238e-16`;
- allowlist and whitespace checks passed.

The exact CLI remained unavailable at review time because shared stage-C
registration is Sol-owned. The rereview therefore authorized serial integration
and shared registration, not packet acceptance by itself. Detailed evidence is
retained locally at
`/private/tmp/triomics-c-01-repair1-rereview-2026-10-18.md`.

## Scientific boundary

This review verifies software contracts and numerical behavior on public
synthetic fixtures. It does not establish biological validity, real-study
performance, clinical utility, or qualification of deferred integration
backends.
