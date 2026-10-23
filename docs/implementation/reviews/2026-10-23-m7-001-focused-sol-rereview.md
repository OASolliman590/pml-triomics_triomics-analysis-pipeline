# M7-001 focused Sol rereview

- Date: 2026-10-23
- Base: `71c0a1b628c314c03112a4c2a27a760e9ab0d0a1`
- Candidate: `1b44a84e71139ca52b496bbe562e2e2a70142b2f`
- Review package SHA256: `6909a7e1e514e8bf070845a84510d6390dd4b03630a05d54c2b7d5e336403c2b`
- Reviewer: `/root/m7_001_sol_review`, fresh read-only Sol-high
- Verdict: **Critical 0, Important 5, Minor 0; `ready_to_integrate: no`**

The candidate is clean and changes exactly the four M7-001-authorized files.
The supplied exact D-T01 filter passed 268 expectations across 27 test blocks
with FAIL 0, WARN 0 and SKIP 0. The reviewer independently reran the packet
tests and diff check. Those passing tests do not close five reproduced gaps:

1. `R/cli.R:1905`, `R/cli.R:1942` and `R/cli.R:2067` do not bind current
   FeatureRegistry semantics to accepted B/C lineage. Changing only the public
   fixture's `id_namespace` let D exit 0 and publish the changed namespace next
   to unchanged predecessor evidence.
2. `R/cli.R:1852` and `R/cli.R:1860` reproduce B-T03/B-T04 producer hashes
   that omit consumed visit/time design values. Public resolver probes changed
   the paired delta assay or repeated design while the guard hash stayed equal.
3. `R/cli.R:1828` hashes `block$design` for DEqMS, but the two-group producer
   synthesizes its design separately. An unchanged supported two-group input
   therefore yields a different consumer and producer identity.
4. `R/cli.R:1818` treats any list member named `model_hash` as a scalar hash.
   A valid contrast whose ID is literally `model_hash` therefore hides its row
   identities and is rejected.
5. `R/cli.R:2076` rolls over `registries/run.json` before the D writer checks
   destination leaves. A dangling `D/evidence-routing.envelope.json` symlink
   caused D to fail only after the current registry had already moved.

The original B-envelope identity and structured-key findings are closed. The
original registry-directory symlink escape and invalid-config mutation examples
are also closed. Measurement-byte and continuous-score mutations are covered,
but the authorized full provenance and preflight guarantees are not.

Independent probe evidence is in
`/private/tmp/triomics-m7-001-sol-review-probes.R` and
`/private/tmp/triomics-m7-001-model-probes.R`; the full transient review is
`/private/tmp/triomics-m7-001-sol-rereview.md`. No private study data were read.
No candidate commit was integrated and D-T01 was not accepted.

M7-001 authorized one exceptional repair and one focused rereview. That repair
is now consumed. Any further product edit requires a new explicit change record
and user authorization; the existing exception cannot be silently extended.
