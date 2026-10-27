# M7-017 source CLI probe correction

The historical one-off trajectory-probe.R uses <repo>/exec/triomics. That script
sets source_root to dirname(script)/../.., which is the parent of the repository
for that layout; absent an R directory there, it loads installed triomics. It
therefore does not establish current source-candidate behavior. Its B0/C4
A_ARTIFACT_FINALIZE result is retained but not counted as a candidate RED.
Use <repo>/inst/exec/triomics: the same two-level source-root logic resolves the
actual repository. This is a test command correction, not an entrypoint/product
change. Parent pkgload diagnostics and structured test helper source selection
must be identified separately; final required native proofs use explicit
inst/exec/triomics with recorded candidate commit and exact config/output roots.
