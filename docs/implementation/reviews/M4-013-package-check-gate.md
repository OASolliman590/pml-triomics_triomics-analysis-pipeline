# M4-013 integrated package-check gate

- Date: 2026-10-16
- Integrated snapshot: `bd7da66ba58bbce571e90ab1ca76017c8e269d78`
- Complete local suite: **PASS**, zero failures, warnings or skips
- Clean-archive package check: **FAIL**
- M4/B-T07 disposition: unaccepted; package and Astra gates not satisfied

The committed snapshot was expanded under
`/private/tmp/triomics-m4-013-archive-check.jhhGSR/src` and checked with
`R_PROFILE_USER=/dev/null Rscript --vanilla tools/check-package.R`. Build
completed, but `R CMD check` reported one ERROR, two WARNINGs and one NOTE:

1. installed tests reported 15 failures in B-T07 because ticket paths that work
   in a checkout still point into excluded `discovery/` content after package
   installation;
2. `R/export_b.R` contains one em dash in generated Markdown;
3. `triomics_b07_export_handoff()` uses unqualified `setNames()`;
4. `man/triomics-api.Rd` documents `resume` twice after the B-T07 API addition.

The installed tests reached 3,403 passing expectations, eight warnings and zero
skips before failing. The warnings include the known absent-Git-metadata messages
from the clean archive. These failures are packaging, namespace, documentation
and installed-test-path defects; they do not negate the green local numerical
or scientific suite, but no required installed test, WARNING or NOTE may be
waived for M4 acceptance.

Evidence hashes:

- `build.log`: `8b1284943bd763e8dbc14515c987d83508f4ed61102e1fa51f3e1790ae77ba06`
- `check.log`: `a8ea4b4324047ee37f051858e72e6fd3f58b652e8a27dc07dcd8134c7518e51e`

B-T07 has consumed its two targeted repairs. No correction is authorized by
this receipt. A read-only Astra adjudication must classify whether a narrow
package-gate exception requires explicit user authorization, as the analogous
B-T08 exception did, before any B-T07 source/test change is made.
