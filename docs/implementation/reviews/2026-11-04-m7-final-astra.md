# M7 initial final Astra audit — not accepted

Requested reviewer: fresh `/root/m7_final_astra`, gpt-6-astra / low. Host API
accepted those settings; effective runtime settings are not independently
exposed. Read-only: no edits, tests, fits, children or acceptance.

**audit_pass: no** for product `126a30fc68fd593826a3ca82cd399fff35ee90ef` (P),
with records `f33b1fd764589f4645691847b9ee610b42d718cf` (Q). Q is documentation-only,
not a separately tested product. The reviewer inspected relevant contracts,
changes, tests, DOC-M7 and receipts, and verified six log digests, the archive tar
digest and all 80 recorded native file hashes. The passed source/package/native
gates remain valid regression evidence for P; they do not establish acceptance.

- **M7-FINAL-ASTRA-01, P1:** `R/target_request.R:106` makes `assay` and
  `assay_limitations` optional. Numeric value plus identity matching can become
  measured direct binding through `R/target_assessment.R:157`, without identifying
  the experiment. Static reproduction: `t11_external_assay_profile()` at
  `tests/testthat/test-D-T11.R:598` already omits `assay`; also remove
  `assay_limitations`. The record remains measured/comparable, contrary to the
  exact assay and limitations requirements in D contract section 5.
- **M7-FINAL-ASTRA-02, P1:** `R/target_assessment.R:48` matches molecular features
  by namespace and ID but ignores taxon. Static reproduction: change
  `td_target_evidence()$feature_registry$feature_key_a$taxon_id` from 9606 to
  10090 while retaining the human request. The B effect still becomes support,
  without cross-species or unknown applicability qualification.

Sol independently confirmed both code paths and the existing scientific
contract. These need behavioral regressions and a bounded implementation repair;
no statistical or frozen-schema amendment is needed. The review also suggested
using “gates remain unreleased” instead of ambiguous “closed” wording.

The initial final milestone review is consumed. One focused final rereview
remains after the separately audited M7-029 repair and fresh gates. M7 and G7
remain unreleased; C-06 and all eight required current D registrations remain
unaccepted. Conditional D06/D07/D14 receive no credit; C07/C09 limitations remain.
