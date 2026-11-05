# M8-D15-S01 local review — 5e4b81a

Fresh read-only Sol 6.1 high Standards and Spec reviewers inspected immutable `5e4b81accdc3f63ad1d33b96176ee934c561f5db` against `7e7207faa121dd34686d413af98c6b7700b8e4e3`. Archive SHA-256 `570340583c084e8d11cea3ca2b5c2b8cb00ab24efc5d395263d01e5b57c8aa56`. Effective runtime settings are not exposed. Neither reviewer edited, fitted, integrated or accepted anything.

## Standards

One concrete P2: `R/clinical_native_handoff.R:570–576` filters missingness dispositions before counting unsupported categories, so the count is always zero. The scope requires preservation of null/unsupported dispositions. Sol independently reproduced a custom category being reported as zero.

One judgment heuristic, not a hard violation: route qualification is duplicated at `R/clinical_native_handoff.R:279–315` and `758–801`. The same between-person mode, one-analysis/one-integration and explicit-window conditions could diverge. Repository standards override this heuristic; no refactor is required merely for style.

Standards total: one concrete defect and one heuristic. Worst concrete issue: lost unsupported missingness information.

## Spec

P1: Metadata group assignments can change after C and still pass B preflight (`R/clinical_native_handoff.R:363–371`), because only measurement source manifests are compared. Scope requires rejection of changed inputs before any B fit. Sol reproduced accepted preflight after swapping one control/treatment pair, then restored exact source bytes.

P1: Rehashing the saved context envelope after changing decoded handoff evidence passes the saved-context reader (`R/clinical_native_handoff.R:1228–1245`). Scope requires the same completed C run and unchanged C09 restrictions. Sol reproduced a decoded C09 status of complete and benefit_guaranteed=true from the mutated copy. This is a defect demonstration, never an accepted C09 disposition. Final publication was not demonstrated; the independent B partial-state blocker intervenes.

P2: A registries directory symlink outside the output root is accepted by B preflight (`R/clinical_native_handoff.R:356–357`). Scope explicitly rejects symlinks before fitting. Sol reproduced acceptance with an unchanged completion registry through that external directory.

P2: The new failure test (`tests/testthat/test-clinical-native-handoff.R:230–324`) lacks injected publication failure and preservation of a previous D generation/full original C inventory. Scope expressly requires those proofs. No failure-injection or zero-refit success is credited.

Spec total: four findings; worst issues are changed-input authentication and substituted saved evidence. No scope expansion was identified. Verdict: changes required. No repair was dispatched after the independent native run revealed an owning B export file outside the allowlist.
