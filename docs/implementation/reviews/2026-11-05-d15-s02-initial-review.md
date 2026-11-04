# M8-D15-S02 initial candidate and bounded repair

Immutable candidate5519fc12f84daca952731bb5fbb1257189804de6 changes only R/export_b.R and tests/testthat/test-B-T07.R, from5e4b81a. Archive SHA-256 b8e09e00f38c639c593699c0b16f29640ebcfd72603723a0d696713b48e8b460. Worker source gate:409pass,0fail/error/warn/skip,27blocks. Sol read the actual source receipt. Native A/C/B exited0/0/0; worker readback shows complete B, actual B08 result, absent/not_requested B09 and retained objective/family. Hashes matched disk. The worker's final profile-validation JSON is absent and unverified, so no profile gate is credited. Parent process inspection confirmed no owned verification process remains.

## Standards review

Fresh /root/s02_standards_review (requested Sol6.1/high; effective settings unexposed) found a concrete P2: as.integer truncates fractional family counts before matching. Its separate heuristic was repeated same_string helper definitions; no style-only repair is required. No documented AGENTS/GLOSSARY violation was identified.

## Spec review

Fresh /root/s02_spec_review (same requested settings) found three blockers: B09 predictor_visit contradictions are unchecked (P1); internal_p/internal_q can omit a declared unavailable family member (P1); fractional counts are truncated (P2, same defect as Standards). Both reviewers remained read-only, with no fits or acceptance. Source anchors are R/export_b.R:568,608–610,655–662 on5519fc1.

Sol independently reproduced all three with actual service-shaped synthetic data at the accounting boundary, without fits or publication. A one-member family declaring1.9/1.9/0.9 passes; a two-member family with one-entry correction vectors passes; changing both B09 audit copies' predictor_visit toV0 for a between-person objective passes. These are isolated negative probes, not native producer proof.

Sol assigns repair1of2 to the same Luna, same two-file allowlist. Add artifact-level RED cases, validate whole counts before conversion, match applicable fixed time-policy fields, and require full consistent correction accounting without modifying numeric results or denominators. Preserve legitimate unavailable results and observed failure evidence. Commit before final gates, inject pinned runtime before R starts, use a fresh output root and repo-relative CLI. No schema, statistical method, shared CLI or S01 helper edits.

Earlier default-temp RED, absolute-path launch failure and dirty-source B refusal remain diagnostic-only. All later acceptance needs clean-source proof. No integration yet. S01repairs0of2 and its four defects remain separate; D13cap preserved. M7/G7accepted, G8/G9closed. No private inputs, push or AIU execution.
