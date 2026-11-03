# M7-024 initial behavioral RED

- Base: `8246369b159691dcdd4aca547289b17a5a9ab270` (`codex/m7-024-luna`).
- Command: `testthat::test_local(filter='D-T10', stop_on_failure=TRUE)` with the packet runtime paths.
- Result: FAIL 4, WARN 0, SKIP 0, PASS 60 (64 assertions).
- Wrong target ID, namespace, and entity type each attached as `request` instead of `target_mismatch`.
- Missing target ID attached as `request` instead of `unresolved`.
- This is expected RED evidence from `test-D-T10.R`; no implementation edits had been made.
