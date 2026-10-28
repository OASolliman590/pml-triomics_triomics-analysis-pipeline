# M7-018 full-source warning inventory

Snapshot d660879. No warning suppressed.

1. **C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes** (test-C-05.R): Some view(s) have less than 15 features, MOFA will have little power to to learn meaningful factors for these view(s)....

2. **C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes** (test-C-05.R): Factor(s) 1 are strongly correlated with the average expression of features for at least one of your omics. Such factors appear when there are differences in the total 'levels' between your samples, *sometimes* because of poor normalisation in the preprocessing steps.


3. **C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes** (test-C-05.R): no non-missing arguments to max; returning -Inf

4. **C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes** (test-C-05.R): Factor(s) 1 are strongly correlated with the average expression of features for at least one of your omics. Such factors appear when there are differences in the total 'levels' between your samples, *sometimes* because of poor normalisation in the preprocessing steps.


5. **C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes** (test-C-05.R): no non-missing arguments to max; returning -Inf

6. **C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes** (test-C-05.R): Some view(s) have less than 15 features, MOFA will have little power to to learn meaningful factors for these view(s)....

7. **C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes** (test-C-05.R): Factor(s) 1 are strongly correlated with the average expression of features for at least one of your omics. Such factors appear when there are differences in the total 'levels' between your samples, *sometimes* because of poor normalisation in the preprocessing steps.


8. **C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes** (test-C-05.R): no non-missing arguments to max; returning -Inf

9. **C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes** (test-C-05.R): Some view(s) have less than 15 features, MOFA will have little power to to learn meaningful factors for these view(s)....

10. **C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes** (test-C-05.R): Factor(s) 1 are strongly correlated with the average expression of features for at least one of your omics. Such factors appear when there are differences in the total 'levels' between your samples, *sometimes* because of poor normalisation in the preprocessing steps.


11. **C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes** (test-C-05.R): no non-missing arguments to max; returning -Inf

12. **C-05 real MOFA2 fits and extracts a missing sample-view capability cell** (test-C-05.R): Some view(s) have less than 15 features, MOFA will have little power to to learn meaningful factors for these view(s)....

13. **C-05 real MOFA2 fits and extracts a missing sample-view capability cell** (test-C-05.R): All 2 factors were found to explain little or no variance so remove_inactive_factors option has been disabled.

14. **C-05 real MOFA2 fits and extracts a missing sample-view capability cell** (test-C-05.R): All 2 factors were found to explain little or no variance so remove_inactive_factors option has been disabled.

15. **D-T01 refuses stale paired signs and preserves the old pointer** (test-D-T01.R): running command ''/Library/Frameworks/R.framework/Resources/bin/Rscript' --vanilla 'triomics' --config '/private/tmp/d-t01-paired-native-fixture-1314071540f9d/tickets/d-t01-paired-sign-stale-131404c088440.yaml' --stage 'D' --output '/private/tmp/d-t01-paired-native-output-1314045cd468b' 2>&1' had status 2


Disposition: fourteen C05 native small-fixture diagnostics match historical named categories; one D-T01 warning is system2 reporting intentional status2 from the stale-sign rejection. That test asserts mismatch and unchanged pointer. These15 warnings do not explain or waive the separate C08 signed-weight type failure.
