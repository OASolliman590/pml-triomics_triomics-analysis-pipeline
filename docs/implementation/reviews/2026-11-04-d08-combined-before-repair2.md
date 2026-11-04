# D-T08/S01 combined verification before final report repair

Immutable isolated candidate `8a5c7621372d8c3e03b0d64587929912407f653a` combines report7c551b0, Sol helper0e30743, mechanical registrationda79e86 and inspected S01 call-site/test commit7d78050. No canonical integration or packet acceptance.

Uncapped focused source: 1376 pass,0 fail/error/skip,1 warning,90 blocks. By file: A-T09=160, C-06-shared=355, D-T01=406, D-T08=124, M4-005-B-T05-integration=191, stage-run-registry=140. The sole warning is named `D-T01 refuses stale paired signs and preserves the old pointer`: its intentionally rejected stale-sign D subprocess has status2. Warning was not suppressed.

The warning exposed hard-coded temporary locations in legacy tests. That run is preserved behavioral evidence, but not the requested no-tmp runtime qualification. Separate scope M8-D08-S02 is audited and must relocate only test temporary paths before qualifying the final combined run. Do not relabel this receipt GREEN for the location requirement.

The initial native driver stopped at the missing-producer negative probe: report exited2 with D_REPORT_PATH_UNSAFE instead of the required missing-dependency3. A separate explicitly diagnostic positive chain then ran in a fresh untouched internal APFS root: A0, B0, C0, D0, report2. A registry/payload/envelope hashes were unchanged across B. Sol independently matched the four B predecessor hashes in C and the B/C profile, envelope and bundle hashes in D; D current-pointer, generation manifest and member hashes/sizes matched. No registry was deleted or relocated.

Positive diagnostic provenance hashes:

- B provenance payload: fc4619593e54c4c59dc60533ecf3cb2b456e74f33df33176b6e450578a5c223d
- B provenance envelope: 1fee4536a7da93311b84dc4f369a3c19fb887b23f54d27ed274cafc37ff895f3
- C provenance payload: e06ff5fe390a23e27fa74f1cb02056aaf6ec33055a2ab403825ff4b1f0d4c4d2
- C provenance envelope: 7a5f50740862765e1f3d4d7390997e88abe788570100ee737485c093f741b898
- D generation manifest: aaaf0b601faa659cea5eb632909009878fef76e9cb3ad50630a9e074db797ad5

The report fails before rendering because the existing secure path utility returns a double-leading-slash spelling for a nonexistent child, and the report's prefix comparison rejects it. A narrow no-fit reproduction confirms this at the legitimate new report destination. Repair only the report-owned wrapper; preserve traversal/symlink rejection. Consolidate it with the prior quoted-credential/single-component-path and nested-CV status findings for report repair2, which remains undispatched.

Local receipts: sol-combined-before-repair2/focused/source-receipt.json; native/native-receipt.json; sol-native-positive-before-repair2/{receipt,lineage-readback}.json; report-missing-child-probe/{probe.R,probe.log,receipt.json}. All original failures remain. No HTML acceptance or package check has occurred. M7/G7 accepted baseline unchanged; G8/G9 unreleased; conditional restrictions unchanged. No private data or push.
