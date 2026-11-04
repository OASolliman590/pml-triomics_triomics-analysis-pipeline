# D-T08 repair 1 checkpoint

Immutable candidate `7c551b05ac13903231cafed03ae398a9fff3f574` preserves initial `23b5689`. Only report.R, report_security.R and test-D-T08.R changed in this repair. Clean-commit source capture: 124 pass, 0 fail/error/warning/skip, 21 blocks. The earlier capped RED and intermediate failures are retained locally; they are not this GREEN result. No native/report render was repeated.

Fresh read-only local Sol-high review `/root/d08_repair1_sol_review`: changes_required. Requested model/effort accepted by host; effective settings not independently exposed. Remaining findings:

- P1 privacy: single-component absolute paths and quoted credential values survive sanitation and detection, including producer-manifest/JSON/HTML projection. Synthetic examples are `/patient.tsv`, `password="SYNTH_SECRET_42"`, and `api_key: "SYNTH_JSON_KEY_42"`.
- P1 compatibility: a native nested-CV result may truthfully contain completed evaluation while C-06 exports unavailable single-fit evidence with no native reference. The report currently rejects that distinction. It must show unavailable evidence without fabricating lineage or declaring C-07 scientific controls run.

Native clinical adjustment/interval projection, B byte binding, C value/context comparison, ORA claim context and provider-driven statuses otherwise address the focused findings. Final repair2 remains unstarted; consolidate actual combined native/report findings first. No packet/milestone acceptance.

The separate audited S01 predecessor transition is now active. Sol helper is frozen at `0e307435c9b3705d3ac2ec2cb00141344b1c04bd`; fresh Luna `/root/d08_s01_luna` owns only the two B registry call sites and test-stage-run-registry.R. Sol mechanical report CLI/stage/export/manual registration is isolated at `da79e86`. Neither candidate is integrated into the canonical checkout. The helper records producer artifacts, not successful later calibration/handoff or stage acceptance.

A transient PortableSSD mount loss interrupted Git availability. The mounted volume was recovered without filesystem repair, source recreation or data reading. Original worker HEAD and canonical HEAD/protected dirty hashes were verified unchanged before work resumed. Internal APFS working files and evidence remained available. The original native A0/B4 collision and rejected manual-deletion workaround are preserved. M7/G7 remain accepted; G8/G9 unreleased. No private data or push.
