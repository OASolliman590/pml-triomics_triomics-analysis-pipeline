M7-027 worker receipt — 2026-11-04
Base: 6e73239ebbca95bc0aab24bb7b7faf78d6a640cf; initial commit f9abc42713387c3b5e33ba87766eea5d150e7222; repair1 commit eed7cb251c229399de0b5c110f516c4013197314.
Attempts: initial implementation + repair1 = 2/3; RED logs: red.json (336 pass, 3 fail, 1 error), repair1-red.json (342 pass, 1 fail, 1 error); no repair2 used.
Committed-source focused run: D-T10|D-T11|target-shared; sol-focused.json; 359 pass, 0 fail/error/warn/skip, 54 blocks. R parse and git diff --check passed; changes stayed in the allowlist.
Native command: native-packet-proof.py /tmp/triomics-m7-027-luna /tmp/triomics-stage-d-evidence/m7-027/native-worker inst/examples/tickets/D-T10.yaml inst/examples/tickets/D-T11.yaml.
Native: six B/C/D commands exit 0; four B predecessor hashes/member digests verified; generations D-T10=347c84a1d32fb1e5d9869a5345a4ac40ac43d479ef97d51d0baaf406fb43f75c, D-T11=71109a0e8b6188388b31232e8c795f76cb5381244c15f5e99964c7038ead5316.
Readback: sol-target-readback.json (parent readback: 144 assertions); molecular result/effect/unit and C loading lineage matched, dependency groups retained, and local metabolomics SHA matched profile source SHA.
Local mechanism SHA 93434242b0f8ab69023cc66e27f98f56797c5840026b23040a9f27bb750b79fa and assay SHA a59bc918cdb17d96d1c7c4d958f6da51087da580e792c342e8c1b8d221f18804 matched consumed provenance.
Assay values remain null/unavailable; four axes are present; D-T11 is noncomparable with selected_target null; no affinity or winner was inferred. No worker process remains active.
