# G0-001 — resource-service ownership

- Date: 2026-09-20
- Trigger: fresh Astra G0 P1 finding against content hash `cf02a3affbc7eceef5fcbd719b09780704854f9f0aa9397fdbecf073382852c4`
- Finding: D-T02 required a version/hash/rights gate implemented in `R/resource_manifest.R`, but that shared file belonged to parallel sibling D-T03.
- Decision: move `R/resource_manifest.R` and the shared operational acceptance to D-T01, the common predecessor of D-T02 and D-T03.
- Contract effect: no scientific or product scope change; D-T02 and D-T03 now consume the reviewed D-T01 resource service and remain parallel.
- Files changed: `packets/D-T01.md`, `packets/D-T02.md`, `packets/D-T03.md`, `packets.json`, plus candidate/manifest hashes.
- Corrected content hash: `f0d482db62f16c8e4c802c99c1fd88e501f29959bc475d67f934b40c6f6ae5d2`
- Required follow-up: rerun specification validation and obtain one focused Astra re-review before G0 approval.
