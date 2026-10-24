# Runtime, resources and operational boundary

## Environment

R core plus isolated Python for MOFA is already approved; do not reopen the
language choice. Current official release pages inspected on 2026-09-20 describe
Bioconductor 3.23 with R 4.6. Observed package metadata appear in
`evidence/dependency-manifest.json`; they are not claims of an installed lock or
guarantees of the newest patch. Older repository notes have different patch
observations; the actual resolved installation is the source of truth for a run.

A-T01 captures a genuine coherent minimal environment/renv lock on the authorized
host. Later adapters extend the reviewed dependency manifest in a serial,
project-local provisioning operation, never through competing workers. A chosen
older coherent approved environment is allowed only after explicit compatibility
record and actual capability requalification. Preserve working host environments,
global model settings and unrelated projects. No root/admin commands, paid compute,
provider-credit purchase or broad dependency installation is implied by this kit.

The dependency allowlist names intended dependencies, not a blanket new permission
to install every optional/transitive package. Resolve a concrete install plan,
actual versions/size/runtime requirements and existing authorization. A required
unapproved operation blocks that operation, not unrelated specifications or code
that can be developed/tested safely. Do not fake scientific acceptance with mocks.

CI uses synthetic/local reference fixtures and separates fast invariant tests from
native adapter smoke tests and slower calibration. Optional dependency absence
must not make required CI silently green through skip_if_not_installed. An actual
R CMD build/check wrapper is required; no rcmdcheck dependency. Do not require
Quarto for early A/B milestones. The final report/vignette stage must render.

## Source/resource rights

ResourceManifest records exact release/content hash, source/terms URL and retrieval
date, intended use, permitted operations and redistribution/caching status. These
must match the actual retrieved artifact, not a general website logo. Default
network policy is offline; approved fetches retrieve public reference resources,
never upload study matrices or clinical identifiers.

KEGG is disabled absent documented operation-specific entitlement; academic website
access does not authorize a public/commercial service or redistribution. Respect
published API restrictions and rate limits where use is approved. Do not vendor
KEGG dumps. UniProt/HMDB/RefMet/LIPID MAPS/LION/BioPAN/GO/Rhea/ChEMBL or other routes
also require actual identifier/coverage/version/rights qualification before live
activation. Their mention in a plan is not evidence of installed access.

Reactome's current license page separates data, software and art; old release-news
summaries are not the final terms. Data use, figure reuse and code redistribution
must use the applicable current terms for that artifact. No automatic blanket
license claim is made in this package. Domain/protein aliases alone never create
new web-service permission. Curated local mapping fixtures can test adapter behavior
without certifying biological coverage or user entitlements.

## Execution host and model fleet

The current authoring environment did not run the target R/Python/Quarto pipeline
or inspect the user's HPC installation. Historical provisioning notes do not prove
present-day readiness. Resolve the real host and resources once, then preserve a
capability ledger; do not repeatedly perform expensive environment discovery.

`fleet.request.json` is a **role request**, not native Codex configuration. Sol
must inspect the actual installed host's supported agent configuration/skills,
resolve requested Sol high/Luna max/Terra max/Astra low, and verify effective model
and effort after spawn. Supported model names in documentation do not establish
account authentication or per-model max availability. If an alias cannot be
resolved or the host cannot set effective models independently, report a precise
capability blocker instead of silently running every worker as expensive Sol.

Use project-local agent configuration/worktrees. No recursive worker delegation.
Default max 3 coding + 2 documentation with max 4 active workers total, and one
heavy model fit at a time. Those are explicit budget choices, not a benchmarked
optimum. Token/currency spend is recorded only if the provider exposes actual
usage; no estimated dollar savings are fabricated. When a cap or authorization
is absent, do not start open-ended paid work. Bound retries and reviews.

## Publication, Notion and privacy

This repository was verified PUBLIC. Public status is not permission to publish
participant data or private infrastructure. Git holds reviewed code/specification,
synthetic fixtures and safe evidence summaries. Real matrices, clinical source
records/identity crosswalks, raw outputs, private paths, credentials and unlicensed
resources stay in approved local storage. Review future .gitignore and secret/data
scans in A-T01; do not delete preexisting user files to make a scan pass.

The drafting session had a working GitHub read connector but no exposed write/PR
action; the local runtime could not clone GitHub, and no authenticated CLI write
route was available. This delivered bundle is an overlay, not a pushed commit.
Apply in an authenticated clone, review a normal branch diff and publish through
an ordinary branch/PR when supported. No force push, default-branch overwrite or
repository visibility change is authorized.

Notion synchronization is optional in this turn because no exact destination was
requested. If a verified configured project page is available in execution, read
it first and send safe milestone summaries only. Otherwise record a pending sync
entry without guessing a page or making Notion access block runnable code. Never
claim a sync, push, worker run or scientific test without an actual result.
