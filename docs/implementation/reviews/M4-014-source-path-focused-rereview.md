# M4-014 source path repair — focused Sol-high re-review

- Date: 2026-10-16
- Reviewer: `/root/m4_014_repair1_sol_rereview` (fresh Sol 6.1 high, read-only)
- Repair candidate: `74335d16e732dcaa840ed077f619fb8261d613cc`
- Verdict: **ADDRESSED**
- New findings: P0 0, P1 0

The repair changes only `tests/testthat/test-B-T07.R` (+5/-1). The source helper
still resolves and executes the real checkout CLI, but now runs from its
directory and passes the executable basename so Rscript never receives an
absolute script argument containing spaces. `on.exit()` restores the caller's
working directory after success, nonzero child status or launch error. The
installed dispatcher branch is unchanged.

All expressions outside `t07_cli_smoke()` are unchanged, so every scientific,
hash, restart, privacy and reproduction assertion remains intact; no skip was
added. Fresh execution from a checkout path containing spaces and fresh
installed-package parity each passed 124 expectations with zero failures or
skips. Each retained eight known absent-Git-metadata warnings; there were no
path-corruption, CLI-launch or `normalizePath()` warnings.

Allowed next transition: serial integration and fresh integrated source,
installed, full-suite and clean archive package gates. This receipt does not
accept B-T07 or M4 and does not replace the immutable Astra milestone audit.
