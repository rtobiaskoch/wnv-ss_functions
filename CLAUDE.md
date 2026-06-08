# CLAUDE.md — `wnvSurv` package

This per-repo file extends the global `~/.claude/CLAUDE.md` working agreement.
It does not repeat global rules; it adds project-specific context.

## What this is

`wnvSurv` is an R package consolidating shared functions used by two
upstream pipelines in the Ebel Lab:

- [`wnv-ss_trap_hx_combiner`](https://github.com/rtobiaskoch/wnv-ss_trap_hx_combiner) — assembles long-form trap × week × species histories from VDCI, Boulder, CFC, and CMC data sources.
- [`wnv-ss-wkly_report`](https://github.com/rtobiaskoch/wnv-ss-wkly_report) — produces the weekly Culex/WNV surveillance report for Larimer County.

Before `wnvSurv` existed, both pipelines `source()`-ed their own copies of
`fun_*.R`. Drift between copies caused silent disagreements in cleaning logic.
This package is the single source of truth.

## Commands

```r
devtools::load_all(".")        # interactive smoke
devtools::document()           # roxygen → NAMESPACE + man/
devtools::test()               # run testthat suite
devtools::check()              # R CMD check
devtools::install_local(".")   # install for consumer repos
```

After editing `data-raw/*.R`:

```r
source("data-raw/zone_lvls.R") # re-runs usethis::use_data(..., overwrite = TRUE)
```

## Data notes

This package ships **no surveillance data** — only reference values:

- `zone_lvls` — ordered factor levels for Larimer County trapping zones
- `spp_levels` — canonical *Culex* species ordering (`pip`, `tar`, `ery`, ...)
- `fc_zones` — Fort Collins–only zone subset
- `trap_status_colors` — named ggplot palette for trap-status states

Surveillance CSV/XLSX data lives in the consumer repos under their respective
`1_input/` directories, not here.

## Domain context

West Nile virus (WNV) is monitored locally by trapping female *Culex*
mosquitoes, testing pools by qPCR, and reporting weekly per-zone abundance
(mean mosquitoes per trap-night), pooled infection rate (PIR, infected
mosquitoes per 1,000 tested, via the bias-corrected MLE in
[`PooledInfRate`](https://github.com/CDCgov/PooledInfRate)), and the vector
index (VI = abundance × PIR / 1000). PIR is the headline epi metric — it is
what triggers public-health adulticide decisions — so `calc_pir()`,
`calc_abund()`, and `calc_vi()` are the package's most consequential
functions and have the highest test coverage requirement.

## Package layout

```
R/                 # one function per file; verb_noun() naming
data/              # *.rda built by data-raw/ scripts (committed)
data-raw/          # scripts that produce data/ objects (committed; outputs gitignored within R session)
tests/testthat/    # one test-<function>.R per function in R/
man/               # roxygen-generated; do not hand-edit
inst/              # (empty for v1)
```

## Coding rules specific to this repo

- Every exported function takes `verbose = TRUE` and routes chatter through
  `cli::cli_alert_*()`, gated by `if (verbose)`. No bare `cat()`.
- Defaults for zone/species/palette come from package data
  (`zone_lvls = wnvSurv::zone_lvls`), never from inlined `c(...)` literals.
- `PooledInfRate` is a hard `Imports:` dependency with a `Remotes:` line —
  expect `devtools::install_local()` to pull it from GitHub.
- Interactive prompts (`readline()`) live only inside `*_prompt()` wrapper
  functions. The non-prompt core (e.g. `gsheet_pull()`) is pure and callable
  from non-interactive pipelines.

## Out of scope

`wnvSurv` is **functions only**. No `{targets}` pipeline, no Quarto reports,
no Shiny app. Those live in the consumer repos.
