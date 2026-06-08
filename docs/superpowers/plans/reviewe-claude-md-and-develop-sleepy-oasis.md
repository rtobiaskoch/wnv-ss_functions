# Plan — Build `wnvSurv` R package from two source repos

## Context

The `wnv-ss_functions` directory is empty except for `CLAUDE.md`, which is being used as a spec file. The spec says: "use `wnv-ss_trap_hx_combiner` and `wnv-ss-wkly_report` to develop an R package that can be used between these and any other projects. Use the most recent versions when there are naming conflicts."

**Problem:** functions are duplicated across two consumer projects, drift out of sync, and several violate RSE principles (interactive `readline()` calls in pipelines, hardcoded `cat()` chatter, package data inlined into function defaults, no namespace).

**Outcome:** a CRAN-style R package `wnvSurv` (in this repo) that the two source projects can `library()` instead of `source()`-ing local copies. v1 ships only the 13 priority functions plus their direct helpers, refactored to be pure-function-friendly, parameter-driven, and testable.

**Version-conflict resolution (from `git log -1 --format=%ai`):**

| Function | trap_hx_combiner | wkly_report | Winner |
|---|---|---|---|
| `key_rename` | 2026-05-15 | 2025-06-12 | trap_hx_combiner |
| `make_key` | 2026-05-19 | — | trap_hx_combiner |
| `wnv_s_clean` | 2026-05-21 | 2025-08-01 | trap_hx_combiner |
| `culex_dedup` | 2026-05-19 | — | trap_hx_combiner |
| `plot_n_trap` | 2026-05-20 | — | trap_hx_combiner |
| `gsheet_pull_prompt` | graveyard only | 2025-07-10 | wkly_report |
| `clean_platemap` | — | 2025-07-01 | wkly_report |
| `calc_abund/pir/vi/all` | graveyard only | 2025-07–08 | wkly_report |
| `plot_pcr`, `plot_hx` | graveyard only | 2025-08 | wkly_report |

---

## Design decisions (confirmed with user)

1. **Package name**: `wnvSurv`
2. **Scope**: priority 13 functions + their direct helpers (e.g. `parse_flexible_date` for `wnv_s_clean`, `clean_long_hx_wk` for `plot_hx`)
3. **`gsheet_pull_prompt` refactor**: split into pure `gsheet_pull(key, dest_path, force=FALSE)` (no `readline()`) + thin `gsheet_pull_prompt()` wrapper that calls the pure form after a prompt
4. **Defaults as package data**: `zone_lvls`, `spp_levels`, `trap_status_colors`, `fc_zones` ship as `data/*.rda` exposed at `wnvSurv::zone_lvls` etc.; function defaults reference them
5. **Verbose handling**: add `verbose = TRUE` arg; wrap all chatter in `if (verbose) cli::cli_alert_*()`. Default `TRUE` preserves current UX
6. **`PooledInfRate` dep**: `Imports:` + `Remotes: CDCgov/PooledInfRate` in DESCRIPTION
7. **No pipeline**: functions-only package; consumer repos keep their own `_targets.R`

---

## Function inventory for v1

### Source: `wnv-ss_trap_hx_combiner/R/` (newer)
- `key_rename.R` → `R/key_rename.R`
- `fun_make_key.R` → `R/make_key.R`
- `fun_wnv_s_clean.R` → `R/wnv_s_clean.R` (depends on `parse_flexible_date`)
- `fun_dedup_culex.R` → `R/culex_dedup.R`
- `fun_plot_n_trap.R` → `R/plot_n_trap.R`
- `fun_parse_flexible_date.R` → `R/parse_flexible_date.R` (internal helper)
- `palettes.R` (`trap_status_colors`) → `data/trap_status_colors.rda` via `data-raw/`

### Source: `wnv-ss-wkly_report/utils/`
- `fun_gsheet_pull_prompt.R` → split into `R/gsheet_pull.R` (pure) + `R/gsheet_pull_prompt.R` (wrapper)
- `fun_clean_platemap.R` → `R/clean_platemap.R`
- `fun_calc_abund.R` → `R/calc_abund.R`
- `fun_calc_pir.R` → `R/calc_pir.R`
- `fun_calc_vi.R` → `R/calc_vi.R`
- `fun_calc_all.R` → `R/calc_all.R` (orchestrator)
- `fun_plot_pcr.R` → `R/plot_pcr.R`
- `fun_plot_hx.R` → `R/plot_hx.R` (+ helpers `clean_long_hx_wk`, `clean_long_hx`, `plot_hx_line` as internals)

---

## Package skeleton

```
wnv-ss_functions/
├── DESCRIPTION              # Package: wnvSurv, Imports, Remotes: CDCgov/PooledInfRate
├── NAMESPACE                # roxygen-generated
├── CLAUDE.md                # rewritten to per-repo standard (Commands/Data/Domain)
├── README.md                # install + quickstart
├── R/                       # 14 .R files (one function each)
├── data/                    # zone_lvls.rda, spp_levels.rda, trap_status_colors.rda, fc_zones.rda
├── data-raw/                # scripts that build /data objects (gitignored output, scripts committed)
│   ├── zone_lvls.R
│   ├── spp_levels.R
│   ├── trap_status_colors.R
│   └── fc_zones.R
├── tests/testthat/          # one test_*.R per function
├── inst/                    # (empty for v1)
└── man/                     # roxygen-generated .Rd files
```

---

## RSE-compliance refactors (applied per function)

For EACH ported function, the migration pass does:

1. **Add roxygen2 header** with `@param`, `@return`, `@export`, `@examples`
2. **Replace `library()`** in the function body with namespaced calls (`dplyr::filter`, etc.)
3. **Replace `cat()` / `message()`** with `if (verbose) cli::cli_alert_info(...)`; add `verbose = TRUE` arg
4. **Replace hardcoded defaults** (e.g. `zone_lvls = c("zone_1", "zone_2", ...)`) with `zone_lvls = wnvSurv::zone_lvls`
5. **Remove side-effect file I/O** where avoidable — return data; let caller `write_csv()` (applies to `check_data` if pulled later, not v1)
6. **`gsheet_pull` split**: extract the `googledrive::drive_download()` + `drive_auth()` core into `gsheet_pull(key, dest_path, force=FALSE)`; `gsheet_pull_prompt()` becomes 5-line wrapper that runs `readline()` then calls `gsheet_pull(force=TRUE)`
7. **No reassignment of inputs** (e.g. `df <- df |> mutate(...)` is OK inside a function but no `<<-`)
8. **Bug fix in `fun_check_data.R`** (undefined `cq_data` at line 183) is OUT OF SCOPE for v1 — function not ported
9. **Verify each refactored function still produces identical output** on a small fixture from one of the source repos (snapshot test under `tests/testthat/`)

---

## Implementation phases

### Phase A — Skeleton + tooling (1 commit)
- `usethis::create_package(".")` to scaffold DESCRIPTION/NAMESPACE/.Rbuildignore
- `usethis::use_testthat(3)`, `use_roxygen_md()`, `use_pipe()`, `use_mit_license()` (or chosen)
- `usethis::use_package()` for each Import: dplyr, tidyr, stringr, purrr, lubridate, rlang, cli, ggplot2, janitor, readxl, googlesheets4, googledrive, PooledInfRate (the last via `usethis::use_dev_package()` to record under Remotes)
- Rewrite `CLAUDE.md` to per-repo template (Commands / Data notes / Domain context — one paragraph each)

### Phase B — Package data (1 commit)
- Write `data-raw/zone_lvls.R`, `spp_levels.R`, `trap_status_colors.R`, `fc_zones.R`. Each ends with `usethis::use_data(<obj>, overwrite = TRUE)`
- Document via `R/data.R` with `@docType data` roxygen blocks

### Phase C — Port functions in 4 sub-commits (one per cluster)
1. **Cleaning cluster**: `key_rename`, `make_key`, `parse_flexible_date`, `wnv_s_clean`, `culex_dedup`, `clean_platemap`
2. **GSheet cluster**: `gsheet_pull`, `gsheet_pull_prompt`
3. **Calc cluster**: `calc_abund`, `calc_pir`, `calc_vi`, `calc_all`
4. **Plot cluster**: `plot_n_trap`, `plot_pcr`, `plot_hx` (+ `plot_hx` helpers as `@keywords internal`)

Each cluster commit includes: ported `R/*.R`, matching `tests/testthat/test-*.R`, roxygen run (`devtools::document()`).

### Phase D — Tests + CI (1 commit)
- Port existing tests from `wnv-ss_trap_hx_combiner/tests/testthat/` (wnv_s_clean) and `wnv-ss-wkly_report/tests/testthat/` (calc_abund, calc_pir, calc_vi)
- Add at least one happy-path test per remaining function using small inline fixtures
- `usethis::use_github_action_check_standard()` (optional, skip if CI not desired)

### Phase E — Integration verification (no commit, manual)
- In `wnv-ss-wkly_report`: install `wnvSurv` via `devtools::install_local("../wnv-ss_functions")`, replace one `source("utils/fun_calc_pir.R")` with `library(wnvSurv)`, re-run the weekly report. Confirm output matches.
- Same swap in `wnv-ss_trap_hx_combiner` for `wnv_s_clean`.

---

## Critical files to read while implementing

Newer versions (copy these):
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss_trap_hx_combiner/R/fun_wnv_s_clean.R`
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss_trap_hx_combiner/R/key_rename.R`
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss_trap_hx_combiner/R/fun_make_key.R`
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss_trap_hx_combiner/R/fun_dedup_culex.R`
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss_trap_hx_combiner/R/fun_plot_n_trap.R`
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss_trap_hx_combiner/R/fun_parse_flexible_date.R`
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss_trap_hx_combiner/R/palettes.R`

Older but only source:
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss-wkly_report/utils/fun_gsheet_pull_prompt.R`
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss-wkly_report/utils/fun_clean_platemap.R`
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss-wkly_report/utils/fun_calc_abund.R`
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss-wkly_report/utils/fun_calc_pir.R`
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss-wkly_report/utils/fun_calc_vi.R`
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss-wkly_report/utils/fun_calc_all.R`
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss-wkly_report/utils/fun_plot_pcr.R`
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss-wkly_report/utils/fun_plot_hx.R`

Existing tests to port:
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss_trap_hx_combiner/tests/testthat/test-wnv_s_clean.R`
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss-wkly_report/tests/testthat/test-calc_abund.R`
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss-wkly_report/tests/testthat/test-calc_pir.R`
- `/Users/user/Programming_Directory/Ebel_Lab/wnv-ss-wkly_report/tests/testthat/test-calc_vi.R`

---

## Verification

After each phase:
```r
devtools::document()      # roxygen → NAMESPACE + man/
devtools::load_all(".")   # interactive smoke
devtools::test()          # all testthat tests
devtools::check()         # R CMD check (no errors, warnings ok in v1)
```

End-to-end (Phase E):
```r
# in wnv-ss-wkly_report root
devtools::install_local("../wnv-ss_functions")
library(wnvSurv)
# swap one `source("utils/fun_calc_pir.R")` → already covered by library(wnvSurv)
targets::tar_make()       # if used; else run weekly_report.qmd
```
Confirm numeric output (e.g. PIR table) matches the pre-swap run within rounding tolerance.

---

## Out of scope for v1 (parking lot)

- `fun_check_data.R` (and its `cq_data` bug)
- All `pivot_*`, `read_source`, `expand_trap`, `fill_skeleton`, `manifest_*` from trap_hx_combiner — these belong to that repo's specific pipeline
- Google Drive upload helpers (`fun_update_gsheet`, `fun_gdrive_download`)
- A {targets} reference pipeline
- pkgdown site / vignettes
