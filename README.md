# wnvSurv

Suite of functions for Larimer County summer (Culex/WNV) surveillance. This
package consolidates ingest, cleaning, abundance / pooled infection rate /
vector index calculations, and standard plots used across the
[`wnv-ss_trap_hx_combiner`](https://github.com/rtobiaskoch/wnv-ss_trap_hx_combiner)
and
[`wnv-ss-wkly_report`](https://github.com/rtobiaskoch/wnv-ss-wkly_report)
pipelines, so both depend on a single source of truth.

## Install

```r
# install.packages("devtools")
devtools::install_github("rtobiaskoch/wnv-ss_functions")
# or, from a local checkout:
devtools::install_local("/path/to/wnv-ss_functions")
```

`wnvSurv` depends on the GitHub-only package
[`PooledInfRate`](https://github.com/CDCgov/PooledInfRate); it is declared in
`Remotes:` so `install_github()` / `install_local()` will pull it
automatically.

## Quickstart

```r
library(wnvSurv)

clean <- raw_culex |>
  key_rename(rename_map) |>
  wnv_s_clean(verbose = FALSE) |>
  culex_dedup()

abund <- calc_abund(clean, grp_var = c("zone", "year", "week"))
pir   <- calc_pir(clean, grp_var = c("zone", "year", "week"))
vi    <- calc_vi(abund, pir, by = c("zone", "year", "week"))

plot_n_trap(clean)
```

## Function reference

| Cluster   | Functions |
|-----------|-----------|
| Ingest    | `gsheet_pull()`, `gsheet_pull_prompt()` |
| Cleaning  | `key_rename()`, `make_key()`, `parse_flexible_date()`, `wnv_s_clean()`, `culex_dedup()`, `clean_platemap()` |
| Calc      | `calc_abund()`, `calc_pir()`, `calc_vi()`, `calc_all()` |
| Plots     | `plot_n_trap()`, `plot_pcr()`, `plot_hx()` |

Run `?wnvSurv::<function>` for argument docs.

## License

MIT © 2026 Toby Koch
