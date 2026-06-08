#' Larimer County trapping zone levels
#'
#' Canonical ordering of mosquito trapping zones used across the Ebel Lab
#' surveillance pipelines. NW/NE/SE/SW are Fort Collins quadrants; FC is a
#' Fort Collins composite group; LV = Loveland, BE = Berthoud, BC = Boulder.
#' Provided as the default for `zone_lvls`/`zone_lvl` arguments throughout the
#' package.
#'
#' @format A character vector of length 8.
#' @source `config_culex_combine.yml` and `fun_wnv_s_clean.R` defaults in
#'   `wnv-ss_trap_hx_combiner`.
"zone_lvls"

#' Fort Collins quadrant subset
#'
#' The four Fort Collins quadrants — used by `calc_pir()` and `calc_vi()` to
#' aggregate sub-zones up to a single composite "FC" group when reporting at
#' city level.
#'
#' @format A character vector of length 4 (`"NE"`, `"NW"`, `"SE"`, `"SW"`).
#' @source `config_culex_combine.yml` `fc_zone:` block.
"fc_zones"

#' Canonical *Culex* species ordering
#'
#' Two species ordered with the principal WNV vector first. *Cx. tarsalis* is
#' the dominant enzootic and bridge vector for WNV in the western U.S. and is
#' listed first so it is plotted on top in stacked bars.
#'
#' @format A character vector of length 2 (`"Tarsalis"`, `"Pipiens"`).
#' @source `config_culex_combine.yml` `spp_levels:` block.
"spp_levels"

#' Trap-status color palette
#'
#' Named character vector of hex colors keyed by trap-status state, used as a
#' `ggplot2::scale_fill_manual()` palette in `plot_n_trap()` and related
#' figures. Keys match the levels of `trap_status` produced by `wnv_s_clean()`.
#'
#' @format A named character vector of length 5.
#' @source `wnv-ss_trap_hx_combiner/R/palettes.R`.
"trap_status_colors"
