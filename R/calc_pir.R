#' Calculate Pooled Infection Rate (PIR) via the Firth MLE
#'
#' Estimates the bias-corrected maximum-likelihood pooled infection rate
#' (infected mosquitoes per pool member) for each `grp_var` combination, using
#' [PooledInfRate::pIR()] with `pt.method = "firth"`. Pools of size 1 — which
#' break the MLE — are handled via a separate edge-case branch that assigns
#' PIR = `test_code` (0 or 1) for those single-mosquito pools. Missing
#' group/zone combinations are completed with PIR = 0.
#'
#' Input is the pooled (PCR-tested) datasheet, **not** the all-species culex
#' trap datasheet used by [calc_abund()]. Required columns include
#' `test_code` (0 = negative, 1 = positive).
#'
#' @param df Pool-level datasheet. Must contain `trap_id`, `year`, `week`,
#'   `zone`, `zone2`, `method`, `spp`, `total`, `test_code`.
#' @param grp_var Character vector of columns to group by. Default
#'   `c("zone", "year", "week", "spp")`. Cannot contain `"csu_id"`.
#' @param zone_complete Character vector of zones to complete in the output
#'   grid. Default `wnvSurv::zone_lvls`. `rm_zone` is subtracted from this.
#' @param rm_zone Zones to drop entirely. Default `NULL`.
#'
#' @return A tibble with one row per `grp_var` combination containing `pir`,
#'   `pir_lci`, `pir_uci`. Renames `zone2` to `zone` if present and adds
#'   `spp = "All"` when species was not in the grouping.
#'
#' @importFrom dplyr filter group_by summarise mutate select rename bind_rows
#' @importFrom tidyr unite complete separate
#' @importFrom rlang syms .data
#' @export
calc_pir <- function(df,
                     grp_var       = c("zone", "year", "week", "spp"),
                     zone_complete = wnvSurv::zone_lvls,
                     rm_zone       = NULL) {

  if (!requireNamespace("PooledInfRate", quietly = TRUE)) {
    stop("`PooledInfRate` is required. Install with: ",
         "devtools::install_github('CDCgov/PooledInfRate')")
  }

  if ("csu_id" %in% grp_var) {
    stop("Cannot calculate PIR using csu_id as a grouping variable. ",
         "CSU IDs represent individual pools, not trap-level data.")
  }

  if (any(!grp_var %in% colnames(df))) {
    stop("One or more grouping variables (grp_var) do not exist in `df`.")
  }

  req_var <- c("trap_id", "year", "week", "zone", "zone2",
               "method", "spp", "total")
  missing <- setdiff(req_var, names(df))
  if (length(missing) > 0) {
    stop(
      "Required variables missing from `df`: ",
      paste0(missing, collapse = ", ")
    )
  }

  grp_var_sym   <- rlang::syms(grp_var)
  zone_complete <- setdiff(zone_complete, rm_zone)

  # Main PIR branch: pools with > 1 mosquito (Firth MLE is valid).
  df_pir <- df |>
    dplyr::filter(!.data$zone %in% rm_zone) |>
    dplyr::filter(.data$total > 1) |>
    tidyr::unite(col = "grp", dplyr::all_of(grp_var), sep = "_", remove = FALSE)

  mle <- PooledInfRate::pIR(test_code ~ total | grp, data = df_pir,
                            pt.method = "firth")

  df_pir <- as.data.frame(mle) |>
    tidyr::separate(.data$grp, into = {{ grp_var }}, sep = "_") |>
    dplyr::mutate(
      year    = as.numeric(.data$year),
      week    = as.numeric(.data$week),
      pir     = round(.data$P, 4),
      pir_lci = round(.data$Lower, 4),
      pir_uci = round(.data$Upper, 4)
    ) |>
    dplyr::select(-"P", -"Upper", -"Lower")

  # Edge case: single-mosquito pools (total == 1) — Firth MLE undefined.
  # Replaces upstream's `sum(total, rm.na = T)` bug with the correct
  # `na.rm = TRUE`.
  single_pools <- df |>
    dplyr::filter(!.data$zone %in% rm_zone) |>
    dplyr::group_by(!!!grp_var_sym) |>
    dplyr::summarise(total = sum(.data$total, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(.data$total == 1)

  if (nrow(single_pools) > 0) {
    df_pir1 <- df |>
      dplyr::filter(!.data$zone %in% rm_zone) |>
      dplyr::group_by(!!!grp_var_sym) |>
      dplyr::summarise(
        total     = sum(.data$total, na.rm = TRUE),
        test_code = max(.data$test_code),
        .groups   = "drop"
      ) |>
      dplyr::filter(.data$total <= 1) |>
      # Single-mosquito pool: pir = 1 if positive, 0 if negative.
      dplyr::mutate(
        pir     = dplyr::if_else(.data$test_code == 0 | is.na(.data$test_code), 0, 1),
        pir_lci = dplyr::if_else(.data$test_code == 0 | is.na(.data$test_code), 0, 1),
        pir_uci = dplyr::if_else(.data$test_code == 0 | is.na(.data$test_code), 0, 1)
      ) |>
      dplyr::select(dplyr::all_of(grp_var), "pir", "pir_lci", "pir_uci")

    df_pir <- dplyr::bind_rows(df_pir, df_pir1)
  }

  # Complete missing group combinations with PIR = 0.
  df_pir <- tidyr::complete(df_pir, !!!grp_var_sym)
  df_pir[is.na(df_pir)] <- 0

  if ("zone2" %in% names(df_pir)) {
    df_pir <- dplyr::rename(df_pir, zone = "zone2")
  }
  if (!"spp" %in% names(df_pir)) {
    df_pir <- dplyr::mutate(df_pir, spp = "All")
  }

  df_pir
}
