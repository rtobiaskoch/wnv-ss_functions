#' Calculate mean mosquito abundance per light trap
#'
#' Computes mean count per trap-night (`abund = mosq_L / trap_L`) with a 95%
#' normal-approximation confidence interval, grouped by `grp_var`. Filters to
#' light-trap collections (`method == "L"`), excludes malfunction rows, and
#' restricts species to `spp_keep` before summarising. The LCI is clamped at 0
#' (a small-sample normal-approx CI can otherwise extend below zero).
#'
#' Input is the cleaned all-species culex datasheet from [wnv_s_clean()] /
#' [culex_dedup()] — not the pooled-PCR datasheet used by [calc_pir()].
#'
#' @param df Cleaned culex datasheet. Must contain `trap_id`, `year`, `week`,
#'   `zone`, `zone2`, `trap_status`, `method`, `spp`, `total`.
#' @param grp_var Character vector of columns to group by before summarising.
#'   Default `c("zone", "year", "week", "spp")`. Use `"zone2"` instead of
#'   `"zone"` to roll FC quadrants up to the composite group.
#' @param spp_keep Species to include. Default `wnvSurv::spp_levels`
#'   (`c("Tarsalis", "Pipiens")`).
#' @param rm_zone Zones to drop entirely before computing. Default `NULL`.
#'
#' @return A tibble with one row per group containing `trap_L` (number of
#'   distinct light traps), `mosq_L` (sum of `total`), `abund` (mean per
#'   trap), `abund_sd`, and `abund_lci` / `abund_uci`. If `zone2` was the
#'   grouping variable it is renamed back to `zone`. A `spp = "All"` column
#'   is added when species was not in the grouping.
#'
#' @importFrom dplyr filter group_by summarize mutate rename n_distinct if_else
#' @importFrom rlang syms .data
#' @importFrom stats sd
#' @export
calc_abund <- function(df,
                       grp_var  = c("zone", "year", "week", "spp"),
                       spp_keep = wnvSurv::spp_levels,
                       rm_zone  = NULL) {

  if (any(!grp_var %in% colnames(df))) {
    stop("One or more grouping variables (grp_var) do not exist in `df`.")
  }

  req_var <- c("trap_id", "year", "week", "zone", "zone2",
               "trap_status", "method", "spp", "total")
  missing <- setdiff(req_var, names(df))
  if (length(missing) > 0) {
    stop(
      "Required variables missing from `df`: ",
      paste0(missing, collapse = ", ")
    )
  }

  grp_sym <- rlang::syms(grp_var)

  abund <- df |>
    dplyr::filter(.data$method == "L") |>
    dplyr::filter(.data$trap_status != "malfunction") |>
    dplyr::filter(.data$spp %in% spp_keep) |>
    dplyr::filter(!.data$zone %in% rm_zone) |>
    dplyr::group_by(!!!grp_sym) |>
    dplyr::summarize(
      trap_L   = if (all(is.na(.data$trap_id))) 0 else dplyr::n_distinct(.data$trap_id, na.rm = TRUE),
      mosq_L   = sum(.data$total, na.rm = TRUE),
      abund_sd = round(stats::sd(.data$total), 4),
      .groups  = "drop"
    ) |>
    dplyr::mutate(
      abund     = round(.data$mosq_L / .data$trap_L, 4),
      abund_lci = round(.data$abund - 1.96 * (.data$abund_sd / .data$trap_L^0.5), 4),
      abund_uci = round(.data$abund + 1.96 * (.data$abund_sd / .data$trap_L^0.5), 4)
    ) |>
    dplyr::mutate(
      abund_lci = dplyr::if_else(.data$abund_lci < 0, 0, .data$abund_lci)
    )

  if ("zone2" %in% names(abund)) {
    abund <- dplyr::rename(abund, zone = "zone2")
  }
  if (!"spp" %in% names(abund)) {
    abund <- dplyr::mutate(abund, spp = "All")
  }

  abund
}
