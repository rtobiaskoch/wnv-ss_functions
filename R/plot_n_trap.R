#' Bar chart of trap counts per week, by status, faceted by zone × year
#'
#' Collapses duplicate `(trap_id, year, week)` rows from joined sources to a
#' single representative `trap_status` using the priority
#' **malfunction > culex > no culex > no mosquitoes**, then counts distinct
#' trap IDs per zone × week and plots as a stacked column chart faceted by
#' `zone2 ~ year`. Colours come from [trap_status_colors].
#'
#' @param df A cleaned culex datasheet with at least `trap_id`, `year`,
#'   `week`, `zone`, `zone2`, and `trap_status`. Run [wnv_s_clean()] first.
#' @param rm_zone Zones to drop entirely before plotting. Default `NULL`.
#' @param year_start Earliest year to include (inclusive). Default `NULL`
#'   (no lower bound).
#' @param year_end Latest year to include (inclusive). Default `NULL`
#'   (no upper bound).
#' @param week_breaks Numeric vector of x-axis breaks. Default `seq(23, 37,
#'   by = 5)` (the canonical surveillance season).
#' @param week_limits Numeric length-2 vector of x-axis limits. Default
#'   `c(23, 37)`.
#'
#' @return A `ggplot` object.
#'
#' @importFrom dplyr filter group_by ungroup mutate summarise n_distinct case_when coalesce
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual scale_color_manual
#'   scale_x_continuous facet_grid theme_classic
#' @importFrom rlang .data
#' @export
plot_n_trap <- function(
  df,
  rm_zone = NULL,
  year_start = NULL,
  year_end = NULL,
  week_breaks = seq(23, 37, by = 5),
  week_limits = c(23, 37)
) {
  if (!"zone2" %in% names(df)) {
    stop("`df` must have a `zone2` column - run wnv_s_clean() first.")
  }

  df |>
    dplyr::filter(!.data$zone %in% rm_zone) |>
    dplyr::filter(
      .data$year >= dplyr::coalesce(year_start, -Inf),
      .data$year <= dplyr::coalesce(year_end, Inf)
    ) |>
    dplyr::group_by(.data$trap_id, .data$year, .data$week, .data$zone2) |>
    dplyr::mutate(
      trap_status = dplyr::case_when(
        any(.data$trap_status == "malfunction", na.rm = TRUE) ~ "malfunction",
        any(.data$trap_status == "culex", na.rm = TRUE) ~ "culex",
        any(.data$trap_status == "no culex", na.rm = TRUE) ~ "no culex",
        TRUE ~ "no mosquitoes"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$year, .data$week, .data$zone2, .data$trap_status) |>
    dplyr::summarise(n = dplyr::n_distinct(.data$trap_id), .groups = "drop") |>
    ggplot2::ggplot(ggplot2::aes(
      .data$week,
      .data$n,
      color = .data$trap_status,
      fill = .data$trap_status
    )) +
    ggplot2::geom_col(alpha = 0.7) +
    ggplot2::scale_fill_manual(
      values = wnvSurv::trap_status_colors,
      na.value = "grey40"
    ) +
    ggplot2::scale_color_manual(
      values = wnvSurv::trap_status_colors,
      na.value = "grey40"
    ) +
    ggplot2::scale_x_continuous(breaks = week_breaks, limits = week_limits) +
    ggplot2::facet_grid(zone2 ~ year, scales = "free_y") +
    ggplot2::theme_classic()
}
