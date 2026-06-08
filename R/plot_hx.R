#' Build the long-format current-vs-historical dataframe (weekly, wide-summarise)
#'
#' Binds current-year and historical estimates, drops requested zones,
#' pivots long across estimate columns, then pivots back wide on estimate
#' name and finally summarises the historical strata to a single mean per
#' `(zone, week, spp)`. The output is one row per zone × week × spp × type
#' (`hx` / `current`) with `abund`, `pir`, and `vi` columns. Designed to feed
#' the season-overlay layout used by [plot_hx()].
#'
#' @param ytd Current year-to-date estimates (from [calc_vi()]).
#' @param hx Historical multi-year estimates with the same schema as `ytd`,
#'   plus a `type` column distinguishing them.
#' @param rm_zone Zones to drop. Default `NULL`.
#' @param grp_vars Grouping columns to pivot around. Default
#'   `c("year", "week", "zone", "spp")`.
#'
#' @return A tibble with one row per zone × week × spp × type.
#'
#' @importFrom dplyr bind_rows filter select any_of group_by summarise
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data
#' @keywords internal
clean_long_hx_wk <- function(ytd, hx,
                             rm_zone  = NULL,
                             grp_vars = c("year", "week", "zone", "spp")) {

  dplyr::bind_rows(ytd, hx) |>
    dplyr::filter(!.data$zone %in% rm_zone) |>
    dplyr::select(-dplyr::any_of(c("mosq_L", "trap_L", "zone2"))) |>
    tidyr::pivot_longer(
      cols      = -dplyr::any_of(c(grp_vars, "type")),
      names_to  = "est",
      values_to = "value"
    ) |>
    dplyr::mutate(
      type = factor(.data$type, levels = c("hx", "current")),
      zone = factor(.data$zone, levels = wnvSurv::zone_lvls)
    ) |>
    tidyr::pivot_wider(names_from = "est", values_from = "value") |>
    dplyr::group_by(.data$zone, .data$week, .data$spp, .data$type) |>
    dplyr::summarise(
      abund = mean(.data$abund, na.rm = TRUE),
      pir   = mean(.data$pir,   na.rm = TRUE),
      vi    = mean(.data$vi,    na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$spp == "All")
}

#' Build the long-format current-vs-historical dataframe (yearly lines)
#'
#' Binds and reshapes estimates into a long format suitable for
#' [plot_hx_line()] — one row per zone × week × spp × year × type ×
#' estimate. Drops `rm_zone` and restricts to species in `spp_keep`.
#'
#' @param ytd Current year-to-date estimates.
#' @param hx Historical estimates.
#' @param rm_zone Zones to drop. Default `"BC"`.
#' @param grp_var Columns to keep alongside the estimate columns. Default
#'   `c("year", "week", "zone", "spp", "type")`.
#' @param est_keep Estimate names to retain. Default `c("abund", "vi",
#'   "pir")`.
#' @param spp_keep Species to retain. Default `c("All")`.
#'
#' @return A long-format tibble.
#'
#' @importFrom dplyr bind_rows filter select any_of
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @keywords internal
clean_long_hx <- function(ytd, hx,
                          rm_zone  = "BC",
                          grp_var  = c("year", "week", "zone", "spp", "type"),
                          est_keep = c("abund", "vi", "pir"),
                          spp_keep = c("All")) {

  dplyr::bind_rows(ytd, hx) |>
    dplyr::filter(!.data$zone %in% rm_zone) |>
    dplyr::filter(.data$spp %in% spp_keep) |>
    dplyr::select(dplyr::any_of(c(grp_var, est_keep))) |>
    tidyr::pivot_longer(
      cols      = dplyr::all_of(est_keep),
      names_to  = "est",
      values_to = "value"
    ) |>
    dplyr::filter(.data$est %in% est_keep)
}

#' Plot current-vs-historical surveillance estimates (weekly area)
#'
#' Draws `value` (e.g. `abund`, `pir`, or `vi`) by week, with one area per
#' type (`current` overlaid on `hx`), faceted by zone. Designed to be fed by
#' [clean_long_hx_wk()].
#'
#' @param df Long-format dataframe produced by [clean_long_hx_wk()] with
#'   columns `week`, `zone`, `type`, and the column passed as `value`.
#' @param value Bare column name (unquoted) to plot on the y-axis (e.g.
#'   `abund`).
#' @param text Plot title.
#' @param palette Named character vector of fill/colour values keyed by
#'   `type`. Default `c(current = "#e9724c", hx = "grey50")`.
#'
#' @return A `ggplot` object.
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_area facet_grid theme_classic
#'   ggtitle scale_x_continuous scale_color_manual scale_fill_manual
#' @importFrom rlang .data
#' @export
plot_hx <- function(df, value, text,
                    palette = c("current" = "#e9724c", "hx" = "grey50")) {

  min_week <- min(df$week, na.rm = TRUE)
  max_week <- max(df$week, na.rm = TRUE)

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$week, y = {{ value }},
                 color = .data$type, fill = .data$type, group = .data$type)
  ) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_area(position = "dodge", alpha = 0.3) +
    ggplot2::facet_grid(zone ~ .) +
    ggplot2::theme_classic() +
    ggplot2::ggtitle(text) +
    ggplot2::scale_x_continuous(
      limits = c(min_week, max_week),
      breaks = seq(min_week, max_week, by = 2)
    ) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_fill_manual( values = palette)
}

#' Plot historical lines + current-year area (one panel per estimate × zone)
#'
#' Draws historical years as a grey-gradient line stack and the current year
#' as a solid coloured area, with one facet per estimate × zone. Designed to
#' be fed by [clean_long_hx()].
#'
#' @param df Long-format dataframe with `week`, `value`, `zone`, `est`,
#'   `type`, and `color_var`.
#' @param text Plot title.
#' @param color_var Bare column name to colour historical lines by (usually
#'   `year`). Default `year`.
#' @param current_color Fill colour for the current-year area. Default
#'   `"#e9724c"`.
#' @param hx_start_grey Low end of the historical-year grey gradient.
#'   Default `"grey40"`.
#' @param hx_end_grey High end of the gradient. Default `"grey80"`.
#'
#' @return A `ggplot` object.
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_line geom_area facet_grid
#'   theme theme_classic ggtitle scale_color_gradient scale_fill_manual
#'   guide_colorbar guide_legend
#' @importFrom rlang enquo as_name .data
#' @importFrom dplyr filter
#' @keywords internal
plot_hx_line <- function(df, text,
                         color_var     = year,
                         current_color = "#e9724c",
                         hx_start_grey = "grey40",
                         hx_end_grey   = "grey80") {

  color_var <- rlang::enquo(color_var)

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x     = .data$week,
      y     = .data$value,
      group = interaction(!!color_var, .data$type)
    )
  ) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::facet_grid(est ~ zone, scales = "free_y") +
    ggplot2::theme_classic() +
    ggplot2::ggtitle(text)

  if ("type" %in% names(df) && any(df$type == "hx")) {
    p <- p + ggplot2::geom_line(
      data = ~ dplyr::filter(.x, .data$type == "hx"),
      ggplot2::aes(color = !!color_var),
      alpha = 0.5,
      linewidth = 0.5
    ) +
      ggplot2::scale_color_gradient(
        low  = hx_start_grey,
        high = hx_end_grey,
        guide = ggplot2::guide_colorbar(title = "Year")
      )
  }

  if ("type" %in% names(df) && any(df$type == "current")) {
    p <- p + ggplot2::geom_area(
      data = ~ dplyr::filter(.x, .data$type == "current"),
      ggplot2::aes(fill = "Current"),
      alpha = 0.8,
      position = "identity"
    ) +
      ggplot2::scale_fill_manual(
        values = c("Current" = current_color),
        guide = ggplot2::guide_legend(title = NULL)
      )
  }

  if (!"type" %in% names(df)) {
    p <- p + ggplot2::geom_line(ggplot2::aes(color = !!color_var))
  }

  p + ggplot2::theme(legend.position = "bottom")
}
