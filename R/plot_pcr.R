#' Plot qPCR copy-number distribution for one virus target
#'
#' Filters the qPCR result table to a single `virus` (matched against
#' `target_name`) and to wells with `csu_id` matching `pattern_2_keep`
#' (defaults to recognised sample-type prefixes), then plots jittered
#' `log(copies)` by `sample_type`, coloured by `test_code` and shaped by
#' `amp_status`. A red dashed line marks `log(copy_threshold)` — the
#' detection limit.
#'
#' @param data qPCR result table with columns `target_name`, `csu_id`,
#'   `sample_type`, `cq`, `copies`, `test_code`, `amp_status`.
#' @param virus Target virus, e.g. `"WNV"`. Matched against `target_name`.
#' @param copy_threshold Numeric detection threshold in copies; plotted as a
#'   horizontal dashed line at `log(copy_threshold)`.
#' @param week_filter Week label used in the plot title (does not filter
#'   data — pre-filter `data` if needed).
#' @param pattern_2_keep Regex of `csu_id` prefixes to retain. Default
#'   matches WNV samples and standard controls.
#'
#' @return A `ggplot` object.
#'
#' @importFrom dplyr filter mutate if_else
#' @importFrom stringr str_detect regex
#' @importFrom ggplot2 ggplot aes geom_jitter geom_hline scale_shape_manual
#'   ggtitle theme theme_minimal
#' @importFrom rlang .data
#' @export
plot_pcr <- function(data,
                     virus,
                     copy_threshold,
                     week_filter,
                     pattern_2_keep = "WNV|CSU|RMRP|CDC|pos|neg") {

  log_threshold <- log(copy_threshold)

  data_filtered <- data |>
    dplyr::filter(.data$target_name == virus) |>
    dplyr::filter(stringr::str_detect(
      .data$csu_id, stringr::regex(pattern_2_keep, ignore_case = TRUE)
    )) |>
    dplyr::mutate(
      cq         = dplyr::if_else(.data$cq == 55.55, 40, .data$cq),
      test_code  = dplyr::if_else(.data$amp_status == "No Amp", 0L,
                                  as.integer(.data$test_code)),
      test_code  = factor(.data$test_code, levels = c("1", "0")),
      log_copies = log(.data$copies)
    )

  ggplot2::ggplot(data_filtered) +
    ggplot2::geom_jitter(
      ggplot2::aes(x = .data$sample_type, y = .data$log_copies,
                   color = .data$test_code, shape = .data$amp_status),
      size = 3, alpha = 0.6
    ) +
    ggplot2::geom_hline(yintercept = log_threshold,
                        linetype   = "dashed", color = "red") +
    ggplot2::scale_shape_manual(values = c("Amp" = 16, "No Amp" = 1,
                                           "Inconclusive" = 8)) +
    ggplot2::ggtitle(paste0("Week ", week_filter, " ", virus)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

#' Plot qPCR standard curves over time
#'
#' Filters to standards and positive controls whose `csu_id` prefix matches
#' the `target`, recodes `cq == 55.55` (the "no-amp" sentinel) to 40, then
#' draws `cq` vs `log(copies)` lines/points coloured by week, faceted by
#' target. Used to monitor assay drift across the season.
#'
#' @param df A merged qPCR + platemap table containing `sample_type`,
#'   `year`, `week`, `plate`, `target`, `log_copies`, `cq`, `csu_id`.
#'
#' @return A `ggplot` object.
#'
#' @importFrom dplyr mutate filter if_else
#' @importFrom stringr str_detect str_to_lower str_extract
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_y_reverse
#'   facet_wrap ggtitle theme_classic
#' @importFrom rlang .data
#' @export
plot_std <- function(df) {
  req_cols <- c("sample_type", "year", "week", "plate",
                "target", "log_copies", "cq", "csu_id")
  missing_cols <- setdiff(req_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("plot_std: required columns missing: ",
         paste(missing_cols, collapse = ", "))
  }

  df |>
    dplyr::mutate(
      sample_type = dplyr::if_else(stringr::str_detect(.data$sample_type, "std"),
                                   "std", "pos_ctrl"),
      grp = paste(.data$year, .data$week, .data$plate,
                  .data$target, .data$sample_type, sep = "-")
    ) |>
    dplyr::filter(
      stringr::str_to_lower(.data$target) ==
        stringr::str_extract(.data$csu_id, "^[^_]*")
    ) |>
    dplyr::mutate(cq = dplyr::if_else(.data$cq == 55.55, 40, .data$cq)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$log_copies, y = .data$cq,
                                 color = .data$week, group = .data$grp)) +
    ggplot2::geom_point(alpha = 0.4, size = 3) +
    ggplot2::geom_line() +
    ggplot2::scale_y_reverse() +
    ggplot2::facet_wrap(~ .data$target) +
    ggplot2::ggtitle("Standards by Week") +
    ggplot2::theme_classic()
}
