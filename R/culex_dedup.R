#' Resolve within-week collection-date near-duplicates
#'
#' Some VDCI weekly files contain two collection events for the same trap
#' within the same ISO week — one weekday (routine surveillance) and one
#' weekend (field test or aberrant run). They share the same
#' `(trap_id, spp, year, week)` key but differ in `trap_date`, so generic
#' `distinct()` will not remove them. Downstream this creates duplicate keys
#' when joining against the wnv-s_database (which aggregates to one row per
#' trap-week).
#'
#' Deduplication is conservative:
#'   * Single-date trap-weeks pass through unchanged (lone weekend
#'     collections are never removed).
#'   * Multi-date trap-weeks that include a weekend: keep the collection date
#'     with the highest total mosquito count summed across all species.
#'     Ties favour weekday dates, then the later date. Exactly one row per
#'     near-duplicate trap-week survives.
#'
#' Must be called *after* [wnv_s_clean()] so that `trap_date` is class `Date`
#' and `year` / `week` have been derived from it.
#'
#' @param df Data frame containing `key_cols` plus `trap_date` and `total`.
#' @param key_cols Character vector identifying a trap-week. Passed to
#'   [make_key()] — must have at least two elements. Default
#'   `c("trap_id", "year", "week")`.
#' @param verbose Logical. If `TRUE` (default), summary cli alerts are
#'   printed describing what was removed.
#'
#' @return Deduplicated data frame.
#'
#' @importFrom dplyr distinct group_by ungroup slice_max slice_head summarise
#'   filter select semi_join anti_join bind_rows arrange mutate across desc
#'   n_distinct
#' @importFrom lubridate wday
#' @importFrom rlang .data
#' @export
culex_dedup <- function(df,
                        key_cols = c("trap_id", "year", "week"),
                        verbose  = TRUE) {

  required <- c(key_cols, "trap_date", "total")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      "culex_dedup: missing columns: ", paste(missing, collapse = ", "),
      " - run wnv_s_clean() first."
    )
  }
  if (!inherits(df$trap_date, "Date")) {
    stop("culex_dedup: `trap_date` must be class Date - run wnv_s_clean() first.")
  }

  # Pass 1: drop fully identical rows.
  n_exact_before  <- nrow(df)
  df              <- dplyr::distinct(df)
  n_exact_removed <- n_exact_before - nrow(df)
  if (verbose && n_exact_removed > 0) {
    cli::cli_alert_info("culex_dedup: removed {n_exact_removed} exact duplicate row{?s}.")
  }

  # Pass 2: same-key same-date conflicts with differing totals -> keep max.
  n_conflict_before <- nrow(df)
  df <- df |>
    dplyr::group_by(dplyr::across(-.data$total)) |>
    dplyr::slice_max(.data$total, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  n_conflict_removed <- n_conflict_before - nrow(df)
  if (verbose && n_conflict_removed > 0) {
    cli::cli_alert_info(
      "culex_dedup: resolved {n_conflict_removed} same-date same-key conflict{?s} by keeping max total."
    )
  }

  # Build a composite key for the rest of the logic.
  df <- make_key(df, key_cols, name = "dedup_key")

  near_dup_keys <- df |>
    dplyr::group_by(.data$dedup_key) |>
    dplyr::summarise(
      n_dates  = dplyr::n_distinct(.data$trap_date),
      has_wknd = any(lubridate::wday(.data$trap_date) %in% c(1L, 7L), na.rm = TRUE),
      .groups  = "drop"
    ) |>
    dplyr::filter(.data$n_dates > 1, .data$has_wknd) |>
    dplyr::select(.data$dedup_key)

  n_near_dups <- nrow(near_dup_keys)

  if (n_near_dups == 0) {
    if (verbose) {
      cli::cli_alert_success(
        "culex_dedup: no near-duplicate trap-weeks found. No rows removed."
      )
    }
    return(dplyr::select(df, -.data$dedup_key))
  }

  best_dates <- df |>
    dplyr::semi_join(near_dup_keys, by = "dedup_key") |>
    dplyr::group_by(.data$dedup_key, .data$trap_date) |>
    dplyr::summarise(total_sum = sum(.data$total, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(is_wknd = lubridate::wday(.data$trap_date) %in% c(1L, 7L)) |>
    dplyr::group_by(.data$dedup_key) |>
    dplyr::arrange(
      dplyr::desc(.data$total_sum),
      .data$is_wknd,
      dplyr::desc(.data$trap_date),
      .by_group = TRUE
    ) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(.data$dedup_key, .data$trap_date)

  n_before <- nrow(df)

  df_single <- dplyr::anti_join(df, near_dup_keys, by = "dedup_key")
  df_best   <- dplyr::semi_join(df, best_dates,    by = c("dedup_key", "trap_date"))
  df_out    <- dplyr::bind_rows(df_single, df_best) |>
    dplyr::select(-.data$dedup_key)

  n_removed <- n_before - nrow(df_out)

  if (verbose) {
    cli::cli_alert_info(
      "culex_dedup: {n_near_dups} trap-week{?s} had multiple collection dates including a weekend. Removed {n_removed} row{?s}; {nrow(df_out)} row{?s} remaining."
    )
  }

  df_out
}
