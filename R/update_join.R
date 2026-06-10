#' Update one table with values from another via a join
#'
#' Full-joins `new` and `old` on `by`, then coalesces every shared non-key
#' column so that `new` values win where both sides have data and `old`
#' values fill in everywhere else. Generic, composable join for "update table
#' x with the latest data from table y" — e.g. merging this week's trap counts
#' into a historical trap database, keyed on a stable composite key.
#'
#' @param new Data frame whose values take precedence on conflict.
#' @param old Data frame supplying historical/fallback values and any rows not
#'   present in `new`.
#' @param by Character vector of column names to join on.
#' @param col_keep Character vector of output column names/order. Default
#'   `names(old)`.
#' @param arrange_desc Optional column name to sort the result by, descending.
#'   Default `NULL` (no sorting).
#'
#' @return A data frame with one row per distinct `by` combination present in
#'   either input, `new` values preferred on conflict, columns ordered as
#'   `col_keep`.
#'
#' @details
#' `dplyr::coalesce()` errors when combining a factor column with a character
#' column, which happens when one side has been through `wnv_s_clean()`
#' (factors) and the other hasn't. Factor columns in both `new` and `old` are
#' coerced to character before joining; `wnv_s_clean()` re-factors downstream
#' as needed. `dplyr::full_join()` (unlike `rquery::natural_join()`) preserves
#' factor levels and `Date` classes, so no post-join type recovery is needed.
#'
#' @examples
#' \dontrun{
#' update_join(culex_new, culex_database, by = "key",
#'             col_keep = names(culex_database), arrange_desc = "trap_date")
#' }
#'
#' @importFrom dplyr full_join coalesce mutate select all_of arrange across where
#' @importFrom purrr reduce
#' @importFrom rlang .data
#' @export
update_join <- function(new, old, by, col_keep = names(old), arrange_desc = NULL) {

  to_character <- function(df) dplyr::mutate(df, dplyr::across(dplyr::where(is.factor), as.character))
  new <- to_character(new)
  old <- to_character(old)

  # Shared non-key columns gain .new/.old suffixes after the join and need
  # coalescing into a single column (new wins, old is the fallback).
  shared_cols <- setdiff(intersect(names(new), names(old)), by)

  joined <- dplyr::full_join(new, old, by = by, suffix = c(".new", ".old"))

  out <- purrr::reduce(
    shared_cols,
    function(df, col) {
      dplyr::mutate(
        df,
        !!col := dplyr::coalesce(.data[[paste0(col, ".new")]],
                                 .data[[paste0(col, ".old")]])
      )
    },
    .init = joined
  ) |>
    dplyr::select(dplyr::all_of(col_keep))

  if (!is.null(arrange_desc) && arrange_desc %in% names(out)) {
    out <- dplyr::arrange(out, dplyr::desc(.data[[arrange_desc]]))
  }

  out
}
