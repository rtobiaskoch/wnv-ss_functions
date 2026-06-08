#' Rename data frame columns via a lookup table
#'
#' Renames columns in `df` according to a two-column lookup data frame (`old`
#' → `new`). Unmatched columns are left untouched by default, or dropped if
#' `drop_extra = TRUE`. Lookup rows that reference columns not present in `df`
#' are silently ignored, so a single canonical rename map can be reused across
#' heterogeneous source files.
#'
#' @param df A data frame to rename columns of.
#' @param rename_df A data frame with two character columns: `old` (current
#'   column names in `df`) and `new` (desired names).
#' @param drop_extra Logical. If `TRUE`, columns in `df` that have no entry in
#'   `rename_df$old` are dropped from the output; if `FALSE` (the default),
#'   they are passed through with their original names.
#'
#' @return A data frame with renamed (and optionally pruned) columns.
#'
#' @details
#' Matching is **whitespace-insensitive**: both the `df` column names and the
#' lookup's `old` values are collapsed (internal runs of whitespace reduced to a
#' single space and trimmed) before comparison. This is deliberate — source
#' spreadsheets routinely carry cosmetic spacing that the lookup cannot
#' anticipate (e.g. a template header `"Collection Site       (Trap ID)"` with
#' seven spaces vs a one-space lookup entry). The **actual** column name in `df`
#' is what gets renamed, so no data is altered. If the lookup lists the same
#' column under several whitespace/alias variants, the first is used. Renaming
#' that would collapse two different columns onto one name is an error, not a
#' silent drop.
#'
#' @examples
#' \dontrun{
#' rename_map <- tibble::tribble(
#'   ~old,             ~new,
#'   "Trap ID",        "trap_id",
#'   "Collection Date", "trap_date"
#' )
#' key_rename(raw_culex, rename_map)
#' }
#'
#' @importFrom dplyr select all_of
#' @export
key_rename <- function(df, rename_df, drop_extra = FALSE) {
  if (!all(c("old", "new") %in% names(rename_df))) {
    stop("`rename_df` must contain columns: 'old' and 'new'")
  }

  # Collapse internal whitespace + trim so matches survive cosmetic spacing
  # differences between source headers and the lookup. Matching is done on the
  # squished forms; the original `df` column name is what is renamed.
  squish <- function(s) gsub("\\s+", " ", trimws(s))

  # De-duplicate the lookup by squished `old` (a map may list a column under
  # several whitespace/alias variants); first occurrence wins.
  rename_df <- rename_df[!duplicated(squish(rename_df$old)), , drop = FALSE]

  df_names_sq <- squish(names(df))
  old_sq      <- squish(rename_df$old)

  # For each df column, the desired `new` name (NA where the column is unmapped).
  target <- rename_df$new[match(df_names_sq, old_sq)]
  mapped <- !is.na(target)

  new_names <- names(df)
  new_names[mapped] <- target[mapped]

  # Guard: renaming must not collapse two columns onto one name (this is the
  # failure the old exact-match version produced silently together with
  # drop_extra). Surface it loudly instead.
  if (anyDuplicated(new_names)) {
    dup <- unique(new_names[duplicated(new_names)])
    stop("`key_rename` would create duplicate column name(s): ",
         paste(dup, collapse = ", "), call. = FALSE)
  }
  names(df) <- new_names

  if (drop_extra) {
    df <- dplyr::select(df, dplyr::all_of(unique(target[mapped])))
  }

  df
}
