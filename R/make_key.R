#' Build a composite key column from multiple columns
#'
#' Concatenates the values of `key_cols` (after stripping non-alphanumeric
#' characters from each value) into a single string column joined by `sep`.
#' Originals are untouched — only the key is added. Used to create stable
#' join keys across heterogeneous data sources where small whitespace or
#' punctuation differences would otherwise prevent matching.
#'
#' @param df A data frame.
#' @param key_cols Character vector of column names to concatenate. At least
#'   two names must be supplied.
#' @param name Name of the new key column. Default `"key"`.
#' @param sep Separator string inserted between values. Default `"|"`.
#'   Non-alphanumeric separators are preserved verbatim (only the values are
#'   sanitised, not the separator).
#'
#' @return The input data frame with one additional column (named `name`)
#'   placed first.
#'
#' @examples
#' \dontrun{
#' make_key(culex, c("trap_id", "spp", "year", "week"))
#' }
#'
#' @importFrom dplyr mutate select everything
#' @importFrom rlang sym
#' @importFrom purrr map
#' @export
make_key <- function(df,
                     key_cols,
                     name = "key",
                     sep = "|") {
  if (missing(key_cols) || length(key_cols) < 2) {
    stop("Please provide at least two column names for the key.")
  }

  missing_cols <- setdiff(key_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(
      "The following key columns are missing from the data frame: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  cleaned_parts <- purrr::map(
    key_cols,
    ~ gsub("[^[:alnum:]]", "", as.character(df[[.x]]))
  )

  dplyr::mutate(
    df,
    !!rlang::sym(name) := do.call(paste, c(cleaned_parts, sep = sep))
  ) |>
    dplyr::select(!!rlang::sym(name), dplyr::everything())
}
