#' Parse a single date string from any of several common formats
#'
#' Tries a list of date formats in order and returns the first that yields a
#' valid `Date`. ISO `YYYY-MM-DD` is checked first, then 4-digit-year formats
#' (only when the input actually contains a 4-digit sequence — avoids
#' `%Y` greedily matching 2-digit years), then 2-digit-year formats as a
#' fallback. Returns `NA` of class `Date` if no format succeeds.
#'
#' @param date_str A single character string (length 1), or a `Date`, or `NA`.
#'   Vectors longer than 1 raise an error — vectorise with `purrr::map_chr()`
#'   at the call site (this is how [wnv_s_clean()] uses it).
#'
#' @return A length-1 object of class `Date`. `NA_Date_`-equivalent if
#'   parsing fails.
#'
#' @examples
#' parse_flexible_date("2024-07-15")
#' parse_flexible_date("7/15/24")
#' parse_flexible_date("15 July 2024")
#'
#' @export
parse_flexible_date <- function(date_str) {
  na_date <- as.Date(NA_character_)

  if (length(date_str) == 0 || is.na(date_str)) return(na_date)

  if (inherits(date_str, "Date")) {
    return(as.Date(format(date_str, "%Y-%m-%d")))
  }

  if (!is.character(date_str) || length(date_str) != 1) {
    stop("`date_str` must be a single character string.")
  }

  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date_str)) {
    iso_date <- suppressWarnings(as.Date(date_str, format = "%Y-%m-%d"))
    if (!is.na(iso_date)) return(iso_date)
  }

  if (grepl("\\d{4}", date_str)) {
    formats_4digit <- c(
      "%m/%d/%Y", "%m-%d-%Y",
      "%Y/%m/%d", "%d/%m/%Y", "%d-%m-%Y",
      "%Y.%m.%d", "%m.%d.%Y",
      "%b %d %Y", "%d %b %Y",
      "%B %d %Y", "%d %B %Y"
    )
    for (fmt in formats_4digit) {
      dt <- suppressWarnings(as.Date(date_str, format = fmt))
      if (!is.na(dt)) return(dt)
    }
  }

  formats_2digit <- c("%m/%d/%y", "%d/%m/%y", "%y-%m-%d", "%y/%m/%d")
  for (fmt in formats_2digit) {
    dt <- suppressWarnings(as.Date(date_str, format = fmt))
    if (!is.na(dt)) return(dt)
  }

  na_date
}
