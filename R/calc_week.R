#' First Monday on or after June 1 of a given year
#'
#' The ecological anchor for the surveillance season: the start of the first
#' full week of June, defined as reported (seasonal) week 23. Kept internal —
#' callers use [calc_season_week()].
#'
#' @param year Integer vector of years.
#'
#' @return A `Date` vector (the first Monday of June for each year).
#'
#' @importFrom lubridate wday
#' @keywords internal
#' @noRd
first_monday_of_june <- function(year) {
  jun1 <- as.Date(paste0(year, "-06-01"))           # June 1 of each year
  # lubridate::wday(): Sunday = 1 ... Saturday = 7. Advance to the next Monday.
  jun1 + ((9 - lubridate::wday(jun1)) %% 7)
}


#' Seasonal (reported) week
#'
#' Converts a date to the surveillance "seasonal" week, where the first full
#' week of June is **always** week 23, every year. In normal years this equals
#' the MMWR/CDC epiweek, but a 53-week year (e.g. 2025) shifts the following
#' year's epiweeks back by one, so `epiweek(first-week-June 2026) = 22`.
#' `calc_season_week()` patches that shift so the reported week stays comparable
#' year over year.
#'
#' The patch is self-correcting: it anchors week 23 to the first Monday of June
#' of each date's year rather than hardcoding which years are affected, so
#' future leap-week years are handled automatically.
#'
#' @param date A `Date` (or coercible) vector of trap dates.
#'
#' @return A numeric vector of seasonal weeks.
#'
#' @examples
#' \dontrun{
#' calc_season_week(as.Date(c("2025-06-02", "2026-06-01")))  # both -> 23
#' }
#'
#' @importFrom lubridate epiweek year
#' @export
calc_season_week <- function(date) {
  date <- as.Date(date)
  # Anchor epiweek = the epiweek of the first Monday of June for each date's
  # year. In normal years this is 23; in years following a 53-week year it is 22.
  anchor_epiweek <- lubridate::epiweek(first_monday_of_june(lubridate::year(date)))
  # Offset the raw epiweek so the anchor week always lands on 23.
  23 + (lubridate::epiweek(date) - anchor_epiweek)
}


#' Add seasonal `week` and raw `epiweek` columns from a date column
#'
#' Pure: returns a new data frame; does not modify its input. `week` is the
#' seasonal/reported week (see [calc_season_week()]); `epiweek` is the raw
#' MMWR/CDC week (Sunday-start), kept for external submission (e.g. VectorSurv).
#'
#' @param df A data frame containing a date column.
#' @param date_col Name of the date column. Default `"trap_date"`.
#'
#' @return `df` with `week` (seasonal) and `epiweek` (raw MMWR) columns added.
#'
#' @examples
#' \dontrun{
#' add_week_cols(culex, date_col = "trap_date")
#' }
#'
#' @importFrom dplyr mutate
#' @importFrom lubridate epiweek
#' @export
add_week_cols <- function(df, date_col = "trap_date") {
  dates <- as.Date(df[[date_col]])   # sourced from the named date column in df
  dplyr::mutate(
    df,
    week    = calc_season_week(dates),
    epiweek = lubridate::epiweek(dates)
  )
}
