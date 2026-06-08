#' Clean a PCR plate map
#'
#' Parses plate metadata from `file_name` (year / week / plate number), builds
#' `well_position` from `row` + `column`, normalises `csu_id`, classifies each
#' well into a `sample_type` (mosquito sample, neg/pos control, standard
#' dilution, or bird tissue), and drops wells with no `csu_id`. Used in the
#' weekly PCR ingest pipeline.
#'
#' @param df A data frame with at least the columns `csu_id`, `file_name`,
#'   `row`, and `column`.
#' @param y_pattern Lookbehind regex for the 4-digit year embedded in
#'   `file_name`. Default matches the substring after `"y"` in e.g.
#'   `"y2025_w28_p1"`.
#' @param w_pattern Lookbehind regex for the week number.
#' @param p_pattern Lookbehind regex for the plate number.
#'
#' @return A data frame with one row per non-missing well and columns
#'   `well_position`, `csu_id`, `sample_type`, `year`, `week`, `plate`.
#'
#' @importFrom dplyr mutate filter select case_when if_else
#' @importFrom stringr str_remove str_extract
#' @importFrom rlang .data
#' @export
clean_platemap <- function(df,
                           y_pattern = "(?<=y)\\d+",
                           w_pattern = "(?<=w)\\d+",
                           p_pattern = "(?<=p)\\d+") {

  df |>
    dplyr::mutate(
      csu_id        = stringr::str_remove(.data$csu_id, "-"),
      year          = stringr::str_extract(.data$file_name, y_pattern),
      week          = stringr::str_extract(.data$file_name, w_pattern),
      plate         = stringr::str_extract(.data$file_name, p_pattern),
      plate         = dplyr::if_else(is.na(.data$plate), .data$plate, paste0("plate_", .data$plate)),
      well_position = paste0(.data$row, .data$column),
      well_position = stringr::str_remove(.data$well_position, "\\.0")
    ) |>
    dplyr::mutate(
      sample_type = dplyr::case_when(
        grepl("^CSU|^BOU|^CDC", .data$csu_id, ignore.case = TRUE) ~ "mozzy",
        grepl("neg|negative",   .data$csu_id, ignore.case = TRUE) ~ "neg ctrl",
        grepl("pos|positive",   .data$csu_id, ignore.case = TRUE) ~ "pos ctrl",
        grepl("1e2",            .data$csu_id, ignore.case = TRUE) ~ "std 1e2",
        grepl("1e4",            .data$csu_id, ignore.case = TRUE) ~ "std 1e4",
        grepl("1e6",            .data$csu_id, ignore.case = TRUE) ~ "std 1e6",
        grepl("RMRP",           .data$csu_id, ignore.case = TRUE) ~ "bird",
        TRUE                                                      ~ "undefined"
      )
    ) |>
    dplyr::filter(!is.na(.data$csu_id)) |>
    dplyr::select(
      "well_position", "csu_id", "sample_type",
      "year", "week", "plate"
    )
}
