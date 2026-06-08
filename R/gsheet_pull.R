#' Download a Google Sheet to a local CSV (non-interactive core)
#'
#' Authenticates against Google Drive if needed, reads the specified Google
#' Sheet, and writes it to a local CSV. Pure with respect to interactivity:
#' contains **no `readline()` calls**, so it is safe to use inside `{targets}`
#' pipelines, Quarto renders, and CI jobs. For an interactive
#' "do-you-want-to-overwrite?" prompt, use [gsheet_pull_prompt()] which wraps
#' this function.
#'
#' Behaviour:
#'   * If `filename` does not exist, it is downloaded.
#'   * If `filename` exists and `force = FALSE` (default), the download is
#'     **skipped** (no overwrite).
#'   * If `filename` exists and `force = TRUE`, it is re-downloaded and
#'     overwritten.
#'
#' List-columns produced by `googlesheets4::read_sheet()` (e.g. cells that
#' contain mixed types) are flattened to comma-joined character strings
#' before writing.
#'
#' @param filename Path to the local CSV to (over)write. Must end in `.csv`.
#' @param key The Google Sheet ID (the long string from the sheet's URL),
#'   passed to `googlesheets4::read_sheet()`.
#' @param sheet Name of the worksheet/tab within the spreadsheet. Default
#'   `"Sheet1"`.
#' @param force Logical. If `TRUE`, overwrite `filename` even if it already
#'   exists. Default `FALSE`.
#' @param verbose Logical. If `TRUE` (default), emit cli alerts on each step.
#'
#' @return Invisibly `TRUE`. Called for the side effect of writing
#'   `filename`.
#'
#' @seealso [gsheet_pull_prompt()] for the interactive variant.
#'
#' @examples
#' \dontrun{
#' gsheet_pull(
#'   filename = "1_input/foco_trap - data.csv",
#'   key      = "1AbCdEf...",   # sheet ID
#'   sheet    = "data"
#' )
#' }
#'
#' @importFrom dplyr mutate across
#' @importFrom rlang .data
#' @export
gsheet_pull <- function(filename,
                        key,
                        sheet   = "Sheet1",
                        force   = FALSE,
                        verbose = TRUE) {

  if (!grepl("\\.csv$", filename)) {
    stop("`filename` must end in .csv")
  }

  exists_already <- file.exists(filename)
  should_download <- !exists_already || isTRUE(force)

  if (!should_download) {
    if (verbose) {
      cli::cli_alert_info("{.file {filename}} exists and {.code force = FALSE}; skipping download.")
    }
    return(invisible(TRUE))
  }

  if (!googledrive::drive_has_token()) {
    googledrive::drive_auth()
  }

  mdata <- googlesheets4::read_sheet(key, sheet = sheet)

  if (verbose) {
    cli::cli_alert_success(
      "Read Google Sheet: {nrow(mdata)} row{?s} x {ncol(mdata)} column{?s}."
    )
    cli::cli_alert_info("Headers: {.field {names(mdata)}}")
  }

  mdata <- dplyr::mutate(
    mdata,
    dplyr::across(dplyr::where(is.list), ~ sapply(., paste, collapse = ", "))
  )

  utils::write.csv(mdata, filename, row.names = FALSE, na = "")

  if (!file.exists(filename)) {
    stop(
      filename,
      " file doesn't exist after write. Please reauthenticate googledrive ",
      "(`googledrive::drive_auth()`), check inputs, and rerun."
    )
  }
  if (verbose) cli::cli_alert_success("Wrote {.file {filename}}.")

  invisible(TRUE)
}
