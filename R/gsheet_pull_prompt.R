#' Interactive wrapper around [gsheet_pull()]
#'
#' Thin wrapper that adds an interactive overwrite prompt. If `filename`
#' already exists and the R session is interactive, asks the user whether to
#' overwrite. In non-interactive sessions the function falls back to
#' `force_update`. The actual download is delegated to [gsheet_pull()] so the
#' two functions stay in lock-step.
#'
#' @inheritParams gsheet_pull
#' @param force_update Logical. In non-interactive sessions, overwrite an
#'   existing `filename` only if this is `TRUE`. In interactive sessions,
#'   bypasses the prompt and forces the download. Default `FALSE`.
#'
#' @return Invisibly `TRUE`.
#'
#' @seealso [gsheet_pull()] for the non-interactive core (use in pipelines).
#'
#' @examples
#' \dontrun{
#' gsheet_pull_prompt(
#'   filename = "1_input/foco_trap - data.csv",
#'   key      = "1AbCdEf...",
#'   sheet    = "data"
#' )
#' }
#'
#' @export
gsheet_pull_prompt <- function(filename,
                               key,
                               sheet        = "Sheet1",
                               force_update = FALSE,
                               verbose      = TRUE) {

  if (!grepl("\\.csv$", filename)) {
    stop("`filename` must end in .csv")
  }

  if (file.exists(filename)) {
    if (interactive() && !isTRUE(force_update)) {
      user_input <- readline(prompt = paste0(
        filename,
        " exists. Would you like to replace it from Google Drive? (y/n): "
      ))
      do_force <- tolower(user_input) == "y"
    } else {
      if (verbose && !isTRUE(force_update)) {
        cli::cli_alert_info(
          "{.file {filename}} exists; running non-interactively, not overwriting (use {.code force_update = TRUE} to override)."
        )
      }
      do_force <- isTRUE(force_update)
    }
  } else {
    do_force <- TRUE   # file doesn't exist - always download
  }

  gsheet_pull(
    filename = filename,
    key      = key,
    sheet    = sheet,
    force    = do_force,
    verbose  = verbose
  )
}
