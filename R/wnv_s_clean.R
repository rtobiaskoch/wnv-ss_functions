#' Report the outcome of a per-column cleaning step
#'
#' Helper called from inside [wnv_s_clean()] after each column transformation.
#' Classifies the column's transition from `df0` -> `df` into one of:
#'   - **added** (column did not exist in input),
#'   - **no-op** (existed, nothing changed),
#'   - **transformed** (values changed, no new NA introduced),
#'   - **cleaned (warning)** (values changed AND new NA introduced).
#'
#' @param df0 The input data frame, before the cleaning step.
#' @param df  The output data frame, after the cleaning step.
#' @param col_name Unquoted column name to summarise.
#' @param label Optional display label; defaults to the deparsed `col_name`.
#' @param verbose Logical. If `FALSE`, suppress the cli alert entirely.
#'
#' @return Invisibly `NULL`. Called for the side effect of a cli alert.
#'
#' @importFrom dplyr pull
#' @importFrom rlang enquo as_name
#' @keywords internal
clean_summary <- function(df0,
                          df,
                          col_name,
                          label = deparse(substitute(col_name)),
                          verbose = TRUE) {
  if (!isTRUE(verbose)) return(invisible(NULL))

  col        <- rlang::enquo(col_name)
  col_string <- rlang::as_name(col)
  n_rows     <- nrow(df)

  if (!col_string %in% names(df0)) {
    cli::cli_alert_info("{.field {label}} added ({n_rows} rows)")
    return(invisible(NULL))
  }

  old_vals <- as.character(dplyr::pull(df0, !!col))
  new_vals <- as.character(dplyr::pull(df,  !!col))

  changed       <- sum(old_vals != new_vals, na.rm = TRUE)
  na_in         <- sum(is.na(old_vals))
  na_out        <- sum(is.na(new_vals))
  na_introduced <- max(0L, na_out - na_in)

  if (changed == 0 && na_introduced == 0) {
    cli::cli_alert("{.field {label}} no-op")
    return(invisible(NULL))
  }

  if (na_introduced > 0) {
    cli::cli_alert_warning(
      "{.field {label}} cleaned ({changed} changed, {na_introduced} new NA)"
    )
    return(invisible(NULL))
  }

  cli::cli_alert_success(
    "{.field {label}} transformed ({changed}/{n_rows} rows changed)"
  )
  invisible(NULL)
}

#' Clean a Culex surveillance data sheet
#'
#' Standardises raw Culex mosquito surveillance data: trims whitespace, parses
#' collection dates with [parse_flexible_date()], normalises zone and `csu_id`
#' values, derives `year`/`week` from `trap_date`, canonicalises trap IDs and
#' species labels, and computes a per-trap-night `trap_status` (`culex` /
#' `no culex` / `no mosquitoes` / `malfunction` / `no trap`). Each step runs
#' only if its inputs are present, so the function works on partial frames.
#'
#' Must be paired with [key_rename()] beforehand if the input column names do
#' not yet match the canonical schema (`trap_id`, `trap_date`, `spp`, `total`,
#' …). Run [culex_dedup()] afterward to resolve within-week duplicates.
#'
#' @param df A data frame with raw Culex surveillance data. Expected columns
#'   are a subset of `all_cols`.
#' @param all_cols Character vector of canonical column names that this
#'   function knows how to clean and that it will pull to the front of the
#'   output. Defaults match the wnv-s_database schema.
#' @param rm_col Character vector of column names to *skip* cleaning for
#'   (still kept in the data, just not transformed).
#' @param verbose Logical. If `TRUE` (default), per-step cli alerts are
#'   printed. Set `FALSE` in pipelines / tests for silent operation.
#'
#' @return A cleaned data frame, sorted by `trap_date` (desc) and `trap_id`
#'   when those columns are present.
#'
#' @examples
#' \dontrun{
#' raw_culex |>
#'   key_rename(rename_map) |>
#'   wnv_s_clean(verbose = FALSE) |>
#'   culex_dedup()
#' }
#'
#' @importFrom dplyr mutate across transmute case_when if_else coalesce group_by ungroup select arrange any_of everything
#' @importFrom stringr str_detect str_remove str_remove_all str_extract str_c
#' @importFrom purrr map_chr
#' @importFrom lubridate year isoweek
#' @importFrom rlang .data
#' @export
wnv_s_clean <- function(df,
                        all_cols = c(
                          "csu_id", "trap_id", "zone", "zone2",
                          "trap_date", "year", "week",
                          "spp", "spp0", "method", "trap_status", "total"
                        ),
                        rm_col = c(),
                        verbose = TRUE) {

  df0 <- df

  col_2_clean  <- setdiff(all_cols, rm_col)
  present_cols <- intersect(all_cols, names(df))
  missing_cols <- setdiff(all_cols, names(df))

  if (verbose) {
    if (length(missing_cols) > 0) {
      cli::cli_alert_warning(
        "Not present for cleaning: {.field {missing_cols}}"
      )
      cli::cli_alert_info(
        "Run {.fn key_rename} to convert columns to the standard naming convention."
      )
    }
    if (length(present_cols) > 0) {
      cli::cli_alert_info(
        "Cleaning columns: {.field {present_cols}}"
      )
    }
  }

  # Trim whitespace from all character columns
  df <- dplyr::mutate(df, dplyr::across(dplyr::where(is.character), trimws))

  # CLEAN csu_id
  if ("csu_id" %in% names(df) && "csu_id" %in% col_2_clean) {
    df <- dplyr::mutate(df, csu_id = stringr::str_remove(.data$csu_id, "-"))
    clean_summary(df0, df, csu_id, verbose = verbose)
  }

  # CLEAN ZONE — uses the canonical wnvSurv::zone_lvls minus the "FC" composite
  # (which is derived in the next block, not a literal zone code in source data).
  if ("zone" %in% names(df) && "zone2" %in% col_2_clean) {
    valid_zones  <- setdiff(wnvSurv::zone_lvls, "FC")
    zone_pattern <- stringr::str_c(valid_zones, collapse = "|")

    df <- dplyr::mutate(
      df,
      zone = dplyr::if_else(stringr::str_detect(.data$zone, "Berthoud"), "BE", .data$zone),
      zone = stringr::str_extract(.data$zone, zone_pattern)
    )
    clean_summary(df0, df, zone, verbose = verbose)
  }

  # DERIVE ZONE2 from zone (FC composite for the four Fort Collins quadrants)
  if ("zone" %in% names(df) && "zone2" %in% col_2_clean) {
    df <- dplyr::mutate(
      df,
      zone2 = dplyr::if_else(.data$zone %in% wnvSurv::fc_zones, "FC", .data$zone)
    )
    clean_summary(df0, df, zone2, verbose = verbose)
  }

  # FALLBACK: derive zone2 (and zone) from trap_id prefix when zone is absent.
  #   FC-*  -> FC ; LV-* -> LV ; (BE|LC|WC)-* -> BE ; BC-* -> BC
  if (!"zone2" %in% names(df) &&
      "zone2" %in% col_2_clean &&
      "trap_id" %in% names(df)) {

    df <- dplyr::mutate(
      df,
      zone2 = dplyr::case_when(
        stringr::str_detect(.data$trap_id, "^(?i)FC")          ~ "FC",
        stringr::str_detect(.data$trap_id, "^(?i)LV")          ~ "LV",
        stringr::str_detect(.data$trap_id, "^(?i)(BE|LC|WC)")  ~ "BE",
        stringr::str_detect(.data$trap_id, "^(?i)BC")          ~ "BC",
        TRUE                                                   ~ NA_character_
      )
    )
    if (!"zone" %in% names(df)) {
      df <- dplyr::mutate(df, zone = .data$zone2)
    }
    clean_summary(df0, df, zone2, verbose = verbose)
  }

  # CLEAN DATE
  if ("trap_date" %in% names(df) && "trap_date" %in% col_2_clean) {
    df <- dplyr::mutate(
      df,
      trap_date = purrr::map_chr(
        .data$trap_date,
        ~ as.character(parse_flexible_date(.x))
      ),
      trap_date = as.Date(.data$trap_date)
    )
    clean_summary(df0, df, trap_date, verbose = verbose)
  }

  # DERIVE YEAR/WEEK from trap_date (filling NAs if year/week already present)
  if ("trap_date" %in% names(df) && "year" %in% col_2_clean) {
    has_year <- "year" %in% names(df)
    has_week <- "week" %in% names(df)

    df <- dplyr::mutate(
      df,
      year = if (has_year) {
        dplyr::coalesce(as.integer(.data$year), lubridate::year(.data$trap_date))
      } else {
        lubridate::year(.data$trap_date)
      },
      week = if (has_week) {
        dplyr::coalesce(as.integer(.data$week), lubridate::isoweek(.data$trap_date))
      } else {
        lubridate::isoweek(.data$trap_date)
      }
    )
    clean_summary(df0, df, year, verbose = verbose)
    clean_summary(df0, df, week, verbose = verbose)
  }

  # CLEAN TRAP_ID — strip whitespace and uppercase
  if ("trap_id" %in% names(df) && "trap_id" %in% col_2_clean) {
    df <- dplyr::mutate(
      df,
      trap_id = toupper(stringr::str_remove_all(.data$trap_id, "\\s+"))
    )
    clean_summary(df0, df, trap_id, verbose = verbose)
  }

  # DERIVE METHOD from trap_id (G = gravid, L = light)
  if ("trap_id" %in% names(df) && "trap_id" %in% col_2_clean) {
    df <- dplyr::mutate(
      df,
      method = dplyr::case_when(
        stringr::str_detect(tolower(.data$trap_id), "gr") ~ "G",
        TRUE                                              ~ "L"
      )
    )
    clean_summary(df0, df, method, verbose = verbose)
  }

  # SAVE raw spp as spp0 (needed by trap_status logic even when spp itself is rm_col)
  if ("spp" %in% names(df)) {
    df <- dplyr::mutate(df, spp0 = .data$spp)
    clean_summary(df0, df, spp0, verbose = verbose)
  }

  # DERIVE TRAP_STATUS from spp0 (per trap_id × trap_date)
  if ("spp" %in% names(df) && "trap_status" %in% col_2_clean) {
    if (!"trap_status" %in% names(df)) {
      df$trap_status <- NA_character_
    }

    df <- df |>
      dplyr::group_by(.data$trap_id, .data$trap_date) |>
      dplyr::mutate(
        trap_status = dplyr::case_when(
          any(.data$trap_status %in% c("No Traps", "no trap"), na.rm = TRUE) ~ "no trap",
          any(.data$trap_status == "malfunction", na.rm = TRUE) ~ "malfunction",
          any(stringr::str_detect(.data$spp0, "(?i)malfunction|stolen|vandalized"), na.rm = TRUE) ~ "malfunction",
          any(stringr::str_detect(.data$spp0, "(?i)no mosquitoes"), na.rm = TRUE) ~ "no mosquitoes",
          any(stringr::str_detect(.data$spp0, "(?i)tarsalis|pipiens") & .data$total > 0, na.rm = TRUE) ~ "culex",
          any(stringr::str_detect(.data$spp0, "(?i)tarsalis|pipiens"), na.rm = TRUE) ~ "no mosquitoes",
          TRUE ~ "no culex"
        )
      ) |>
      dplyr::ungroup()

    # Malfunction traps did not collect — nullify total so they're excluded from abundance
    df <- dplyr::mutate(
      df,
      total = dplyr::if_else(
        .data$trap_status == "malfunction",
        NA_real_,
        as.numeric(.data$total)
      )
    )
    clean_summary(df0, df, trap_status, verbose = verbose)
  }

  # CLEAN SPP — standardise to Tarsalis / Pipiens / none / non culex
  if ("spp" %in% names(df) && "spp" %in% col_2_clean) {
    df <- dplyr::mutate(
      df,
      spp = dplyr::case_when(
        stringr::str_detect(.data$spp, "(?i)Tarsalis") ~ "Tarsalis",
        stringr::str_detect(.data$spp, "(?i)Pipiens")  ~ "Pipiens",
        stringr::str_detect(.data$spp, "(?i)malfunction|stolen|no mosquitoes") ~ "none",
        TRUE                                          ~ "non culex"
      )
    )
    clean_summary(df0, df, spp, verbose = verbose)
  }

  # Coerce total to numeric
  if ("total" %in% names(df) && "total" %in% col_2_clean) {
    df <- dplyr::mutate(df, total = as.numeric(.data$total))
    clean_summary(df0, df, total, verbose = verbose)
  }

  if ("trap_date" %in% names(df) && "trap_id" %in% names(df)) {
    df <- df |>
      dplyr::select(dplyr::any_of(all_cols), dplyr::everything()) |>
      dplyr::arrange(dplyr::desc(.data$trap_date), .data$trap_id)
  } else {
    df <- dplyr::select(df, dplyr::any_of(all_cols), dplyr::everything())
  }

  df
}
