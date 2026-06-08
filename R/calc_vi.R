#' Calculate the Vector Index (VI = abundance × PIR)
#'
#' Joins abundance and PIR estimates on `by`, then computes
#' `vi = abund * pir` (and matching CI from `abund * pir_lci/pir_uci`). VI is
#' the standard summary metric of local WNV risk: it is the expected number
#' of infected mosquitoes caught per trap-night. Optionally completes the
#' output to a full zone × species × year × week grid.
#'
#' @param abund Abundance tibble from [calc_abund()].
#' @param pir PIR tibble from [calc_pir()].
#' @param by Character vector of join keys. Default
#'   `c("year", "week", "zone", "spp")`.
#' @param spp_cmplt Species to include when `complete = TRUE`. Default
#'   `wnvSurv::spp_levels`.
#' @param zone_complete Zones to include when `complete = TRUE`. Default
#'   `wnvSurv::zone_lvls`. Replaces the undefined `grp_zones` global in the
#'   upstream version.
#' @param complete Logical. If `TRUE`, fill the output to a full zone ×
#'   species × year × week grid via [tidyr::complete()]. Default `FALSE`
#'   (avoids the upstream `grp_zones` crash).
#' @param rm_zone Zones to drop entirely. Default `NULL`.
#'
#' @return A tibble with `vi`, `vi_lci`, `vi_uci` columns plus the join keys,
#'   sorted by `year, week, zone, spp`.
#'
#' @importFrom dplyr full_join filter mutate select arrange
#' @importFrom tidyr complete
#' @importFrom rlang .data
#' @export
calc_vi <- function(abund, pir,
                    by            = c("year", "week", "zone", "spp"),
                    spp_cmplt     = wnvSurv::spp_levels,
                    zone_complete = wnvSurv::zone_lvls,
                    complete      = FALSE,
                    rm_zone       = NULL) {

  if ("spp0" %in% names(abund)) abund <- dplyr::select(abund, -"spp0")
  if ("spp0" %in% names(pir))   pir   <- dplyr::select(pir,   -"spp0")

  vi <- dplyr::full_join(abund, pir, by = by) |>
    dplyr::filter(!.data$zone %in% rm_zone) |>
    dplyr::mutate(
      vi     = round(.data$abund * .data$pir,     2),
      vi_lci = round(.data$abund * .data$pir_lci, 2),
      vi_uci = round(.data$abund * .data$pir_uci, 2)
    )

  if (isTRUE(complete)) {
    vi <- tidyr::complete(
      vi,
      zone = setdiff(zone_complete, rm_zone),
      spp  = spp_cmplt,
      .data$year, .data$week
    )
  }

  dplyr::arrange(vi, .data$year, .data$week, .data$zone, .data$spp)
}
