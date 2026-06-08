#' Parse the known copy number from a qPCR standard label
#'
#' Standards are labelled by their starting template concentration in scientific
#' shorthand (`1e2`, `1e4`, `1e6`). This pulls that number out of a free-form
#' label so the *known* copies can be recovered from the platemap alone — i.e.
#' independently of whether QuantStudio's `Quantity` export was populated.
#'
#' @param label Character vector of sample labels, e.g. `"wnv_std_1e6"`,
#'   `"std 1e2"`, `"1e4"`. Matching is case-insensitive.
#'
#' @return A numeric vector of starting copy numbers; `NA_real_` for labels that
#'   carry no `1e<digits>` token (non-standards such as `"mozzy"`, `"neg ctrl"`).
#'
#' @examples
#' parse_std_copies(c("wnv_std_1e6", "std 1e2", "mozzy"))  # 1e6, 1e2, NA
#'
#' @importFrom stringr str_extract regex
#' @export
parse_std_copies <- function(label) {
  # Grab the "1e<digits>" token (e.g. "1e6") then coerce to a number. Labels
  # without such a token (controls, mosquito pools) become NA.
  token <- stringr::str_extract(label, stringr::regex("1e\\d+", ignore_case = TRUE))
  as.numeric(token)
}


#' Fit a qPCR standard curve
#'
#' Ordinary-least-squares fit of `Cq = slope * log10(copies) + intercept` across
#' the standard wells. This is the absolute-quantification relationship qPCR
#' instruments use internally; refitting it in code lets us recover copy numbers
#' when the instrument export lacks them.
#'
#' The caller is responsible for supplying clean standard points — real (not
#' no-amp) Cq values, with each standard matched to its own target. This function
#' is deliberately dumb: it fits whatever points it is given.
#'
#' @param cq Numeric vector of standard-well Cq values.
#' @param log10_copies Numeric vector of `log10(known copies)`, same length as
#'   `cq`.
#'
#' @return A one-row data frame with columns `slope`, `intercept`, `r2`,
#'   `efficiency` (`10^(-1/slope) - 1`), and `n_points`.
#'
#' @details A standard curve cannot be fit through a single template
#'   concentration, so fewer than two **distinct** `log10_copies` values is an
#'   error rather than a degenerate fit. Curve quality (`r2`, `efficiency`) is
#'   returned for the caller to judge — this function does not gate on it.
#'
#' @examples
#' log10_copies <- c(2, 4, 6)
#' cq <- 40 - 3.32 * log10_copies
#' fit_std_curve(cq, log10_copies)
#'
#' @export
fit_std_curve <- function(cq, log10_copies) {
  if (length(unique(log10_copies)) < 2) {
    stop("fit_std_curve() needs at least 2 distinct `log10_copies` values to ",
         "fit a line; got ", length(unique(log10_copies)), ".", call. = FALSE)
  }

  fit <- stats::lm(cq ~ log10_copies)        # Cq = slope*log10(copies) + intercept
  coefs <- stats::coef(fit)
  slope <- unname(coefs[["log10_copies"]])
  intercept <- unname(coefs[["(Intercept)"]])

  # R^2 from residuals directly (avoids summary.lm()'s "perfect fit" warning on
  # near-noiseless synthetic curves; identical to summary()$r.squared otherwise).
  ss_res <- sum(stats::residuals(fit)^2)
  ss_tot <- sum((cq - mean(cq))^2)
  r2 <- if (ss_tot == 0) NA_real_ else 1 - ss_res / ss_tot

  data.frame(
    slope      = slope,
    intercept  = intercept,
    r2         = r2,
    efficiency = 10^(-1 / slope) - 1,        # 100% efficiency <-> slope -3.32
    n_points   = length(cq)
  )
}


#' Predict copy number from Cq and a fitted standard curve
#'
#' Inverts the standard curve: `copies = 10^((Cq - intercept) / slope)`. Pure,
#' vectorized math. The no-amplification rule (`Cq == 55.55 -> 0 copies`) is a
#' domain convention and is intentionally left to the caller, so this stays a
#' faithful inverse of [fit_std_curve()].
#'
#' @param cq Numeric vector of Cq values.
#' @param slope,intercept Scalars from [fit_std_curve()].
#'
#' @return A numeric vector of predicted starting copies.
#'
#' @examples
#' predict_copies(40 - 3.32 * 6, slope = -3.32, intercept = 40)  # ~1e6
#'
#' @export
predict_copies <- function(cq, slope, intercept) {
  10^((cq - intercept) / slope)
}
