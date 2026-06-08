# test-std_curve.R
# Tests for the qPCR standard-curve fallback (parse_std_copies, fit_std_curve,
# predict_copies).
#
# Scientific intent:
#   - Absolute qPCR quantification fits Cq = m*log10(N0) + b on standard wells of
#     known starting copies, then inverts to N0 = 10^((Cq - b)/m).
#   - The known copies live in the platemap label (e.g. "wnv_std_1e6" -> 1e6), so
#     copies can be recovered even when QuantStudio's Quantity export is empty.
#   - Amplification efficiency E = 10^(-1/m) - 1; ideal slope m ~ -3.32 (E = 100%).

# ---- parse_std_copies -------------------------------------------------------

test_that("parse_std_copies extracts the known copy number from std labels", {
  expect_equal(parse_std_copies("wnv_std_1e6"), 1e6)
  expect_equal(parse_std_copies("slev_std_1e4"), 1e4)
  expect_equal(parse_std_copies("std 1e2"), 1e2)
  expect_equal(parse_std_copies("1e6"), 1e6)
})

test_that("parse_std_copies is case-insensitive and vectorized", {
  expect_equal(
    parse_std_copies(c("WNV_STD_1E6", "std 1e2")),
    c(1e6, 1e2)
  )
})

test_that("parse_std_copies returns NA for non-standard labels", {
  expect_true(is.na(parse_std_copies("mozzy")))
  expect_equal(
    parse_std_copies(c("neg ctrl", "pos ctrl", "wnv_std_1e4")),
    c(NA, NA, 1e4)
  )
})

# ---- fit_std_curve ----------------------------------------------------------

test_that("fit_std_curve recovers the slope/intercept of a perfect line", {
  m <- -3.32; b <- 40
  log10_copies <- c(2, 4, 6)
  cq <- b + m * log10_copies            # perfect synthetic standard curve

  fit <- fit_std_curve(cq, log10_copies)

  expect_equal(fit$slope, m, tolerance = 1e-8)
  expect_equal(fit$intercept, b, tolerance = 1e-8)
  expect_equal(fit$r2, 1, tolerance = 1e-8)
  expect_equal(fit$n_points, 3L)
})

test_that("fit_std_curve computes efficiency as 10^(-1/slope) - 1", {
  m <- -3.32; b <- 40
  log10_copies <- c(2, 4, 6)
  cq <- b + m * log10_copies

  fit <- fit_std_curve(cq, log10_copies)
  expect_equal(fit$efficiency, 10^(-1 / m) - 1, tolerance = 1e-8)
})

test_that("fit_std_curve errors when fewer than 2 distinct copy levels", {
  expect_error(fit_std_curve(cq = c(20, 21), log10_copies = c(6, 6)))
  expect_error(fit_std_curve(cq = 20, log10_copies = 6))
})

# ---- predict_copies ---------------------------------------------------------

test_that("predict_copies inverts the standard curve", {
  m <- -3.32; b <- 40
  expect_equal(predict_copies(b + m * 6, slope = m, intercept = b), 1e6,
               tolerance = 1e-6)
  expect_equal(
    predict_copies(b + m * c(2, 4, 6), slope = m, intercept = b),
    c(1e2, 1e4, 1e6),
    tolerance = 1e-6
  )
})

test_that("fit then predict round-trips the standards' known copies", {
  m <- -3.20; b <- 38.5
  log10_copies <- c(2, 4, 6)
  cq <- b + m * log10_copies

  fit <- fit_std_curve(cq, log10_copies)
  recovered <- predict_copies(cq, fit$slope, fit$intercept)

  expect_equal(recovered, 10^log10_copies, tolerance = 1e-4)
})
