# test-calc_week.R
# Tests for the seasonal-week patch (calc_season_week) and add_week_cols().
#
# Scientific intent:
#   - Surveillance starts the first full week of June, which is ALWAYS reported
#     as week 23, every year.
#   - `epiweek` (MMWR, Sunday-start) normally puts that week at 23, but the
#     53-week year 2025 shifts 2026 back by one, so first-June 2026 = epiweek 22.
#   - calc_season_week() patches this: it anchors week 23 to the first Monday of
#     June, so the reported week is leap-week-stable (first-June = 23 every year)
#     while a separate `epiweek` column preserves the raw MMWR week.

test_that("first full week of June is seasonal week 23 every year", {
  # First Monday of June across normal years and the 2026 leap-week year
  first_mondays <- as.Date(c("2023-06-05", "2024-06-03", "2025-06-02", "2026-06-01"))
  expect_equal(calc_season_week(first_mondays), rep(23, 4))
})

test_that("seasonal week equals epiweek in a normal (non-leap-week) year", {
  d <- as.Date(c("2025-06-02", "2025-06-09", "2025-09-08"))
  expect_equal(calc_season_week(d), c(23, 24, 37))
  # In a normal year the patch is a no-op: week == epiweek
  expect_equal(calc_season_week(d), lubridate::epiweek(d))
})

test_that("leap-week year 2026: first week of June is week 23 but epiweek 22", {
  d <- as.Date("2026-06-01")
  expect_equal(calc_season_week(d), 23)        # reported/patched week
  expect_equal(lubridate::epiweek(d), 22)      # raw MMWR week
  # The patch must NOT be a no-op in 2026
  expect_false(calc_season_week(d) == lubridate::epiweek(d))
})

test_that("season end stays aligned across normal and leap-week years (week 37)", {
  # 14 weeks after the first Monday of June = end of season in both years
  expect_equal(calc_season_week(as.Date("2025-09-08")), 37)  # epiweek 37
  expect_equal(calc_season_week(as.Date("2026-09-07")), 37)  # epiweek 36, patched +1
})

test_that("add_week_cols() adds patched `week` and raw `epiweek` from trap_date", {
  df <- tibble::tibble(
    trap_id   = c("FC-001", "FC-002"),
    trap_date = as.Date(c("2026-06-01", "2025-06-02"))
  )
  out <- add_week_cols(df, date_col = "trap_date")

  expect_true(all(c("week", "epiweek") %in% names(out)))
  expect_equal(out$week,    c(23, 23))  # both first-June weeks -> 23
  expect_equal(out$epiweek, c(22, 23))  # raw MMWR differs in 2026
  # original columns preserved, no rows added/dropped
  expect_equal(nrow(out), nrow(df))
  expect_equal(out$trap_id, df$trap_id)
})
