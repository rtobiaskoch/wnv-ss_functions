# Tests for make_key() — the composite-key builder both the weekly report and
# the trap-history combiner depend on for joining trap data across sources.
# The exact recipe (sanitise each value, join with "|") MUST stay stable, or
# keys built in one repo won't match keys built in the other.

test_that("builds trap_id|spp|year|week and strips non-alphanumerics from values", {
  df <- data.frame(
    trap_id = "FC-088GR", spp = "Pipiens", year = 2026, week = 23,
    stringsAsFactors = FALSE
  )

  out <- make_key(df, c("trap_id", "spp", "year", "week"))

  # dash in "FC-088GR" stripped; "|" separator preserved verbatim
  expect_equal(out$key, "FC088GR|Pipiens|2026|23")
})

test_that("whitespace and punctuation in values are sanitised, separator kept", {
  df <- data.frame(
    trap_id = "LV 095", spp = "Cx. tarsalis", year = 2025, week = 7,
    stringsAsFactors = FALSE
  )

  out <- make_key(df, c("trap_id", "spp", "year", "week"))

  expect_equal(out$key, "LV095|Cxtarsalis|2025|7")
})

test_that("key column is placed first and originals are unchanged", {
  df <- data.frame(
    trap_id = c("a", "b"), spp = c("Pipiens", "Tarsalis"),
    year = c(2025, 2025), week = c(30, 30), total = c(5, 9),
    stringsAsFactors = FALSE
  )

  out <- make_key(df, c("trap_id", "spp", "year", "week"))

  expect_equal(names(out)[1], "key")               # key first
  expect_equal(out$trap_id, df$trap_id)            # originals untouched
  expect_equal(out$total, df$total)
  expect_equal(nrow(out), nrow(df))                # no rows added/dropped
})

test_that("custom name and separator are honoured", {
  df <- data.frame(trap_id = "FC-001", spp = "Pipiens", stringsAsFactors = FALSE)

  out <- make_key(df, c("trap_id", "spp"), name = "dedup_key", sep = "_")

  expect_true("dedup_key" %in% names(out))
  expect_equal(out$dedup_key, "FC001_Pipiens")
})

test_that("errors when fewer than two key columns are supplied", {
  df <- data.frame(trap_id = "FC-001", stringsAsFactors = FALSE)
  expect_error(make_key(df, "trap_id"), "at least two")
})

test_that("errors when a key column is missing from the data", {
  df <- data.frame(trap_id = "FC-001", spp = "Pipiens", stringsAsFactors = FALSE)
  expect_error(
    make_key(df, c("trap_id", "year")),
    "missing from the data frame"
  )
})
