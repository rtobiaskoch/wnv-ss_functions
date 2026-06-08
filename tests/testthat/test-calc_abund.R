# Ported from wnv-ss-wkly_report/tests/testthat/test-calc_abund.R
# Regression suite for calc_abund() — verifies the refactored function
# preserves the upstream behaviour: light-trap-only filter, malfunction
# exclusion, CI floor at 0.

culex_data <- tibble::tribble(
  ~trap_id,  ~zone, ~zone2, ~year, ~week, ~spp,        ~method, ~trap_status,  ~total,
  "FC-001",  "NW",  "FC",   2025,  35,    "Tarsalis",  "L",     "culex",       10,
  "FC-002",  "NW",  "FC",   2025,  35,    "Tarsalis",  "L",     "culex",       8,
  "FC-001",  "NW",  "FC",   2025,  35,    "Pipiens",   "L",     "culex",       20,
  "FC-002",  "NW",  "FC",   2025,  35,    "Pipiens",   "L",     "culex",       16,
  "FC-003",  "SE",  "FC",   2025,  35,    "Tarsalis",  "L",     "culex",       5,
  "FC-004",  "SE",  "FC",   2025,  35,    "Tarsalis",  "L",     "culex",       3,
  "FC-003",  "SE",  "FC",   2025,  35,    "Pipiens",   "L",     "culex",       15,
  "FC-004",  "SE",  "FC",   2025,  35,    "Pipiens",   "L",     "culex",       13,
  "BC-001",  "BC",  "BC",   2025,  35,    "Tarsalis",  "L",     "culex",       0,
  "BC-002",  "BC",  "BC",   2025,  35,    "Tarsalis",  "L",     "culex",       50,
  "FC-005",  "NW",  "FC",   2025,  35,    "Tarsalis",  "G",     "culex",       100,
  "FC-006",  "SE",  "FC",   2025,  35,    "Tarsalis",  "L",     "malfunction", 999
)

result <- calc_abund(culex_data)

test_that("calc_abund returns one row per zone-species group", {
  expect_equal(nrow(result), 5)
})

test_that("calc_abund computes mosq_L, trap_L, and abund correctly", {
  nw_tar <- dplyr::filter(result, zone == "NW", spp == "Tarsalis")
  expect_equal(nw_tar$trap_L, 2)
  expect_equal(nw_tar$mosq_L, 18)
  expect_equal(nw_tar$abund, 9.0)
})

test_that("calc_abund excludes non-L method traps", {
  nw_tar <- dplyr::filter(result, zone == "NW", spp == "Tarsalis")
  expect_equal(nw_tar$mosq_L, 18)
  expect_equal(nw_tar$trap_L, 2)
})

test_that("calc_abund excludes malfunction traps", {
  se_tar <- dplyr::filter(result, zone == "SE", spp == "Tarsalis")
  expect_equal(se_tar$mosq_L, 8)
  expect_equal(se_tar$trap_L, 2)
})

test_that("abund_lci is never negative", {
  expect_true(all(result$abund_lci >= 0, na.rm = TRUE))
  bc_tar <- dplyr::filter(result, zone == "BC", spp == "Tarsalis")
  expect_equal(bc_tar$abund_lci, 0)
})
