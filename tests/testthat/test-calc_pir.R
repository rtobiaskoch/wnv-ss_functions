# Ported from wnv-ss-wkly_report/tests/testthat/test-calc_pir.R
# Regression suite for calc_pir().

pools_neg <- tibble::tribble(
  ~csu_id,    ~trap_id,  ~zone, ~zone2, ~method, ~spp,        ~year, ~week, ~total, ~test_code,
  "CSU00001", "FC-001",  "NW",  "FC",   "L",     "Tarsalis",  2025,  35,    10,     0,
  "CSU00002", "FC-001",  "NW",  "FC",   "L",     "Tarsalis",  2025,  35,    12,     0,
  "CSU00003", "FC-001",  "NW",  "FC",   "L",     "Tarsalis",  2025,  35,    8,      0
)

pools_pos <- tibble::tribble(
  ~csu_id,    ~trap_id,  ~zone, ~zone2, ~method, ~spp,        ~year, ~week, ~total, ~test_code,
  "CSU00001", "FC-001",  "NW",  "FC",   "L",     "Tarsalis",  2025,  35,    15,     1,
  "CSU00002", "FC-001",  "NW",  "FC",   "L",     "Tarsalis",  2025,  35,    10,     0,
  "CSU00003", "FC-001",  "NW",  "FC",   "L",     "Tarsalis",  2025,  35,    8,      0
)

test_that("all-negative pools produce PIR = 0 and LCI = 0", {
  result <- calc_pir(pools_neg, zone_complete = "NW")
  nw_tar <- dplyr::filter(result, zone == "NW", spp == "Tarsalis")
  expect_equal(nw_tar$pir, 0)
  expect_equal(nw_tar$pir_lci, 0)
  expect_gte(nw_tar$pir_uci, 0)
})

test_that("one positive pool produces PIR > 0", {
  result <- calc_pir(pools_pos, zone_complete = "NW")
  nw_tar <- dplyr::filter(result, zone == "NW", spp == "Tarsalis")
  expect_gt(nw_tar$pir, 0)
})

test_that("csu_id in grp_var raises an error", {
  expect_error(
    calc_pir(pools_neg, grp_var = c("csu_id", "zone", "year", "week", "spp")),
    regexp = "Cannot calculate"
  )
})

test_that("missing required column raises an error", {
  bad_data <- dplyr::select(pools_neg, -total)
  expect_error(calc_pir(bad_data))
})
