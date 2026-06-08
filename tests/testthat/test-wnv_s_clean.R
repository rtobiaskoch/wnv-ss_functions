# Ported from wnv-ss_trap_hx_combiner/tests/testthat/test-wnv_s_clean.R
# Regression suite — verifies the refactored wnv_s_clean() preserves the
# spp0 snapshot, trap_status derivation, malfunction propagation, and
# zone2 derivation behaviours of the upstream function.

make_obs <- function(spp, total,
                     trap_id     = "FC-001",
                     trap_date   = "2023-06-20",
                     zone        = "NE",
                     trap_status = NA_character_) {
  data.frame(
    trap_id     = trap_id,
    trap_date   = trap_date,
    spp         = spp,
    total       = as.integer(total),
    zone        = zone,
    trap_status = trap_status,
    stringsAsFactors = FALSE
  )
}

# All tests run with verbose = FALSE so output is clean.

# -- spp0 preservation ---------------------------------------------------------
test_that("spp0 is present in output and holds the raw species value", {
  result <- wnv_s_clean(make_obs("Culex tarsalis", 10), verbose = FALSE)
  expect_true("spp0" %in% names(result))
  expect_equal(result$spp0[1], "Culex tarsalis")
})

test_that("spp0 value is the pre-clean raw string, not the cleaned spp", {
  result <- wnv_s_clean(make_obs("Culex Tarsalis sp.", 3), verbose = FALSE)
  expect_equal(result$spp0[1], "Culex Tarsalis sp.")
  expect_equal(result$spp[1], "Tarsalis")
})

# -- trap_status labels --------------------------------------------------------
test_that("trap_status is 'culex' when raw spp contains tarsalis and total > 0", {
  result <- wnv_s_clean(make_obs("Culex tarsalis", 12), verbose = FALSE)
  expect_equal(unique(result$trap_status), "culex")
})

test_that("trap_status is 'culex' for messy raw name containing 'Tarsalis'", {
  result <- wnv_s_clean(make_obs("Culex Tarsalis sp.", 3), verbose = FALSE)
  expect_equal(unique(result$trap_status), "culex")
})

test_that("trap_status is 'no mosquitoes' when tarsalis total == 0", {
  result <- wnv_s_clean(make_obs("Culex tarsalis", 0), verbose = FALSE)
  expect_equal(unique(result$trap_status), "no mosquitoes")
})

test_that("trap_status is 'no culex' for non-culex catch", {
  result <- wnv_s_clean(make_obs("Aedes vexans", 5), verbose = FALSE)
  expect_equal(unique(result$trap_status), "no culex")
})

test_that("trap_status is 'malfunction' when raw spp contains 'Malfunction'", {
  result <- wnv_s_clean(make_obs("Malfunction", 0), verbose = FALSE)
  expect_equal(unique(result$trap_status), "malfunction")
})

# -- malfunction total ---------------------------------------------------------
test_that("total is NA for malfunction rows", {
  result <- wnv_s_clean(make_obs("Malfunction", 0), verbose = FALSE)
  expect_true(all(is.na(result$total)))
})

test_that("total is NOT NA for culex rows", {
  result <- wnv_s_clean(make_obs("Culex tarsalis", 8), verbose = FALSE)
  expect_false(any(is.na(result$total)))
})

# -- "no trap" standardisation -------------------------------------------------
test_that("pre-set 'No Traps' (legacy uppercase) is standardised to 'no trap'", {
  df <- make_obs("Tarsalis", 0, trap_status = "No Traps")
  result <- wnv_s_clean(df, verbose = FALSE)
  expect_equal(unique(result$trap_status), "no trap")
  expect_false("No Traps" %in% result$trap_status)
})

test_that("pre-set 'no trap' (lowercase) is preserved unchanged", {
  df <- make_obs("Tarsalis", 0, trap_status = "no trap")
  result <- wnv_s_clean(df, verbose = FALSE)
  expect_equal(unique(result$trap_status), "no trap")
})

# -- group-level malfunction propagation ---------------------------------------
test_that("malfunction in one row propagates to all rows in the same trap-date group", {
  df <- data.frame(
    trap_id     = c("FC-001", "FC-001"),
    trap_date   = c("2023-06-20", "2023-06-20"),
    spp         = c("Culex tarsalis", "Malfunction"),
    total       = c(5L, 0L),
    zone        = c("NE", "NE"),
    trap_status = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  result <- wnv_s_clean(df, verbose = FALSE)
  expect_true(all(result$trap_status == "malfunction"))
  expect_true(all(is.na(result$total)))
})

# -- zone2 derivation ----------------------------------------------------------
test_that("zone2 is 'FC' for Fort Collins zones (NE, NW, SE, SW)", {
  result <- wnv_s_clean(make_obs("Culex tarsalis", 5, zone = "NE"), verbose = FALSE)
  expect_true("zone2" %in% names(result))
  expect_equal(unique(result$zone2), "FC")
})

test_that("zone2 equals zone for non-FC zones", {
  result <- wnv_s_clean(make_obs("Culex tarsalis", 5, zone = "LV"), verbose = FALSE)
  expect_equal(unique(result$zone2), "LV")
})

# -- verbose toggle ------------------------------------------------------------
test_that("verbose = FALSE produces no output", {
  out_msg <- capture.output(
    invisible(wnv_s_clean(make_obs("Culex tarsalis", 10), verbose = FALSE)),
    type = "message"
  )
  out_std <- capture.output(
    invisible(wnv_s_clean(make_obs("Culex tarsalis", 10), verbose = FALSE)),
    type = "output"
  )
  expect_equal(out_msg, character(0))
  expect_equal(out_std, character(0))
})
