trap_for_plot <- tibble::tribble(
  ~trap_id,  ~zone, ~zone2, ~year, ~week, ~trap_status,
  "FC-001",  "NW",  "FC",   2023,  30,    "culex",
  "FC-002",  "NW",  "FC",   2023,  30,    "no culex",
  "FC-001",  "NW",  "FC",   2024,  30,    "culex",
  "FC-002",  "NW",  "FC",   2024,  30,    "no mosquitoes",
  "FC-001",  "NW",  "FC",   2025,  30,    "malfunction",
  "FC-002",  "NW",  "FC",   2025,  30,    "culex",
  "EV-001",  "EV",  "EV",   2025,  30,    "culex"
)

test_that("plot_n_trap with no year bounds keeps all years", {
  p <- plot_n_trap(trap_for_plot)
  expect_setequal(unique(p$data$year), c(2023, 2024, 2025))
})

test_that("plot_n_trap with only year_start drops earlier years", {
  p <- plot_n_trap(trap_for_plot, year_start = 2024)
  expect_setequal(unique(p$data$year), c(2024, 2025))
})

test_that("plot_n_trap with only year_end drops later years", {
  p <- plot_n_trap(trap_for_plot, year_end = 2024)
  expect_setequal(unique(p$data$year), c(2023, 2024))
})

test_that("plot_n_trap with both year bounds filters to the inclusive range", {
  p <- plot_n_trap(trap_for_plot, year_start = 2024, year_end = 2024)
  expect_setequal(unique(p$data$year), 2024)
})

test_that("plot_n_trap respects rm_zone alongside year filtering", {
  p <- plot_n_trap(trap_for_plot, rm_zone = "EV", year_start = 2025)
  expect_setequal(unique(p$data$year), 2025)
  expect_false("EV" %in% p$data$zone2)
})

test_that("plot_n_trap errors without a zone2 column", {
  expect_error(
    plot_n_trap(dplyr::select(trap_for_plot, -zone2)),
    "zone2"
  )
})
