## data-raw/trap_status_colors.R
## Named ggplot fill palette for trap-status states used by plot_n_trap().
## Source: wnv-ss_trap_hx_combiner/R/palettes.R

trap_status_colors <- c(
  "culex"         = "#4e9ec2",
  "no mosquitoes" = "#cccccc",
  "no traps"      = "grey20",
  "no culex"      = "#a8c97f",
  "malfunction"   = "#d62728"
)

usethis::use_data(trap_status_colors, overwrite = TRUE)
