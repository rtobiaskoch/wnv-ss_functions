## data-raw/zone_lvls.R
## Canonical ordering of Larimer County (and Boulder) trapping zones.
## Source: wnv-ss_trap_hx_combiner/R/fun_wnv_s_clean.R default arg.
## NW–SW = Fort Collins quadrants; FC = Fort Collins composite;
## LV = Loveland; BE = Berthoud; BC = Boulder.
## Re-build with: source("data-raw/zone_lvls.R")

zone_lvls <- c("NW", "NE", "SE", "SW", "FC", "LV", "BE", "BC")

usethis::use_data(zone_lvls, overwrite = TRUE)
