## data-raw/fc_zones.R
## Fort Collins–only subset of zone_lvls, used by calc_pir/calc_vi to roll
## sub-zones up to a composite "FC" group.
## Source: config_culex_combine.yml `fc_zone:` block.

fc_zones <- c("NE", "NW", "SE", "SW")

usethis::use_data(fc_zones, overwrite = TRUE)
