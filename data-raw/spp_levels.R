## data-raw/spp_levels.R
## Canonical Culex species ordering for the Larimer County surveillance
## program. Tarsalis is listed first because it is the principal WNV vector
## in the region and is plotted on top in stacked bars.
## Source: config_culex_combine.yml `spp_levels:` block.

spp_levels <- c("Tarsalis", "Pipiens")

usethis::use_data(spp_levels, overwrite = TRUE)
