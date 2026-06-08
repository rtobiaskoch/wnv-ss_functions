#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data :=
## usethis namespace: end
NULL

# Column names referenced as bare symbols in tidy-eval calls (mostly the LHS
# of `mutate(col = ...)`). Declared here so R CMD check does not flag them as
# undefined globals. Update this list when adding new exported functions that
# introduce columns by bare-name assignment.
utils::globalVariables(c(
  # cleaning columns
  "csu_id", "trap_id", "trap_date", "trap_status",
  "zone", "zone2", "year", "week",
  "spp", "spp0", "method", "total",
  # qPCR / platemap columns
  "cq", "log_copies", "copies", "test_code", "amp_status",
  "target", "target_name", "sample_type", "plate", "row", "column",
  "file_name", "well_position",
  # calc / plot intermediates
  "abund", "abund_sd", "abund_lci", "mosq_L", "trap_L",
  "pir", "pir_lci", "pir_uci", "P", "Upper", "Lower",
  "vi", "vi_lci", "vi_uci",
  "n", "n_dates", "has_wknd", "is_wknd",
  "dedup_key", "total_sum", "grp", "est", "value", "type"
))
