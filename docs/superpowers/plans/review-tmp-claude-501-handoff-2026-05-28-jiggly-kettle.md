# Plan — Fix open silent-failure bugs in `wnvSurv` calc functions

## Context

The handoff at `/tmp/claude-501/handoff-2026-05-28.md` documents an audit of the
WNV/SLEV surveillance calculation chain. Seven silent-failure bugs were found in
the legacy `wnv-ss-wkly_report/utils/` source-of-truth files. Three are already
fixed in this package (#1 `rm.na` typo, #4 `grp_zones` free variable, partial
#7). Four remain open in `R/calc_abund.R` and `R/calc_pir.R`. All four are
"runs clean, returns a wrong number" bugs — exactly the class that erodes trust
in the headline epi metrics (PIR, abundance, VI) that drive adulticide
decisions in Larimer County. The user has made the four design decisions the
handoff flagged as deferred (see below); this plan executes those decisions
with TDD.

### User decisions recorded this session
| Bug | Decision |
|----|----------|
| #2 (n=1 sd) | Minimum fix: add `na.rm = TRUE`; `sd` is NA for n=1 by definition, CIs propagate as NA. Honest about uncertainty. |
| #3 (abund ÷0) | Empty/un-sampled zone → `abund = NA` (not 0). VI will also propagate NA — surveillance gaps must not look like measured zeros. |
| #5 (unite/separate) | Avoid the `"_"` round-trip entirely; carry the original group columns through and join PIR results back. |
| #6 (PIR NA→0) | Un-sampled group → `pir = NA`. Distinguish "not measured" from "tested zero positives". |

## Files to change

| File | Change |
|------|--------|
| `R/calc_abund.R` | Fix #2 and #3 — one-line each, plus update `@return` docs |
| `R/calc_pir.R` | Fix #5 (drop unite/separate round-trip) and #6 (do not blanket-replace NA→0); update docs |
| `tests/testthat/test-calc_abund.R` | Add failing tests for #2 (n=1) and #3 (empty zone) **first**, then fix |
| `tests/testthat/test-calc_pir.R` | Add failing tests for #5 (underscore in group value) and #6 (un-sampled zone stays NA) **first**, then fix |
| `tests/testthat/test-calc_vi.R` | Add test that NA abundance/PIR propagates to NA VI (consequence of #3 + #6 decisions) |

`R/calc_vi.R` itself needs no logic change — `abund * pir` already propagates
NA correctly. Only the test grid needs to assert the NA-propagation contract
so a future "helpful" coalesce-to-zero cannot silently regress it.

## Approach — TDD per bug

Follow `superpowers:test-driven-development`. For each bug: write one failing
testthat block that pins the new contract, run `devtools::test()` to see it
fail, then make the one-line code change, then re-run tests. Commit per bug.

### Bug #2 — `calc_abund` sd na.rm + n=1 contract
- **Test first:** add a fixture with one zone that has a single trap (n=1).
  Assert `abund_sd` is `NA_real_`, `abund_lci`/`abund_uci` are `NA_real_`,
  and `abund` itself is the single observed value (not NA).
- **Fix:** `R/calc_abund.R:61` →
  `abund_sd = round(stats::sd(.data$total, na.rm = TRUE), 4)`
- **Doc update:** add a one-line note under `@return` that `abund_sd` and CIs
  are `NA` for groups with n=1 trap (sd undefined).

### Bug #3 — `calc_abund` divide-by-zero on empty zones
- **Test first:** add a fixture row where `rm_zone` removes all traps from
  one zone, or filters all rows for a zone via `trap_status = "malfunction"`.
  Assert `abund` is `NA_real_` (not `NaN`/`Inf`/`0`) for that zone, and that
  `abund_lci`/`abund_uci` are also `NA_real_`.
- **Fix:** `R/calc_abund.R:65` →
  `abund = dplyr::if_else(.data$trap_L == 0, NA_real_, round(.data$mosq_L / .data$trap_L, 4))`
  CI lines already propagate NA naturally because they multiply by `abund`.
- **Doc update:** add a one-line note under `@return` that zones with
  `trap_L == 0` report `abund = NA` (un-sampled, not measured zero).

### Bug #5 — `calc_pir` group-key round-trip on `"_"`
- **Test first:** add a fixture where one grouping value contains an
  underscore (e.g. `spp = "Cx_pipiens"`). Run `calc_pir(..., grp_var =
  c("zone","year","week","spp"))`. Assert that the output `spp` column equals
  the input value verbatim (no split into two pieces). This currently fails
  because `tidyr::separate(... sep = "_")` mangles it.
- **Fix:** in `R/calc_pir.R:62-80`, replace the unite/separate sandwich with:
  1. Build `grp` via `unite` for `PooledInfRate::pIR()` (it needs a single
     `grp` key).
  2. Build a `grp_lookup` tibble of the original group cols + `grp` (unique
     rows from `df_pir` before passing to pIR).
  3. After `as.data.frame(mle)`, `dplyr::left_join` the lookup back by
     `grp`, then `dplyr::select(-grp)`. No `separate()` call at all.
- **Result:** group columns retain their original *types* (no coerce-to-char
  → numeric round-trip), closing residual bug #7 too. Drop the
  `mutate(year = as.numeric(...), week = as.numeric(...))` cleanup — no
  longer needed.

### Bug #6 — `calc_pir` blanket NA→0
- **Test first:** add a fixture with two zones; only one has pools tested.
  Run `calc_pir(..., zone_complete = c("ZA","ZB"))`. Assert the un-sampled
  zone's row has `pir = NA_real_` (not 0), and that the sampled zone's
  zero-positive case still returns `pir = 0`.
- **Fix:** `R/calc_pir.R:114` — remove the line `df_pir[is.na(df_pir)] <- 0`
  entirely. The `tidyr::complete()` call above it adds rows for missing
  group combinations with NA — that is the correct semantic for
  "un-sampled".
- **Doc update:** in `@return`, change "Missing group/zone combinations are
  completed with PIR = 0" → "with PIR = NA (un-sampled, not measured
  zero)".

### VI propagation test (consequence of #3 + #6)
- **Test only:** add to `test-calc_vi.R` a fixture where one zone has no
  pools and no traps. Assert `vi`, `vi_lci`, `vi_uci` are all `NA_real_`
  for that zone. No code change needed — this is a regression guard.

## Reused utilities (no new abstractions)

- `dplyr::if_else()` — already imported in `calc_abund.R:27`, type-safe vs.
  base `ifelse()`.
- `tidyr::complete()` — already imported in `calc_pir.R:27`.
- `dplyr::left_join()` — replaces `tidyr::separate` for the group-key
  round-trip; standard tidyverse, no new dependency.

## Verification

End-to-end check after the four fixes are in place:

```r
devtools::load_all(".")
devtools::test()          # all four new tests + existing suite must pass
devtools::document()      # roxygen updates from @return notes
devtools::check()         # confirms no NAMESPACE drift, no R CMD warnings
```

Spot-check VI propagation interactively:

```r
# Build a fixture with one un-sampled zone, confirm vi is NA, not 0/NaN.
abund <- calc_abund(culex_data)        # contains an un-sampled zone via rm_zone
pir   <- calc_pir(pools_data, zone_complete = c("NW","UNSAMPLED"))
vi    <- calc_vi(abund, pir, complete = FALSE)
stopifnot(is.na(vi$vi[vi$zone == "UNSAMPLED"]))
```

## Out of scope (handoff Open Questions deferred)

- **Migration of `wnv-ss-wkly_report` from `source(utils/...)` to
  `library(wnvSurv)`** — handoff Open Question #3. Belongs to the consumer
  repo, not this package. Will surface as a separate session once the four
  bugs are fixed and tagged.
- **`table1a_w33_2025.csv` fixture validity** — handoff Open Question #2.
  After these fixes land, regenerate that fixture from the consumer pipeline
  and diff against the modified working-tree version to determine whether
  any past published numbers shifted (likely only at zones with n=1 or
  un-sampled groups). Track as a follow-up note to the user, not a code
  change here.
- **Past weekly reports re-run** — handoff Note about bug #1 (`rm.na`).
  Statistical question for the user, not part of this code change.
