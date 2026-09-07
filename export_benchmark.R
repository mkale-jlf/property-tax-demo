# Generate the per-property benchmark file (data/{county}_benchmark.json) for the
# property-tax demo. Follows the data standard of the hand-validated
# mecklenburg/halifax benchmark files: official NCDOR levy/base/rate inputs +
# official BLS national CPI-U and Census population growth.
#
# Usage:
#   Rscript export_benchmark.R <Target County> <r_actual> <x> <r_2016> <l_2016> <l_actual> <growth.csv>
#
#   <Target County>  Place name, e.g. "Mecklenburg", "New Hanover".
#   <r_actual>       Official FY2025-26 county rate, dollars per $100 (NCDOR LG04).
#   <x>              Official FY2025-26 total taxable base (assessed valuation).
#   <r_2016>         Official FY2016-17 county rate, dollars per $100.
#   <l_2016>         Official FY2016-17 county-wide levy (NCDOR LG04).
#   <l_actual>       Official FY2025-26 county-wide levy (NCDOR LG04).
#   <growth.csv>     REQUIRED per-year growth override (the official standard, and
#                    the repo CSVs cannot supply 2024/2025 or the official CPI
#                    series). Columns, one row per transition year 2017-2025:
#                        year,inflation,population
#                    Decimal fractions where inflation is the CPI change T-1 to T
#                    (BLS national CPI-U annual average) and population is the
#                    county resident population change T-1 to T (Census vintage
#                    estimates). Optional extra columns are ignored.
#
# Nine transitions compound the 2016 baseline levy to the 2025 benchmark ceiling.
# Output writes to data/{key}_benchmark.json.

suppressMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
target    <- args[1]
r_actual  <- as.numeric(args[2])
x         <- as.numeric(args[3])
r_2016    <- if (length(args) >= 4 && !is.na(args[4]) && args[4] != "") as.numeric(args[4]) else NULL
l_2016    <- if (length(args) >= 5 && !is.na(args[5]) && args[5] != "") as.numeric(args[5]) else NULL
l_actual  <- if (length(args) >= 6 && !is.na(args[6]) && args[6] != "") as.numeric(args[6]) else NULL
growth_csv <- if (length(args) >= 7 && !is.na(args[7]) && args[7] != "") args[7] else NULL
stopifnot(
  !is.na(target), is.finite(r_actual), is.finite(x),
  length(growth_csv) == 1 && file.exists(growth_csv),
  is.finite(r_2016), is.finite(l_2016), is.finite(l_actual)
)

preferred_start_year <- 2016L
preferred_end_year   <- 2025L
registry_path <- "data/counties.json"

# ---- growth: official T-1->T mapping (required) ----
growth <- read_csv(growth_csv, show_col_types = FALSE) %>%
  select(year, inflation, population) %>%
  mutate(year = as.integer(year),
         inflation = suppressWarnings(as.numeric(inflation)),
         population = suppressWarnings(as.numeric(population))) %>%
  arrange(year) %>%
  filter(year %in% (preferred_start_year + 1):preferred_end_year)
expected_years <- (preferred_start_year + 1):preferred_end_year
present <- setdiff(expected_years, growth$year)
if (length(present) > 0) {
  stop("growth.csv missing transition years: ", paste(present, collapse = ", "))
}
if (anyNA(growth$inflation) || anyNA(growth$population)) {
  stop("growth.csv has non-numeric inflation/population values")
}
if (nrow(growth) != length(expected_years)) {
  stop("growth.csv must contain exactly the nine transition years 2017-2025")
}
stopifnot(all(growth$inflation >= -1), all(growth$population >= -1))

# ---- benchmark ceiling ----
transitions <- growth %>%
  mutate(
    growth_rate = population + inflation,
    benchmark_levy = NA_real_
  )
b <- l_2016
for (i in seq_len(nrow(transitions))) {
  g <- transitions$growth_rate[i]
  b <- b * (1 + g)
  transitions$benchmark_levy[i] <- b
}
b_endpoint <- b

l_scenario <- min(l_actual, b_endpoint)
r_scenario <- 100 * l_scenario / x
rate_reconciliation_pct <- 100 * (x * r_actual / 100 - l_actual) / l_actual

# ---- FIPS from registry ----
registry <- fromJSON(registry_path, flatten = FALSE)
reg_key <- names(registry)[vapply(registry, function(e) e$label, "") == paste0(target, " County")]
fips3 <- if (length(reg_key) == 1 && !is.null(registry[[reg_key]]$fips)) {
  substr(registry[[reg_key]]$fips, nchar(registry[[reg_key]]$fips) - 2, nchar(registry[[reg_key]]$fips))
} else NA_character_
if (is.na(fips3)) warning("Could not look up FIPS in data/counties.json for ", target)

# ---- output ----
out <- list(
  county = paste0(target, " County, NC"),
  fips = fips3,
  scope_year = preferred_end_year,
  baseline_year = preferred_start_year,
  r_actual = r_actual,
  r_2016 = r_2016,
  x = x,
  l_actual = l_actual,
  l_2016 = l_2016,
  b_endpoint = round(b_endpoint, 2),
  l_scenario = round(l_scenario, 2),
  r_scenario = round(r_scenario, 4),
  rate_reconciliation_pct = round(rate_reconciliation_pct, 3),
  transitions = transitions %>%
    transmute(to_year = year, inflation = round(inflation, 8),
              population = round(population, 8), growth = round(growth_rate, 8),
              benchmark_levy = round(benchmark_levy, 2)),
  mapping_note = paste0(
    "The transition into year T uses that year's CPI inflation change (BLS national ",
    "CPI-U annual average, T-1 to T) and ", target,
    " County resident population growth from Census vintage estimates (T-1 to T). ",
    "Nine transitions compound the 2016 baseline levy to the 2025 benchmark ceiling."
  ),
  sources = list(
    ncdor_levy = "https://www.ncdor.gov/news/local-government/reports-statistics-property-tax-rates",
    parcels = "https://services.nconemap.gov/secure/rest/services/NC1Map_Parcels/MapServer/0",
    census_pop = "https://www.census.gov/programs-surveys/popest.html",
    bls_cpi = "https://www.bls.gov/cpi/",
    hb1089 = "https://www.ncleg.gov/EnactedLegislation/SessionLaws/HTML/2025-2026/SL2026-5.html"
  )
)

key <- str_to_lower(str_replace_all(target, "[^A-Za-z0-9]+", "_"))
out_json <- file.path("data", paste0(key, "_benchmark.json"))
writeLines(toJSON(out, pretty = TRUE, auto_unbox = TRUE, digits = NA), out_json)

cat("Wrote", out_json, "\n")
cat("L2016:", format(l_2016, big.mark=","),
    "| L_actual:", format(l_actual, big.mark=","),
    "| B_endpoint:", format(round(b_endpoint,2), big.mark=","),
    "| r_scenario:", r_scenario,
    "| reconciliation:", round(rate_reconciliation_pct,3), "%\n")
if (abs(rate_reconciliation_pct) > 0.5) {
  cat("WARNING: rate/base/levy reconcile by", round(rate_reconciliation_pct,3),
      "% - verify official FY2025-26 rate, taxable base, and levy.\n")
}