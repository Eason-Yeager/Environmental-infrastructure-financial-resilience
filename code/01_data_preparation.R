# ==============================================================================
# 01_data_preparation.R
# Data cleaning, variable construction, and panel assembly
# ==============================================================================

library(tidyverse)

# ------------------------------------------------------------------------------
# 1. Load raw CHFS data (2018–2022 waves)
# ------------------------------------------------------------------------------
# Adjust file paths and read function to match your CHFS file format.
# CHFS distributes data in .dta (Stata), .xlsx, or .csv format depending on
# the request. Use haven::read_dta() for Stata files or readxl::read_xlsx().

raw_files <- list(
  "2018" = "data/raw/chfs2018_household.dta",
  "2019" = "data/raw/chfs2019_household.dta",
  "2020" = "data/raw/chfs2020_household.dta",
  "2021" = "data/raw/chfs2021_household.dta",
  "2022" = "data/raw/chfs2022_household.dta"
)

# Check files exist
missing_files <- raw_files[!file.exists(unlist(raw_files))]
if (length(missing_files) > 0) {
  stop(
    "Missing CHFS data files:\n",
    paste(unlist(missing_files), collapse = "\n"),
    "\nPlease obtain CHFS data from https://chfs.swufe.edu.cn"
  )
}

# Read all waves and stack
waves <- imap(raw_files, function(path, yr) {
  df <- haven::read_dta(path)
  df$wave <- as.integer(yr)
  df
})

raw <- bind_rows(waves)
cat("Raw data loaded:", nrow(raw), "rows,", n_distinct(raw$hhid), "unique households\n")

# ------------------------------------------------------------------------------
# 2. Restrict to rural households
# ------------------------------------------------------------------------------
# CHFS rural indicator: rural_flag == 1 or hukou_type == 1 (agricultural hukou)
# Variable names may differ slightly across CHFS waves; adjust as needed.
rural <- raw %>%
  filter(rural_flag == 1 | hukou_type == 1)

cat("Rural sample:", nrow(rural), "rows,", n_distinct(rural$hhid), "unique households\n")

# ------------------------------------------------------------------------------
# 3. Construct treatment variables: Environmental Infrastructure Index (EII)
# ------------------------------------------------------------------------------
# Water source codes (CHFS question on drinking water source):
#   1 = piped tap water (municipal/community), 2 = bottled/purified -> SAFE
#   3 = well (covered), 4 = well (open), 5 = river/lake, 6 = other -> UNSAFE
# Cooking fuel codes:
#   1 = natural gas (pipeline), 2 = LPG, 3 = electricity -> CLEAN
#   4 = coal, 5 = wood, 6 = crop residues, 7 = other solid -> DIRTY
# Toilet facility codes:
#   1 = flush toilet (sewer/septic), 2 = improved pit latrine -> IMPROVED
#   3 = open pit, 4 = hanging latrine, 5 = open defecation -> UNIMPROVED
# NOTE: Recode as needed if your CHFS wave uses different codes.

rural <- rural %>%
  mutate(
    safe_water  = as.integer(water_source %in% c(1, 2)),
    clean_energy = as.integer(cooking_fuel %in% c(1, 2, 3)),
    sanitation  = as.integer(toilet_type %in% c(1, 2)),
    EII = safe_water + clean_energy + sanitation
  )

# ------------------------------------------------------------------------------
# 4. Identify treatment timing (first improvement wave per dimension)
# ------------------------------------------------------------------------------
# For staggered DID, we need the wave in which each household FIRST achieved
# each infrastructure improvement (treatment cohort variable).

rural <- rural %>%
  arrange(hhid, wave) %>%
  group_by(hhid) %>%
  mutate(
    # Baseline (2018) status
    baseline_water  = first(safe_water),
    baseline_energy = first(clean_energy),
    baseline_san    = first(sanitation),
    baseline_EII    = first(EII),

    # Flag improvement events (0->1 transitions)
    improve_water  = (safe_water == 1 & lag(safe_water, default = first(safe_water)) == 0),
    improve_energy = (clean_energy == 1 & lag(clean_energy, default = first(clean_energy)) == 0),
    improve_san    = (sanitation == 1 & lag(sanitation, default = first(sanitation)) == 0),

    # First treatment wave for composite EII improvement
    first_treat_water  = ifelse(any(improve_water),  min(wave[improve_water]),  Inf),
    first_treat_energy = ifelse(any(improve_energy), min(wave[improve_energy]), Inf),
    first_treat_san    = ifelse(any(improve_san),    min(wave[improve_san]),    Inf),

    # For main DID: treated = 1 if any improvement during study period
    ever_treated = as.integer(EII > baseline_EII),
    first_treat  = ifelse(ever_treated == 1, min(wave[EII > baseline_EII]), Inf)
  ) %>%
  ungroup()

cat("Treatment summary:\n")
rural %>%
  distinct(hhid, ever_treated) %>%
  count(ever_treated) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

# ------------------------------------------------------------------------------
# 5. Construct outcome variables
# ------------------------------------------------------------------------------
rural <- rural %>%
  mutate(
    # Health outcomes
    child_hosp = as.integer(child_hosp_any == 1),   # child hospitalization indicator
    med_exp    = med_exp_oop / 1000,                  # out-of-pocket medical exp (1000 yuan)
    # Winsorize at 99th pctile
    med_exp    = pmin(med_exp, quantile(med_exp, 0.99, na.rm = TRUE)),

    # Credit outcome: informal credit share (only for households with debt > 0)
    total_debt    = pmax(total_debt_all, 0),
    informal_debt = pmax(informal_debt_all, 0),
    informal_share = ifelse(total_debt > 0, informal_debt / total_debt, NA_real_),
    has_debt      = as.integer(total_debt > 0),

    # Asset portfolio outcome: income-generating asset share
    income_assets = agri_capital + business_assets + coop_equity,
    income_assets = pmax(income_assets, 0),
    total_assets  = pmax(total_assets_all, 0),
    asset_share   = ifelse(total_assets > 0, income_assets / total_assets, 0),
    asset_share   = pmin(pmax(asset_share, 0), 1),  # Bound to [0,1]

    # Log income
    log_income = log(pmax(total_income_annual, 1))
  )

# ------------------------------------------------------------------------------
# 6. Construct control variables
# ------------------------------------------------------------------------------
rural <- rural %>%
  mutate(
    age_head_sq  = age_head^2,
    # Resource endowment composite for threshold analysis (normalized to [0,1])
    re_income    = (log_income - min(log_income, na.rm = TRUE)) /
                   (max(log_income, na.rm = TRUE) - min(log_income, na.rm = TRUE)),
    re_assets    = (log(total_assets + 1) - min(log(total_assets + 1), na.rm = TRUE)) /
                   (max(log(total_assets + 1), na.rm = TRUE) - min(log(total_assets + 1), na.rm = TRUE)),
    re_insurance = as.numeric(nrcms_enroll == 1),
    RE           = (re_income + re_assets + re_insurance) / 3   # [0,1] composite
  )

# Community-level EII average (for IV construction)
rural <- rural %>%
  group_by(county_code, wave) %>%
  mutate(community_eii = mean(EII, na.rm = TRUE)) %>%
  ungroup()

# Macro-region classification
rural <- rural %>%
  mutate(
    region = case_when(
      province %in% c(11, 12, 13, 21, 22, 23) ~ "Northeast/North",
      province %in% c(31, 32, 33, 34, 35, 36, 37, 44, 45, 46) ~ "Eastern",
      TRUE ~ "Central-Western"
    )
  )

# ------------------------------------------------------------------------------
# 7. Restrict to minimum 3 observations per household (unbalanced panel)
# ------------------------------------------------------------------------------
rural <- rural %>%
  group_by(hhid) %>%
  filter(n() >= 3) %>%
  ungroup()

cat("Final sample:", nrow(rural), "household-year obs,",
    n_distinct(rural$hhid), "unique households\n")

# ------------------------------------------------------------------------------
# 8. Panel id and time variables for fixest
# ------------------------------------------------------------------------------
rural <- rural %>%
  mutate(
    hhid   = as.factor(hhid),
    wave_f = as.factor(wave)
  )

# ------------------------------------------------------------------------------
# 9. Define control variable vector (used in all regressions)
# ------------------------------------------------------------------------------
controls <- c(
  "hh_size", "age_head", "age_head_sq", "edu_head",
  "female_head", "n_children", "n_elderly", "married_head",
  "log_income", "farmland", "has_migrant", "agri_share",
  "chronic_illness", "nrcms_enroll", "insurance_other",
  "has_debt"
)

# ------------------------------------------------------------------------------
# 10. Save processed data
# ------------------------------------------------------------------------------
saveRDS(rural,    "data/processed/chfs_rural_panel.rds")
saveRDS(controls, "data/processed/controls_vector.rds")

cat("Processed data saved to data/processed/chfs_rural_panel.rds\n")
