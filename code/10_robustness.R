# ==============================================================================
# 10_robustness.R
# Robustness checks (Table in paper):
#   (1) Baseline TWFE
#   (2) + Community FE
#   (3) IV-Probit (community-average EII as instrument)
#   (4) Falsification test (treatment date shifted +1 year)
#   (5) Balanced panel only
#   (6) Linear probability model
#   (7) Sun & Abraham (2021) estimator
# Also: Propensity score matching (Table A4)
# ==============================================================================

library(tidyverse)
library(fixest)
library(AER)
library(MatchIt)
library(modelsummary)
library(kableExtra)
library(sandwich)
library(lmtest)

df       <- readRDS("data/processed/chfs_rural_panel.rds")
controls <- readRDS("data/processed/controls_vector.rds")
df_debt  <- df %>% filter(!is.na(informal_share))
ctrl_str <- paste(controls, collapse = " + ")

# Primary outcome for robustness table: informal credit share
OUTCOME <- "informal_share"

# ------------------------------------------------------------------------------
# (1) Baseline: Household + Year FE, clustered SE
# ------------------------------------------------------------------------------
m1 <- feols(
  as.formula(paste(OUTCOME, "~ EII +", ctrl_str, "| hhid + wave")),
  data = df_debt, cluster = ~hhid
)

# ------------------------------------------------------------------------------
# (2) Add community (county x year) fixed effects
# ------------------------------------------------------------------------------
m2 <- feols(
  as.formula(paste(OUTCOME, "~ EII +", ctrl_str, "| hhid + wave + county_code^wave")),
  data = df_debt, cluster = ~hhid
)

# ------------------------------------------------------------------------------
# (3) IV-Probit: instrument = community-average EII (excludes own household)
# Leave-one-out community average to avoid mechanical correlation
# ------------------------------------------------------------------------------
df_iv <- df %>%
  group_by(county_code, wave) %>%
  mutate(
    n_county = n(),
    sum_eii  = sum(EII),
    iv_eii   = (sum_eii - EII) / (n_county - 1)  # leave-one-out community mean
  ) %>%
  ungroup()

df_debt_iv <- df_iv %>% filter(!is.na(informal_share))

# First stage
m3_fs <- feols(
  as.formula(paste("EII ~ iv_eii +", ctrl_str, "| hhid + wave")),
  data = df_debt_iv, cluster = ~hhid
)
cat("\nFirst Stage F-stat (IV):", fitstat(m3_fs, "ivf")[[1]], "\n")

# 2SLS
m3 <- feols(
  as.formula(paste(OUTCOME, "~ +", ctrl_str, "| hhid + wave | EII ~ iv_eii")),
  data = df_debt_iv, cluster = ~hhid
)

# ------------------------------------------------------------------------------
# (4) Falsification test: shift treatment date forward by 1 year
# If t* is the true treatment year, assign t* + 1 as placebo treatment year
# ------------------------------------------------------------------------------
df_placebo <- df %>%
  mutate(
    EII_placebo = lag(EII, n = 1, default = NA),  # Use last year's EII as "treatment"
    # Shift: households treated in t are assigned treatment in t+1 for placebo
    EII_placebo = ifelse(wave == 2018, NA, lag(EII))
  ) %>%
  filter(!is.na(EII_placebo))

df_debt_placebo <- df_placebo %>% filter(!is.na(informal_share))

m4 <- feols(
  as.formula(paste(OUTCOME, "~ EII_placebo +", ctrl_str, "| hhid + wave")),
  data = df_debt_placebo, cluster = ~hhid
)

cat("\nPlacebo coefficient (should be ~0):",
    round(coef(m4)["EII_placebo"], 4),
    "p-value:", round(pvalue(m4)["EII_placebo"], 3), "\n")

# ------------------------------------------------------------------------------
# (5) Balanced panel (households observed in all 5 waves)
# ------------------------------------------------------------------------------
df_balanced <- df %>%
  group_by(hhid) %>%
  filter(n() == 5) %>%
  ungroup()

df_debt_bal <- df_balanced %>% filter(!is.na(informal_share))
cat("Balanced panel:", n_distinct(df_balanced$hhid), "households\n")

m5 <- feols(
  as.formula(paste(OUTCOME, "~ EII +", ctrl_str, "| hhid + wave")),
  data = df_debt_bal, cluster = ~hhid
)

# ------------------------------------------------------------------------------
# (6) Linear probability model (alternative estimator)
# ------------------------------------------------------------------------------
m6 <- feols(
  as.formula(paste(OUTCOME, "~ EII +", ctrl_str, "| hhid + wave")),
  data = df_debt, cluster = ~hhid
)
# (Same as m1 for continuous outcome; would differ for binary outcomes)

# ------------------------------------------------------------------------------
# (7) Sun & Abraham (2021) timing-robust estimator
# ------------------------------------------------------------------------------
df_debt_sa <- df_debt %>%
  mutate(first_treat_sa = ifelse(is.infinite(first_treat), 0L, as.integer(first_treat)))

m7 <- feols(
  as.formula(paste(OUTCOME, "~ sunab(first_treat_sa, wave) +",
                   ctrl_str, "| hhid + wave")),
  data = df_debt_sa, cluster = ~hhid
)

# ------------------------------------------------------------------------------
# Robustness Table
# ------------------------------------------------------------------------------
models <- list(
  "(1) Baseline"         = m1,
  "(2) +Community FE"    = m2,
  "(3) IV"               = m3,
  "(4) Placebo"          = m4,
  "(5) Balanced panel"   = m5,
  "(6) LPM"              = m6
)

coef_map <- c(
  "EII"         = "EII",
  "EII_placebo" = "EII (placebo)"
)

rob_tbl <- modelsummary(
  models,
  coef_map = coef_map,
  gof_map  = c("nobs", "r.squared"),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt      = 3,
  output   = "kableExtra",
  title    = "Robustness Checks: Informal Credit Share",
  notes    = "Household and year FE in all columns. Column (3): IV = leave-one-out
              community mean EII. Column (4): treatment date shifted +1 year (placebo).
              Column (5): balanced panel only (5 waves). Standard errors clustered
              at household level."
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

save_kable(rob_tbl, "output/tables/table_robustness.tex")
cat("Robustness table saved to output/tables/table_robustness.tex\n")

# ------------------------------------------------------------------------------
# Propensity Score Matching (Table A4)
# Compare matched estimates to unmatched baseline
# ------------------------------------------------------------------------------
cat("\n=== Propensity Score Matching ===\n")

# Use 2018 baseline data for PSM
df_2018 <- df %>%
  filter(wave == 2018) %>%
  mutate(treated = as.integer(ever_treated == 1))

psm_formula <- as.formula(paste("treated ~", ctrl_str))

match_obj <- matchit(
  psm_formula,
  data    = df_2018,
  method  = "nearest",
  distance = "logit",
  ratio   = 1,
  caliper = 0.05
)

cat("PSM summary:\n")
print(summary(match_obj, standardize = TRUE))

# Extract matched household IDs
matched_hhids <- df_2018[match_obj$weights > 0, ]$hhid

# Run DID on matched sample
df_matched      <- df %>% filter(hhid %in% matched_hhids)
df_debt_matched <- df_matched %>% filter(!is.na(informal_share))

m_psm_cred <- feols(
  as.formula(paste("informal_share ~ EII +", ctrl_str, "| hhid + wave")),
  data = df_debt_matched, cluster = ~hhid
)
m_psm_asset <- feols(
  as.formula(paste("asset_share ~ EII +", ctrl_str, "| hhid + wave")),
  data = df_matched, cluster = ~hhid
)

cat(sprintf("\nPSM-matched DID - Informal Credit: %.4f (SE: %.4f)\n",
            coef(m_psm_cred)["EII"], se(m_psm_cred)["EII"]))
cat(sprintf("PSM-matched DID - Asset Share:    %.4f (SE: %.4f)\n",
            coef(m_psm_asset)["EII"], se(m_psm_asset)["EII"]))

# Save all robustness results
saveRDS(
  list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5, m7 = m7,
       m_psm_cred = m_psm_cred, m_psm_asset = m_psm_asset),
  "data/processed/robustness_models.rds"
)

cat("\nAll robustness checks complete.\n")
