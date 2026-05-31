# ==============================================================================
# 03_main_did.R
# Table 3: Baseline staggered difference-in-differences estimates
# Two-way fixed effects (household + year FE) with clustered SEs
# ==============================================================================

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)

df       <- readRDS("data/processed/chfs_rural_panel.rds")
controls <- readRDS("data/processed/controls_vector.rds")

# Subsample with debt (for informal_share outcome)
df_debt <- df %>% filter(!is.na(informal_share))

# Build control formula string
ctrl_str <- paste(controls, collapse = " + ")

# ------------------------------------------------------------------------------
# 1. TWFE DID: Health outcomes (Panel A)
# ------------------------------------------------------------------------------
# Model: Y_it = alpha + beta * EII_it + gamma_i + delta_t + X'_it * theta + e_it
# FE: household (hhid) + year (wave)
# SE: clustered at household level

fml_hosp  <- as.formula(paste("child_hosp  ~ EII +", ctrl_str, "| hhid + wave"))
fml_med   <- as.formula(paste("med_exp     ~ EII +", ctrl_str, "| hhid + wave"))
fml_cred  <- as.formula(paste("informal_share ~ EII +", ctrl_str, "| hhid + wave"))
fml_asset <- as.formula(paste("asset_share ~ EII +", ctrl_str, "| hhid + wave"))

m_hosp  <- feols(fml_hosp,  data = df,      cluster = ~hhid)
m_med   <- feols(fml_med,   data = df,      cluster = ~hhid)
m_cred  <- feols(fml_cred,  data = df_debt, cluster = ~hhid)
m_asset <- feols(fml_asset, data = df,      cluster = ~hhid)

# Compute percentage changes relative to baseline means
bl_hosp  <- mean(df$child_hosp[df$wave == 2018],         na.rm = TRUE)
bl_med   <- mean(df$med_exp[df$wave == 2018],            na.rm = TRUE)
bl_cred  <- mean(df_debt$informal_share[df_debt$wave == 2018], na.rm = TRUE)
bl_asset <- mean(df$asset_share[df$wave == 2018],        na.rm = TRUE)

coef_hosp  <- coef(m_hosp)["EII"]
coef_med   <- coef(m_med)["EII"]
coef_cred  <- coef(m_cred)["EII"]
coef_asset <- coef(m_asset)["EII"]

cat("\n=== Main DID Results ===\n")
cat(sprintf("Child Hospitalization:     coef = %.3f  (%% change = %.1f%%)\n",
            coef_hosp,  coef_hosp  / bl_hosp  * 100))
cat(sprintf("Medical Expenditure:       coef = %.3f  (%% change = %.1f%%)\n",
            coef_med,   coef_med   / bl_med   * 100))
cat(sprintf("Informal Credit Share:     coef = %.3f  (%% change = %.1f%%)\n",
            coef_cred,  coef_cred  / bl_cred  * 100))
cat(sprintf("Income-Generating Asset Share: coef = %.3f  (%% change = %.1f%%)\n",
            coef_asset, coef_asset / bl_asset * 100))

# ------------------------------------------------------------------------------
# 2. Export Table 3
# ------------------------------------------------------------------------------
models <- list(
  "Child Hosp."   = m_hosp,
  "Med. Exp."     = m_med,
  "Informal Cred."= m_cred,
  "Asset Share"   = m_asset
)

coef_map <- c("EII" = "EII (Environmental Infrastructure Index)")

tbl3 <- modelsummary(
  models,
  coef_map    = coef_map,
  gof_map     = c("nobs", "r.squared"),
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt         = 3,
  output      = "kableExtra",
  title       = "Baseline DiD Estimates: Effects of Environmental Infrastructure on Outcomes",
  notes       = "Household and year fixed effects included in all specifications.
                 Standard errors clustered at the household level (in parentheses).
                 Informal Credit Share estimated on debt-holding subsample (N = 47,832)."
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  add_header_above(c(" " = 1,
                     "Panel A: Health Outcomes" = 2,
                     "Panel B: Financial Outcomes" = 2))

save_kable(tbl3, "output/tables/table3_main_did.tex")
cat("Table 3 saved to output/tables/table3_main_did.tex\n")

# ------------------------------------------------------------------------------
# 3. Sun & Abraham (2021) timing-robust estimator (pre-registered check)
# ------------------------------------------------------------------------------
# Requires first_treat variable (Inf for never-treated)

df_sa <- df %>%
  mutate(first_treat_sa = ifelse(is.infinite(first_treat), 0L, as.integer(first_treat)))
df_debt_sa <- df_debt %>%
  mutate(first_treat_sa = ifelse(is.infinite(first_treat), 0L, as.integer(first_treat)))

fml_sa_cred  <- as.formula(paste(
  "informal_share ~ sunab(first_treat_sa, wave) +", ctrl_str, "| hhid + wave"))
fml_sa_asset <- as.formula(paste(
  "asset_share ~ sunab(first_treat_sa, wave) +", ctrl_str, "| hhid + wave"))

m_sa_cred  <- feols(fml_sa_cred,  data = df_debt_sa, cluster = ~hhid)
m_sa_asset <- feols(fml_sa_asset, data = df_sa,      cluster = ~hhid)

cat("\n=== Sun & Abraham (2021) Estimates ===\n")
print(aggregate(m_sa_cred))
print(aggregate(m_sa_asset))

# Save Sun-Abraham estimates for reference
sa_results <- tibble(
  outcome       = c("Informal Credit Share", "Asset Share"),
  sa_coef       = c(aggregate(m_sa_cred)[1, "Estimate"],
                    aggregate(m_sa_asset)[1, "Estimate"]),
  sa_se         = c(aggregate(m_sa_cred)[1, "Std. Error"],
                    aggregate(m_sa_asset)[1, "Std. Error"]),
  baseline_coef = c(coef_cred, coef_asset)
)
saveRDS(sa_results, "data/processed/sa_estimates.rds")
