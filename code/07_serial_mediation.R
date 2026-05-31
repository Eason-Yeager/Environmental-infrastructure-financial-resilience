# ==============================================================================
# 07_serial_mediation.R
# Table 6: Serial (chain) mediation analysis
# Tests the full pathway: EII -> Health Shocks (M1) -> Informal Credit (M2) -> Asset Share (Y)
# Following Hayes (2018) PROCESS macro logic, implemented via lavaan SEM
# with 5,000 bootstrap replications
# ==============================================================================

library(tidyverse)
library(lavaan)
library(kableExtra)

df       <- readRDS("data/processed/chfs_rural_panel.rds")
controls <- readRDS("data/processed/controls_vector.rds")

set.seed(20250527)

# Use demeaned data (panel FE absorbed via within-transformation)
# Re-use demean function from script 06
demean_panel <- function(data, vars, id_var = "hhid", time_var = "wave") {
  data <- as.data.frame(data)
  for (v in vars) {
    if (!v %in% names(data)) next
    hh_mean    <- ave(data[[v]], data[[id_var]],   FUN = function(x) mean(x, na.rm = TRUE))
    yr_mean    <- ave(data[[v]], data[[time_var]], FUN = function(x) mean(x, na.rm = TRUE))
    grand_mean <- mean(data[[v]], na.rm = TRUE)
    data[[paste0(v, "_dm")]] <- data[[v]] - hh_mean - yr_mean + grand_mean
  }
  data
}

all_vars <- c("EII", "child_hosp", "med_exp", "informal_share", "asset_share", controls)
df_dm    <- demean_panel(df, all_vars[all_vars %in% names(df)])

# Restrict to households with debt (for informal_share)
# Use the full df_dm but set informal_share_dm to NA where no debt
df_dm <- df_dm %>%
  mutate(informal_share_dm = ifelse(has_debt == 1, informal_share_dm, NA_real_))

dm <- function(x) paste0(x, "_dm")

# ------------------------------------------------------------------------------
# Serial mediation model (lavaan SEM)
# Path diagram:
#   EII --(a1)--> M1 (child_hosp or med_exp)
#        --(a2)--> M2 (informal_share)
#        --(c')--> Y  (asset_share)
#   M1  --(d21)--> M2
#   M1  --(b1)---> Y
#   M2  --(b2)---> Y
#
# Chain indirect effect = a1 * d21 * b2
# Indirect via M1 only  = a1 * b1
# Indirect via M2 only  = a2 * b2
# ------------------------------------------------------------------------------

build_serial_model <- function(x_var, m1_var, m2_var, y_var) {
  sprintf("
    # Stage 1: X -> M1
    %s ~ a1*%s + %s

    # Stage 2: X -> M2 (controlling for M1)
    %s ~ a2*%s + d21*%s + %s

    # Outcome: X + M1 + M2 -> Y
    %s ~ cp*%s + b1*%s + b2*%s + %s

    # Indirect effects
    indirect_m1_only  := a1 * b1                # X -> M1 -> Y
    indirect_m2_only  := a2 * b2                # X -> M2 -> Y
    chain_indirect    := a1 * d21 * b2          # X -> M1 -> M2 -> Y
    direct_effect     := cp                     # X -> Y (direct)
    total_effect      := cp + a1*b1 + a2*b2 + a1*d21*b2  # Total
  ",
    m1_var, x_var, paste(dm(controls), collapse = " + "),
    m2_var, x_var, m1_var, paste(dm(controls), collapse = " + "),
    y_var, x_var, m1_var, m2_var, paste(dm(controls), collapse = " + ")
  )
}

# Primary model: M1 = child hospitalization (main health shock measure)
model_str_hosp <- build_serial_model(
  x_var  = dm("EII"),
  m1_var = dm("child_hosp"),
  m2_var = dm("informal_share"),
  y_var  = dm("asset_share")
)

# Secondary model: M1 = medical expenditure (robustness)
model_str_med <- build_serial_model(
  x_var  = dm("EII"),
  m1_var = dm("med_exp"),
  m2_var = dm("informal_share"),
  y_var  = dm("asset_share")
)

cat("\n=== Fitting serial mediation model (M1 = child hospitalization) ===\n")
fit_hosp <- sem(
  model_str_hosp,
  data       = df_dm,
  missing    = "FIML",  # Handle NA in informal_share
  estimator  = "MLR",   # Robust (Huber-White sandwich SEs)
  se         = "bootstrap",
  bootstrap  = N_BOOT
)

cat("\n=== Fitting serial mediation model (M1 = medical expenditure) ===\n")
fit_med <- sem(
  model_str_med,
  data      = df_dm,
  missing   = "FIML",
  estimator = "MLR",
  se        = "bootstrap",
  bootstrap = N_BOOT
)

# ------------------------------------------------------------------------------
# Extract key path estimates
# ------------------------------------------------------------------------------
extract_serial <- function(fit, m1_label) {
  pe <- parameterEstimates(fit, boot.ci.type = "bca.simple", level = 0.95)

  # Filter to defined parameters
  defined <- pe %>%
    filter(op == ":=") %>%
    mutate(
      label_clean = case_when(
        label == "chain_indirect"   ~ "Chain indirect: M1->M2",
        label == "indirect_m1_only" ~ "Indirect via M1 only",
        label == "indirect_m2_only" ~ "Indirect via M2 only",
        label == "direct_effect"    ~ "Direct effect",
        label == "total_effect"     ~ "Total effect",
        TRUE ~ label
      )
    ) %>%
    select(label_clean, est, se, ci.lower, ci.upper, pvalue)

  total <- defined$est[defined$label_clean == "Total effect"]

  defined <- defined %>%
    mutate(
      pct_total = round(est / total * 100, 1),
      m1_type   = m1_label
    )

  defined
}

results_hosp <- extract_serial(fit_hosp, "Child Hospitalization")
results_med  <- extract_serial(fit_med,  "Medical Expenditure")

cat("\n=== Serial Mediation Results (M1 = Child Hospitalization) ===\n")
print(results_hosp %>% select(label_clean, est, se, ci.lower, ci.upper, pct_total))

cat("\n=== Serial Mediation Results (M1 = Medical Expenditure) ===\n")
print(results_med %>% select(label_clean, est, se, ci.lower, ci.upper, pct_total))

# ------------------------------------------------------------------------------
# Format Table 6 (primary model: M1 = child hospitalization)
# ------------------------------------------------------------------------------
tbl6 <- results_hosp %>%
  select(
    `Effect Component` = label_clean,
    `Point Estimate`   = est,
    SE                 = se,
    `95% CI Lower`     = ci.lower,
    `95% CI Upper`     = ci.upper,
    `% of Total`       = pct_total
  ) %>%
  mutate(
    `Point Estimate` = round(`Point Estimate`, 4),
    SE               = round(SE, 4),
    `95% CI Lower`   = round(`95% CI Lower`, 4),
    `95% CI Upper`   = round(`95% CI Upper`, 4)
  ) %>%
  # Order as in paper
  slice(match(
    c("Total effect", "Direct effect",
      "Indirect via M1 only", "Indirect via M2 only", "Chain indirect: M1->M2"),
    `Effect Component`
  ))

tbl6 %>%
  kbl(
    format   = "latex",
    booktabs = TRUE,
    caption  = "Serial Mediation Analysis: Decomposing the Effect of EII on Household Income Capacity",
    label    = "tab:serial_mediation"
  ) %>%
  kable_styling(latex_options = "hold_position") %>%
  add_footnote(
    "Note: Bootstrap replications = 5,000; confidence intervals are bias-corrected (BCa).
     M1 = child hospitalization incidence (primary health shock measure);
     M2 = informal credit share. All specifications include household and year fixed effects
     (absorbed via within-transformation). *** p<0.01, ** p<0.05.",
    notation = "none"
  ) %>%
  save_kable("output/tables/table6_serial_mediation.tex")

cat("Table 6 saved to output/tables/table6_serial_mediation.tex\n")

# Save model fits
saveRDS(list(fit_hosp = fit_hosp, fit_med = fit_med),
        "data/processed/serial_mediation_fits.rds")
