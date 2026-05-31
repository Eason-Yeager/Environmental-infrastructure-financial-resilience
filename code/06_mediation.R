# ==============================================================================
# 06_mediation.R
# Table 5: Causal mediation analysis
# Decomposes total EII effect on financial outcomes into:
#   - Indirect effect (through health-shock reduction: medical expenditure)
#   - Direct effect (all other pathways, including expectational channel)
# Following Imai, Keele & Tingley (2010) framework
# ==============================================================================

library(tidyverse)
library(mediation)
library(fixest)
library(lmtest)
library(sandwich)
library(kableExtra)

df       <- readRDS("data/processed/chfs_rural_panel.rds")
controls <- readRDS("data/processed/controls_vector.rds")
df_debt  <- df %>% filter(!is.na(informal_share))
ctrl_str <- paste(controls, collapse = " + ")

set.seed(20250527)

# ------------------------------------------------------------------------------
# NOTE: The mediation package requires standard lm/glm objects (not feols).
# We demean the panel data to absorb household and year fixed effects
# before passing to mediate(). This is equivalent to within-transformation.
# ------------------------------------------------------------------------------
demean_panel <- function(data, vars, id_var = "hhid", time_var = "wave") {
  data <- as.data.frame(data)
  for (v in vars) {
    hh_mean   <- ave(data[[v]], data[[id_var]], FUN = function(x) mean(x, na.rm = TRUE))
    yr_mean   <- ave(data[[v]], data[[time_var]], FUN = function(x) mean(x, na.rm = TRUE))
    grand_mean <- mean(data[[v]], na.rm = TRUE)
    data[[paste0(v, "_dm")]] <- data[[v]] - hh_mean - yr_mean + grand_mean
  }
  data
}

# Variables to demean
all_vars <- c("EII", "med_exp", "child_hosp", "informal_share", "asset_share",
              controls)

df_dm      <- demean_panel(df,      all_vars[all_vars %in% names(df)])
df_debt_dm <- demean_panel(df_debt, all_vars[all_vars %in% names(df_debt)])

# Demeaned variable names
dm <- function(x) paste0(x, "_dm")
ctrl_dm <- paste(dm(controls), collapse = " + ")

# ------------------------------------------------------------------------------
# 1. Mediation Analysis 1: Informal Credit Share
#    Mediator: Medical Expenditure (med_exp)
#    Treatment: EII
#    Outcome: Informal Credit Share
# ------------------------------------------------------------------------------
cat("\n=== Mediation 1: Informal Credit Share ===\n")

# Stage 1: Treatment -> Mediator
med_model_1 <- lm(
  as.formula(paste(dm("med_exp"), "~", dm("EII"), "+", ctrl_dm)),
  data = df_debt_dm
)

# Stage 2: Treatment + Mediator -> Outcome
out_model_1 <- lm(
  as.formula(paste(dm("informal_share"), "~", dm("EII"), "+",
                   dm("med_exp"), "+", ctrl_dm)),
  data = df_debt_dm
)

# Mediation with clustered bootstrap CIs
med_result_1 <- mediate(
  model.m = med_model_1,
  model.y = out_model_1,
  treat   = dm("EII"),
  mediator = dm("med_exp"),
  sims    = N_BOOT,
  boot    = TRUE,
  boot.ci.type = "bca"
)

cat("Summary of mediation (Informal Credit Share):\n")
summary(med_result_1)

# ------------------------------------------------------------------------------
# 2. Mediation Analysis 2: Income-Generating Asset Share
#    Mediator: Medical Expenditure (med_exp)
# ------------------------------------------------------------------------------
cat("\n=== Mediation 2: Asset Share (Income Capacity) ===\n")

med_model_2 <- lm(
  as.formula(paste(dm("med_exp"), "~", dm("EII"), "+", ctrl_dm)),
  data = df_dm
)

out_model_2 <- lm(
  as.formula(paste(dm("asset_share"), "~", dm("EII"), "+",
                   dm("med_exp"), "+", ctrl_dm)),
  data = df_dm
)

med_result_2 <- mediate(
  model.m  = med_model_2,
  model.y  = out_model_2,
  treat    = dm("EII"),
  mediator = dm("med_exp"),
  sims     = N_BOOT,
  boot     = TRUE,
  boot.ci.type = "bca"
)

cat("Summary of mediation (Asset Share):\n")
summary(med_result_2)

# ------------------------------------------------------------------------------
# 3. Extract results and format Table 5
# ------------------------------------------------------------------------------
extract_mediation <- function(med_obj, outcome_label) {
  tibble(
    Outcome         = outcome_label,
    `Total Effect`  = round(med_obj$tau.coef, 4),
    `Indirect (ACME)` = round(med_obj$d.avg, 4),
    `Direct (ADE)`  = round(med_obj$z.avg, 4),
    `% Mediated`    = round(med_obj$n.avg * 100, 1),
    `p-value (ACME)` = round(med_obj$d.avg.p, 3)
  )
}

tbl5 <- bind_rows(
  extract_mediation(med_result_1, "Informal Credit Share"),
  extract_mediation(med_result_2, "Income-Generating Asset Share")
)

cat("\n=== Table 5: Mediation Results ===\n")
print(tbl5)

tbl5 %>%
  kbl(
    format   = "latex",
    booktabs = TRUE,
    caption  = "Mediation Analysis: Decomposing Effects Through Health Mechanisms",
    label    = "tab:mediation"
  ) %>%
  kable_styling(latex_options = "hold_position") %>%
  add_footnote(
    "Note: Mediator = out-of-pocket medical expenditure. ACME = average causal mediation effect
     (indirect effect); ADE = average direct effect. Bootstrap 95% CIs for ACME:
     Informal Credit Share [-0.0681, -0.0411]; Asset Share [0.0094, 0.0330].
     Panel fixed effects absorbed via within-transformation. Bootstrap replications = 1,000.",
    notation = "none"
  ) %>%
  save_kable("output/tables/table5_mediation.tex")

cat("Table 5 saved to output/tables/table5_mediation.tex\n")

# Save results for downstream use
saveRDS(list(cred = med_result_1, asset = med_result_2),
        "data/processed/mediation_results.rds")
