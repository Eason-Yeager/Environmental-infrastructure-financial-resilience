# ==============================================================================
# 02_descriptive_stats.R
# Table 2: Summary statistics for key variables
# ==============================================================================

library(tidyverse)
library(modelsummary)
library(kableExtra)

df <- readRDS("data/processed/chfs_rural_panel.rds")

# ------------------------------------------------------------------------------
# 1. Full-sample descriptive statistics (Table 2 in paper)
# ------------------------------------------------------------------------------
tab_vars <- c(
  "safe_water", "clean_energy", "sanitation", "EII",
  "child_hosp", "med_exp", "informal_share", "asset_share",
  "log_income"
)

tab_labels <- c(
  "Safe Water Access",
  "Clean Energy Access",
  "Sanitation Access",
  "EII (0-3)",
  "Child Hospitalization",
  "Medical Expenditure (1,000 yuan)",
  "Informal Credit Share",
  "Income-Generating Asset Share",
  "Log Household Income"
)

# Custom summary function
desc_stats <- function(x) {
  x <- x[!is.na(x)]
  c(
    Mean    = mean(x),
    `Std Dev` = sd(x),
    Min     = min(x),
    Max     = max(x),
    N       = length(x)
  )
}

tbl2 <- map_dfr(tab_vars, function(v) {
  s <- desc_stats(df[[v]])
  tibble(
    Variable = tab_labels[match(v, tab_vars)],
    Mean     = round(s["Mean"], 3),
    `Std Dev` = round(s["Std Dev"], 3),
    Min      = round(s["Min"], 3),
    Max      = round(s["Max"], 3),
    N        = formatC(as.integer(s["N"]), format = "d", big.mark = ",")
  )
})

print(tbl2)

# Export LaTeX table
tbl2 %>%
  kbl(
    format   = "latex",
    booktabs = TRUE,
    caption  = "Descriptive Statistics of Key Variables",
    label    = "tab:desc"
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_footnote(
    "Note: Informal Credit Share statistics are for the debt-holding subsample (n = 47,832).
     Medical expenditures are winsorized at the 99th percentile.",
    notation = "none"
  ) %>%
  save_kable("output/tables/table2_descriptive_stats.tex")

cat("Table 2 saved to output/tables/table2_descriptive_stats.tex\n")

# ------------------------------------------------------------------------------
# 2. Pre-treatment balance table (Table A3 in appendix)
# ------------------------------------------------------------------------------
baseline <- df %>% filter(wave == 2018)

balance_vars <- c(
  "log_income", "hh_size", "age_head", "edu_head",
  "female_head", "n_children", "farmland",
  "child_hosp", "med_exp", "informal_share", "asset_share",
  "safe_water", "clean_energy", "sanitation"
)

bal_labels <- c(
  "Log income", "Household size", "Age of head", "Education (years)",
  "Female head", "Number of children", "Farmland (mu)",
  "Child hospitalization", "Medical expenditure (1,000 yuan)",
  "Informal credit share", "Income-generating asset share",
  "Safe water access", "Clean energy access", "Sanitation access"
)

bal_tbl <- map_dfr(balance_vars, function(v) {
  s_treat   <- baseline %>% filter(ever_treated == 1) %>% pull(!!sym(v))
  s_control <- baseline %>% filter(ever_treated == 0) %>% pull(!!sym(v))
  s_treat   <- s_treat[!is.na(s_treat)]
  s_control <- s_control[!is.na(s_control)]

  tt <- tryCatch(t.test(s_treat, s_control), error = function(e) NULL)

  tibble(
    Variable    = bal_labels[match(v, balance_vars)],
    `Treated (mean)` = round(mean(s_treat), 3),
    `Control (mean)` = round(mean(s_control), 3),
    Difference  = round(mean(s_treat) - mean(s_control), 3),
    `p-value`   = if (!is.null(tt)) round(tt$p.value, 3) else NA
  )
})

bal_tbl %>%
  kbl(
    format   = "latex",
    booktabs = TRUE,
    caption  = "Baseline (2018) Characteristics by Treatment Status",
    label    = "tab:balance"
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_footnote(
    "Note: Treated = households that received at least one environmental infrastructure
     improvement during 2018-2022. p-values from two-sample t-tests.",
    notation = "none"
  ) %>%
  save_kable("output/tables/tableA3_balance.tex")

cat("Table A3 (balance) saved to output/tables/tableA3_balance.tex\n")

# ------------------------------------------------------------------------------
# 3. Infrastructure change over time
# ------------------------------------------------------------------------------
trend <- df %>%
  group_by(wave) %>%
  summarise(
    safe_water_pct  = mean(safe_water) * 100,
    clean_energy_pct = mean(clean_energy) * 100,
    sanitation_pct  = mean(sanitation) * 100,
    mean_EII        = mean(EII)
  )

cat("\nInfrastructure access by year:\n")
print(trend)
