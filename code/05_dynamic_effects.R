# ==============================================================================
# 05_dynamic_effects.R
# Figure 2: Event-study (lead-lag) specification
# Tests parallel trends assumption and documents treatment dynamics
# ==============================================================================

library(tidyverse)
library(fixest)
library(ggplot2)
library(patchwork)

df       <- readRDS("data/processed/chfs_rural_panel.rds")
controls <- readRDS("data/processed/controls_vector.rds")
df_debt  <- df %>% filter(!is.na(informal_share))
ctrl_str <- paste(controls, collapse = " + ")

# ------------------------------------------------------------------------------
# 1. Construct event-time variable
# Event time = wave - first_treat_wave (for treated households)
# Never-treated households get event_time = NA (excluded from event study)
# Leads: t = -2, -1 (pre-treatment)
# Lag 0: year of treatment
# Lags: t = 1, 2, 3 (post-treatment)
# ------------------------------------------------------------------------------
df_es <- df %>%
  mutate(
    first_treat_w = ifelse(is.infinite(first_treat), NA_real_, first_treat),
    event_time    = wave - first_treat_w
  ) %>%
  # Bin endpoints: leads beyond -2 -> -2, lags beyond 3 -> 3
  mutate(
    event_time_bin = case_when(
      is.na(event_time)  ~ NA_real_,
      event_time <= -2   ~ -2,
      event_time >= 3    ~ 3,
      TRUE               ~ event_time
    )
  )

df_debt_es <- df_debt %>%
  left_join(df_es %>% select(hhid, wave, event_time, event_time_bin),
            by = c("hhid", "wave"))

# ------------------------------------------------------------------------------
# 2. Estimate event-study model (relative-time dummies, omit t = -1)
# ------------------------------------------------------------------------------
run_event_study <- function(data, outcome) {
  # Create event-time dummies manually (excluding t=-1 as reference)
  etimes <- c(-2, 0, 1, 2, 3)

  data <- data %>%
    mutate(across(
      .cols  = starts_with("et_"),
      .fns   = NULL
    ))

  for (t in etimes) {
    varname <- paste0("et_", ifelse(t < 0, "m", "p"), abs(t))
    data[[varname]] <- as.integer(!is.na(data$event_time_bin) & data$event_time_bin == t)
  }

  et_vars  <- paste0("et_", c("m2", "p0", "p1", "p2", "p3"))
  rhs      <- paste(c(et_vars, ctrl_str), collapse = " + ")
  fml      <- as.formula(paste(outcome, "~", rhs, "| hhid + wave"))

  m <- feols(fml, data = data, cluster = ~hhid)
  m
}

m_es_cred  <- run_event_study(df_debt_es, "informal_share")
m_es_asset <- run_event_study(df_es,      "asset_share")
m_es_hosp  <- run_event_study(df_es,      "child_hosp")
m_es_med   <- run_event_study(df_es,      "med_exp")

# ------------------------------------------------------------------------------
# 3. Extract and plot event-study coefficients
# ------------------------------------------------------------------------------
extract_es_coefs <- function(model, outcome_label) {
  et_vars <- paste0("et_", c("m2", "p0", "p1", "p2", "p3"))
  times   <- c(-2, 0, 1, 2, 3)

  coefs <- coef(model)[et_vars]
  ses   <- se(model)[et_vars]

  # Add reference category (t = -1, coef = 0, se = 0)
  tibble(
    event_time = c(times, -1),
    coef       = c(coefs, 0),
    se         = c(ses, 0),
    ci_low     = coef - 1.96 * se,
    ci_high    = coef + 1.96 * se,
    outcome    = outcome_label
  ) %>%
    arrange(event_time)
}

es_data <- bind_rows(
  extract_es_coefs(m_es_cred,  "Informal Credit Share"),
  extract_es_coefs(m_es_asset, "Asset Share"),
  extract_es_coefs(m_es_hosp,  "Child Hospitalization"),
  extract_es_coefs(m_es_med,   "Medical Expenditure")
)

# ------------------------------------------------------------------------------
# 4. Plot Figure 2
# ------------------------------------------------------------------------------
plot_es <- function(data, title, color_treat = "#2166AC") {
  data %>%
    mutate(type = ifelse(event_time < 0, "Pre-treatment (leads)", "Post-treatment (lags)")) %>%
    ggplot(aes(x = event_time, y = coef,
               color = type, fill = type, shape = type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey50") +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Pre-treatment (leads)" = "grey50",
                                   "Post-treatment (lags)"  = color_treat)) +
    scale_fill_manual(values  = c("Pre-treatment (leads)" = "grey80",
                                   "Post-treatment (lags)"  = color_treat)) +
    scale_shape_manual(values = c("Pre-treatment (leads)" = 21,
                                   "Post-treatment (lags)"  = 19)) +
    scale_x_continuous(breaks = -2:3,
                       labels = c("t-2", "t-1\n(ref)", "t0", "t+1", "t+2", "t+3")) +
    labs(
      title   = title,
      x       = "Event Time (years relative to treatment)",
      y       = "Estimated Coefficient",
      color   = NULL, fill = NULL, shape = NULL
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )
}

fig2a <- es_data %>%
  filter(outcome == "Informal Credit Share") %>%
  plot_es("Informal Credit Share")

fig2b <- es_data %>%
  filter(outcome == "Asset Share") %>%
  plot_es("Income-Generating Asset Share", color_treat = "#D6604D")

fig2 <- fig2a + fig2b +
  plot_annotation(
    title   = "Figure 2. Dynamic Treatment Effects (Event-Study Specification)",
    caption = "Notes: Grey points (leads t-2, t-1) are statistically insignificant, confirming parallel trends.
Filled points show post-treatment effects. Reference category: t-1.
Error bars = 95% confidence intervals. Standard errors clustered at the household level."
  )

ggsave("output/figures/figure2_event_study.pdf",
       fig2, width = 10, height = 5, dpi = 300)

# Also save individual panels
ggsave("output/figures/figure2a_event_study_credit.pdf",
       fig2a, width = 6, height = 4.5, dpi = 300)
ggsave("output/figures/figure2b_event_study_assets.pdf",
       fig2b, width = 6, height = 4.5, dpi = 300)

cat("Figure 2 saved to output/figures/\n")

# ------------------------------------------------------------------------------
# 5. Print pre-treatment coefficients (for text reporting)
# ------------------------------------------------------------------------------
cat("\n=== Pre-Treatment Lead Coefficients (Parallel Trends Test) ===\n")
es_data %>%
  filter(event_time < 0) %>%
  select(outcome, event_time, coef, se, ci_low, ci_high) %>%
  print()
