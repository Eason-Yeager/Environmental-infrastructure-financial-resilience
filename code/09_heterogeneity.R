# ==============================================================================
# 09_heterogeneity.R
# Threshold effects by: (1) poverty/wealth quartile, (2) gender of head,
# (3) geographic region
# ==============================================================================

library(tidyverse)
library(fixest)
library(ggplot2)
library(patchwork)
library(kableExtra)

df       <- readRDS("data/processed/chfs_rural_panel.rds")
controls <- readRDS("data/processed/controls_vector.rds")
df_debt  <- df %>% filter(!is.na(informal_share))
ctrl_str <- paste(controls, collapse = " + ")

# ------------------------------------------------------------------------------
# Helper: Run DID and extract EII coefficient with CI
# ------------------------------------------------------------------------------
run_subgroup_did <- function(data, outcome, subgroup_label) {
  data_sub <- if ("has_debt_filter" %in% names(data) && outcome == "informal_share") {
    data %>% filter(!is.na(informal_share))
  } else {
    data
  }

  fml <- as.formula(paste(outcome, "~ EII +", ctrl_str, "| hhid + wave"))
  tryCatch({
    m <- feols(fml, data = data_sub, cluster = ~hhid)
    tibble(
      Subgroup   = subgroup_label,
      Outcome    = outcome,
      Coef       = round(coef(m)["EII"], 4),
      SE         = round(se(m)["EII"], 4),
      CI_low     = round(coef(m)["EII"] - 1.96 * se(m)["EII"], 4),
      CI_high    = round(coef(m)["EII"] + 1.96 * se(m)["EII"], 4),
      pval       = round(pvalue(m)["EII"], 3),
      N          = nobs(m)
    )
  }, error = function(e) {
    tibble(Subgroup = subgroup_label, Outcome = outcome,
           Coef = NA, SE = NA, CI_low = NA, CI_high = NA, pval = NA, N = NA)
  })
}

# ------------------------------------------------------------------------------
# 1. Threshold effects by wealth quartile (Q1 = poorest, Q4 = richest)
# ------------------------------------------------------------------------------
df <- df %>%
  mutate(
    income_q = ntile(log_income, 4),
    income_q_label = paste0("Q", income_q, " (", c("Poorest","","","Richest")[income_q], ")")
  )
df$income_q_label <- factor(df$income_q_label,
                              levels = paste0("Q", 1:4, " (", c("Poorest","","","Richest"), ")"))

quartile_results <- map_dfr(1:4, function(q) {
  sub  <- df %>% filter(income_q == q)
  lbl  <- paste0("Q", q, " (income quartile)")
  bind_rows(
    run_subgroup_did(sub, "child_hosp",     lbl),
    run_subgroup_did(sub, "med_exp",        lbl),
    run_subgroup_did(sub, "informal_share", lbl),
    run_subgroup_did(sub, "asset_share",    lbl)
  )
})

cat("\n=== Heterogeneity by Wealth Quartile ===\n")
print(quartile_results %>% filter(Outcome %in% c("informal_share", "asset_share")))

# Compute ratio Q1/Q4 for informal_share
q1_cred <- quartile_results %>% filter(Outcome == "informal_share", grepl("Q1", Subgroup)) %>% pull(Coef)
q4_cred <- quartile_results %>% filter(Outcome == "informal_share", grepl("Q4", Subgroup)) %>% pull(Coef)
cat(sprintf("Informal credit reduction ratio Q1/Q4: %.2f\n", abs(q1_cred) / abs(q4_cred)))

# ------------------------------------------------------------------------------
# 2. Threshold effects by gender of household head
# ------------------------------------------------------------------------------
gender_results <- bind_rows(
  map_dfr(c("informal_share", "asset_share"), ~ run_subgroup_did(
    df %>% filter(female_head == 1), .x, "Female head")),
  map_dfr(c("informal_share", "asset_share"), ~ run_subgroup_did(
    df %>% filter(female_head == 0), .x, "Male head"))
)

cat("\n=== Heterogeneity by Gender ===\n")
print(gender_results)

# Test interaction: female_head x EII
fml_interact_cred  <- as.formula(paste(
  "informal_share ~ EII * female_head +", ctrl_str, "| hhid + wave"))
fml_interact_asset <- as.formula(paste(
  "asset_share ~ EII * female_head +", ctrl_str, "| hhid + wave"))

m_int_cred  <- feols(fml_interact_cred,  data = df_debt, cluster = ~hhid)
m_int_asset <- feols(fml_interact_asset, data = df,      cluster = ~hhid)

cat("\nInteraction EII x female_head (informal credit):\n")
cat(sprintf("  Coefficient: %.4f, p-value: %.3f\n",
            coef(m_int_cred)["EII:female_head"],
            pvalue(m_int_cred)["EII:female_head"]))

# ------------------------------------------------------------------------------
# 3. Threshold effects by geographic region
# ------------------------------------------------------------------------------
region_results <- map_dfr(unique(df$region), function(reg) {
  sub <- df %>% filter(region == reg)
  bind_rows(
    run_subgroup_did(sub, "informal_share", reg),
    run_subgroup_did(sub, "asset_share",    reg)
  )
})

cat("\n=== Heterogeneity by Region ===\n")
print(region_results)

# ------------------------------------------------------------------------------
# 4. Export heterogeneity table
# ------------------------------------------------------------------------------
het_tbl <- bind_rows(
  quartile_results %>%
    filter(Outcome %in% c("informal_share", "asset_share")) %>%
    mutate(Category = "Wealth Quartile"),
  gender_results %>%
    mutate(Category = "Gender of Head"),
  region_results %>%
    mutate(Category = "Geographic Region")
) %>%
  mutate(
    Stars = case_when(
      pval < 0.01 ~ "***", pval < 0.05 ~ "**", pval < 0.10 ~ "*", TRUE ~ ""
    ),
    Cell = sprintf("%.4f%s\n(%.4f)", Coef, Stars, SE)
  ) %>%
  select(Category, Subgroup, Outcome, Cell) %>%
  pivot_wider(names_from = Outcome, values_from = Cell)

het_tbl %>%
  kbl(
    format   = "latex",
    booktabs = TRUE,
    caption  = "Threshold Effect Analysis by Poverty Status, Gender, and Region",
    label    = "tab:heterogeneity"
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  add_footnote(
    "Note: *** p<0.01, ** p<0.05, * p<0.1. Each cell: coefficient (SE).
     Household and year FE included. Standard errors clustered at household level.",
    notation = "none"
  ) %>%
  save_kable("output/tables/table_heterogeneity.tex")

cat("Heterogeneity table saved to output/tables/table_heterogeneity.tex\n")

# ------------------------------------------------------------------------------
# 5. Forest plot of subgroup effects
# ------------------------------------------------------------------------------
forest_data <- bind_rows(
  quartile_results,
  gender_results,
  region_results
) %>%
  filter(Outcome == "informal_share") %>%
  mutate(
    Subgroup = factor(Subgroup, levels = rev(unique(Subgroup))),
    sig = ifelse(pval < 0.05, "Significant (p<0.05)", "Not significant")
  )

forest_plot <- ggplot(forest_data, aes(x = Coef, y = Subgroup, color = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.25) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Significant (p<0.05)" = "#2166AC",
                                 "Not significant"      = "grey60")) +
  labs(
    title  = "Subgroup Effects on Informal Credit Share",
    x      = "Estimated Coefficient on EII",
    y      = NULL,
    color  = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("output/figures/figure_forest_heterogeneity.pdf",
       forest_plot, width = 8, height = 6, dpi = 300)

cat("Forest plot saved to output/figures/figure_forest_heterogeneity.pdf\n")
