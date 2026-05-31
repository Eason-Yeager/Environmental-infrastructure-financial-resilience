# ==============================================================================
# 00_master.R
# Master replication script
# "Environmental Infrastructure, Health Shocks, and Rural Household Economic
#  Vulnerability: Quasi-Experimental Evidence from China"
# Sustainability (MDPI), 2025
# ==============================================================================
# This script runs all analysis scripts in sequence.
# Estimated total runtime: ~25–40 minutes (bootstrap steps dominate).
# Run individual scripts separately for faster iteration.
# ==============================================================================

# --- 0. Set working directory -------------------------------------------------
# Assumes the working directory is the repository root.
# Adjust if needed:
# setwd("/path/to/repo")

# --- 1. Check and install required packages -----------------------------------
required_pkgs <- c(
  "tidyverse",     # Data manipulation
  "fixest",        # TWFE estimation, Sun-Abraham
  "did",           # Callaway & Sant'Anna (2021)
  "mediation",     # Imai et al. (2010) causal mediation
  "lavaan",        # Serial mediation via SEM
  "MatchIt",       # Propensity score matching
  "AER",           # IV regression
  "sandwich",      # Clustered SEs
  "lmtest",        # coeftest()
  "modelsummary",  # Regression tables
  "ggplot2",       # Figures
  "patchwork",     # Combining ggplots
  "kableExtra",    # Table formatting
  "scales",        # Plot scales
  "broom"          # tidy model outputs
)

missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}

lapply(required_pkgs, library, character.only = TRUE)

# --- 2. Create output directories ---------------------------------------------
dir.create("output/tables",  showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# --- 3. Global settings -------------------------------------------------------
set.seed(20250527)          # Reproducibility seed
N_BOOT <- 1000              # Bootstrap replications for mediation CIs
N_BOOT_THRESH <- 300        # Bootstrap replications for threshold test (Hansen 1999)
CLUSTER_VAR <- "hhid"       # Clustering unit (household ID)

# --- 4. Run scripts in order --------------------------------------------------
cat("\n========== Step 1: Data Preparation ==========\n")
source("code/01_data_preparation.R")

cat("\n========== Step 2: Descriptive Statistics ==========\n")
source("code/02_descriptive_stats.R")

cat("\n========== Step 3: Main DID Estimates ==========\n")
source("code/03_main_did.R")

cat("\n========== Step 4: Dimension-Specific Effects ==========\n")
source("code/04_dimension_specific.R")

cat("\n========== Step 5: Dynamic Event-Study ==========\n")
source("code/05_dynamic_effects.R")

cat("\n========== Step 6: Mediation Analysis ==========\n")
source("code/06_mediation.R")

cat("\n========== Step 7: Serial Mediation ==========\n")
source("code/07_serial_mediation.R")

cat("\n========== Step 8: Threshold Regression ==========\n")
source("code/08_threshold_regression.R")

cat("\n========== Step 9: Heterogeneity Analysis ==========\n")
source("code/09_heterogeneity.R")

cat("\n========== Step 10: Robustness Checks ==========\n")
source("code/10_robustness.R")

cat("\n========== All analyses complete. Outputs saved to output/ ==========\n")
