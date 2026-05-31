# Environmental Infrastructure, Health Shocks, and Rural Household Economic Vulnerability: Quasi-Experimental Evidence from China

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Journal: Sustainability (MDPI)](https://img.shields.io/badge/Journal-Sustainability%20(MDPI)-blue)](https://www.mdpi.com/journal/sustainability)

## Overview

This repository contains replication code for the paper:

> **Environmental Infrastructure, Health Shocks, and Rural Household Economic Vulnerability: Quasi-Experimental Evidence from China**  
> *Sustainability*, 2025, 17, x. https://doi.org/10.3390/xxxxx

The paper employs a **staggered difference-in-differences** design to investigate how environmental infrastructure improvements (safe drinking water, clean cooking energy, sanitation) affect rural household economic vulnerability in China, operating through a sequential **health-credit-income channel**.

---

## Data

### Primary Dataset: China Household Finance Survey (CHFS)

- **Source**: Survey and Research Center for China Household Finance, Southwestern University of Finance and Economics (SWUFE)
- **Access**: Registered academic users may apply at https://chfs.swufe.edu.cn
- **Waves used**: 2018, 2019, 2020, 2021, 2022
- **Sample**: 115,430 household-year observations from 26,340 unique rural households

> **Note**: The CHFS microdata are not publicly redistributable. Researchers must apply directly to SWUFE for data access. This repository provides the complete analytic code; users must supply their own licensed copy of the CHFS data.

### Data Structure

After obtaining CHFS access, download the rural household survey files for 2018–2022. The analysis uses five main CHFS modules:

| Module | Key Variables Used |
|--------|-------------------|
| Demographics | household size, age of head, education, gender of head |
| Environmental Infrastructure | water source type, cooking fuel type, toilet facility type |
| Health | child hospitalization (yes/no), out-of-pocket medical expenditure |
| Finance & Credit | total debt, informal debt amount, asset categories |
| Income & Assets | agricultural assets, business assets, cooperative stakes, liquid assets |

See `data/codebook.md` for the complete variable mapping from CHFS questionnaire items to analysis variables.

---

## Repository Structure

```
.
├── README.md
├── LICENSE
├── data/
│   └── codebook.md              # Variable definitions and CHFS question mapping
├── code/
│   ├── 00_master.R              # Master script: runs all analyses in sequence
│   ├── 01_data_preparation.R    # Data cleaning, variable construction, panel setup
│   ├── 02_descriptive_stats.R   # Table 2: summary statistics
│   ├── 03_main_did.R            # Table 3: baseline staggered DID estimates
│   ├── 04_dimension_specific.R  # Table 4: dimension-specific treatment effects
│   ├── 05_dynamic_effects.R     # Figure 2: event-study / lead-lag specification
│   ├── 06_mediation.R           # Table 5: single-step mediation analysis
│   ├── 07_serial_mediation.R    # Table 6: serial (chain) mediation analysis
│   ├── 08_threshold_regression.R# Tables 7-8: Hansen (1999) panel threshold regression
│   ├── 09_heterogeneity.R       # Threshold effects by poverty, gender, region
│   └── 10_robustness.R          # Robustness checks, IV-Probit, Sun-Abraham
└── output/
    └── README.md
```

---

## Software Requirements

All analyses are conducted in **R** (version >= 4.2.0). Install required packages:

```r
install.packages(c(
  "tidyverse", "fixest", "did", "mediation", "lavaan",
  "MatchIt", "AER", "sandwich", "lmtest",
  "modelsummary", "ggplot2", "patchwork", "kableExtra"
))
```

---

## Replication Instructions

### Step 1: Obtain CHFS Data
Apply at https://chfs.swufe.edu.cn and place raw files in `data/raw/` (gitignored).

### Step 2: Run All Analyses
```r
source("code/00_master.R")
```
Or run scripts 01 through 10 individually in order.

### Step 3: Output
Tables are exported as `.tex` to `output/tables/`. Figures saved as `.pdf` to `output/figures/`.

---

## Key Methodology

| Component | Method | R Implementation |
|-----------|--------|-----------------|
| Main identification | Staggered TWFE DID | `fixest::feols()` |
| Timing-robust DID | Sun & Abraham (2021) | `fixest::feols()` with `sunab()` |
| Parallel trends | Event-study leads/lags | `fixest::feols()` |
| Non-linear threshold | Hansen (1999) panel threshold | Custom bootstrap function |
| Single-step mediation | Imai et al. (2010) | `mediation::mediate()` |
| Serial mediation | Hayes (2018) | `lavaan::sem()` |
| Matching | PSM within cohorts | `MatchIt::matchit()` |
| Endogeneity check | IV-Probit | `AER::ivreg()` |
| Falsification | Placebo test (+1 year shift) | `fixest::feols()` |

---

## Main Results Summary

| Outcome | Coefficient | % Change | p-value |
|---------|------------|----------|---------|
| Child hospitalization | -0.043 | -23.4% | <0.01 |
| Medical expenditure (1,000 CNY) | -1.247 | -32.4% | <0.01 |
| Informal credit share | -0.089 | -14.5% | <0.05 |
| Income-generating asset share | +0.062 | +21.8% | <0.01 |

---

## Citation

```bibtex
@article{author2025environmental,
  title   = {Environmental Infrastructure, Health Shocks, and Rural Household Economic Vulnerability},
  journal = {Sustainability},
  volume  = {17},
  year    = {2025},
  doi     = {10.3390/xxxxx}
}
```

## License
MIT License. CHFS data are not included and require separate application to SWUFE.
