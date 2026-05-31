# Data Codebook: CHFS Variable Mapping

## Dataset
**China Household Finance Survey (CHFS)**, waves 2018–2022  
Administered by: Survey and Research Center for China Household Finance, SWUFE  
Access: https://chfs.swufe.edu.cn  

**Analytical sample**: 115,430 household-year observations from 26,340 unique rural households  
**Panel structure**: Unbalanced (62% five-wave; 27% four-wave; 11% three-wave)

---

## Treatment Variable

### Environmental Infrastructure Index (EII)

A composite index summing three binary indicators (range: 0–3).

| Analysis Variable | CHFS Question / Module | Construction |
|-------------------|------------------------|--------------|
| `safe_water` | Household infrastructure module: *"What is the main source of drinking water for your household?"* | = 1 if response is piped tap water (municipal or community supply), bottled water, or purified water dispenser; = 0 if well water (open or covered), river/lake/pond, rainwater, or other unimproved source |
| `clean_energy` | Household infrastructure module: *"What is the main type of fuel used for cooking?"* | = 1 if natural gas (pipeline or bottled), electricity, or liquefied petroleum gas (LPG); = 0 if coal, wood, crop residues, animal dung, charcoal, or other solid fuel |
| `sanitation` | Household infrastructure module: *"What type of toilet facility does your household use?"* | = 1 if flush toilet (connected to sewage or septic tank) or improved pit latrine with slab; = 0 if open pit latrine, hanging latrine, open defecation, or no facility |
| `EII` | Composite | `safe_water + clean_energy + sanitation` (integer 0–3) |

**Treatment event**: An improvement event is coded when a household transitions from 0 to 1 on any binary indicator between consecutive survey waves. Once achieved, the indicator is held at 1 (absorbing state reflecting infrastructure irreversibility).

---

## Outcome Variables

### Health Outcomes (Mechanism Stage 1)

| Analysis Variable | CHFS Question / Module | Construction |
|-------------------|------------------------|--------------|
| `child_hosp` | Health module: *"In the past year, were any children in your household (under 16) hospitalized?"* | Binary: = 1 if at least one child hospitalization reported; = 0 otherwise |
| `med_exp` | Health module: *"In the past year, what were your household's total out-of-pocket medical expenditures (yuan)?"* | Continuous, in thousands of yuan (CNY); winsorized at 99th percentile |

### Credit Market Outcomes (Mechanism Stage 2)

| Analysis Variable | CHFS Question / Module | Construction |
|-------------------|------------------------|--------------|
| `total_debt` | Credit module: total outstanding debt across all sources (yuan) | Continuous |
| `informal_debt` | Credit module: debt owed to relatives, friends, village associations (hui), or informal moneylenders | Continuous |
| `informal_share` | Derived | `informal_debt / total_debt`; defined only for households with `total_debt > 0` (n = 47,832 household-years) |

> **Note**: Formal credit sources include rural commercial banks, rural credit cooperatives, Postal Savings Bank of China, and microfinance institutions. All other credit sources are classified as informal.

### Asset Portfolio Outcomes (Mechanism Stage 3)

| Analysis Variable | CHFS Question / Module | Construction |
|-------------------|------------------------|--------------|
| `income_assets` | Asset module: sum of (i) agricultural capital [livestock value + machinery value + land improvement investment + seed/fertilizer inventory], (ii) non-agricultural business assets [micro-enterprise inventory, tools, equipment], (iii) cooperative/partnership equity stakes | Continuous (yuan) |
| `total_assets` | Asset module: total household assets across all categories | Continuous (yuan) |
| `asset_share` | Derived | `income_assets / total_assets`; the primary income capacity (Household income capacity) variable |

**Non-income-generating assets** (denominator components not in numerator): liquid cash, bank deposits, durable consumption goods (furniture, appliances, vehicles for personal use), precious metals/jewelry.

---

## Control Variables

### Household Demographics

| Analysis Variable | Label | CHFS Source | Notes |
|-------------------|-------|-------------|-------|
| `hh_size` | Household size | Demographics module | Number of household members |
| `age_head` | Age of household head | Demographics module | Years |
| `age_head_sq` | Age squared | Derived | Captures non-linear age effects |
| `edu_head` | Education of household head | Demographics module | Years of schooling |
| `female_head` | Female household head | Demographics module | Binary |
| `n_children` | Number of children under 16 | Demographics module | Count |
| `n_elderly` | Number of members aged 65+ | Demographics module | Count |
| `married_head` | Head is married | Demographics module | Binary |

### Economic Status

| Analysis Variable | Label | CHFS Source | Notes |
|-------------------|-------|-------------|-------|
| `log_income` | Log household income | Income module | Natural log of total annual income (yuan) |
| `farmland` | Farmland holdings | Land module | Mu (1 mu ≈ 0.067 hectares) |
| `has_migrant` | Has migrant worker member | Labor module | Binary |
| `agri_share` | Agricultural income share | Income module | Agricultural income / total income |

### Health and Insurance

| Analysis Variable | Label | CHFS Source | Notes |
|-------------------|-------|-------------|-------|
| `chronic_illness` | Any member with chronic illness | Health module | Binary |
| `nrcms` | New Rural Cooperative Medical Scheme enrollment | Insurance module | Binary (public health insurance) |
| `insurance_other` | Commercial health insurance | Insurance module | Binary |

### Geographic and Community

| Analysis Variable | Label | CHFS Source | Notes |
|-------------------|-------|-------------|-------|
| `province` | Province code | Geographic identifier | 26 provinces/regions in CHFS rural sample |
| `county_code` | County code | Geographic identifier | Used for community-level IV construction |
| `region` | Macro-region | Derived | Eastern / Central-Western / Northeastern |
| `community_eii` | Community-average EII | Derived | Mean EII of all households in same county-year; used as instrument in IV-Probit |

### Threshold Variable

| Analysis Variable | Label | Construction |
|-------------------|-------|--------------|
| `RE` | Resource Endowment index | Composite index of: standardized baseline (2018) log income + standardized baseline total assets + social insurance coverage score (0/1 for NRCMS enrollment). Normalized to [0,1] range. Estimated threshold: 0.3318 |

---

## Descriptive Statistics (Full Sample, N = 115,430)

| Variable | Mean | SD | Min | Max |
|----------|------|----|-----|-----|
| Safe Water Access | 0.603 | 0.489 | 0 | 1 |
| Clean Energy Access | 0.527 | 0.499 | 0 | 1 |
| Sanitation Access | 0.486 | 0.500 | 0 | 1 |
| EII (0–3) | 1.616 | 1.123 | 0 | 3 |
| Child Hospitalization | 0.184 | 0.387 | 0 | 1 |
| Medical Expenditure (1,000 yuan) | 3.847 | 8.234 | 0 | 127.5 |
| Informal Credit Share | 0.612 | 0.369 | 0 | 1 |
| Income-Generating Asset Share | 0.285 | 0.312 | 0 | 1 |
| Log Household Income | 10.247 | 1.342 | 5.621 | 14.893 |

*Note*: Informal Credit Share statistics are for the debt-holding subsample (n = 47,832).

---

## Sample Construction Notes

1. **Rural restriction**: Only households classified as rural (non-agricultural hukou or residing in rural villages) are included.
2. **Attrition**: 62% of households contribute all 5 waves; 27% contribute 4 waves; 11% contribute 3 waves. Attrition is primarily due to seasonal migration and non-response.
3. **Outlier handling**: Medical expenditure winsorized at 99th percentile (127,500 yuan). Asset share variables bounded to [0, 1].
4. **Debt subsample**: For informal credit share analysis, the sample is restricted to household-years with positive total outstanding debt (n = 47,832, 41.4% of full sample). A zero-debt indicator is included as a control in full-sample specifications.

---

## File Naming Convention for Raw CHFS Data

Place downloaded CHFS files in `data/raw/` with the following expected naming:

```
data/raw/
├── chfs2018_household.dta    (or .csv / .xlsx as distributed by SWUFE)
├── chfs2019_household.dta
├── chfs2020_household.dta
├── chfs2021_household.dta
└── chfs2022_household.dta
```

The data preparation script (`01_data_preparation.R`) reads these files and harmonizes variable names across waves. Adjust file paths in the script if your CHFS files use different naming.
