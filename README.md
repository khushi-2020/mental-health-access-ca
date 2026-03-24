# Geographic Disparities in Mental Health Care Access Across California Counties

**Live Analysis →** [https://pallavisinghlsu.github.io/mental-health-access-ca/](https://pallavisinghlsu.github.io/mental-health-access-ca/)

---

## Overview

This repository presents an exploratory analysis of two access dimensions — **availability** (provider density per 100,000 residents) and **affordability** (session fee burden as a share of disposable income) — across 57 of California's 58 counties. The findings are intended for UX researchers in behavioral health, public sector agencies (HHS, CDPH, CDC), and research firms engaged in mental health workforce and policy analysis.

---

## Key Findings

**Insight 1 — Urban Medicaid Acceptance Is Effectively Zero**
Only 1.3% of urban California providers accept Medicaid, functionally locking out 38.8 million residents. Micropolitan counties are 10× higher (13.0%), likely because rural-market therapists cannot survive on self-pay alone.

**Insight 2 — Fees Are High Everywhere, But Rural Counties Pay the Most Per Session**
California's median session fee ($150) runs 15% above the national median ($130). Rural counties charge the highest median fee ($167.50) despite $11,500 less in disposable income than urban counties — pushing households to spend 4.9% of annual income on 10 sessions versus 3.8% in urban areas.

**Insight 3 — In the Most Constrained Counties, 10 Sessions Cost Nearly 10% of Annual Income**
Sierra County leads the burden curve at 9.7%. Counties below $25,000 in disposable income consistently spend 6–10% of annual earnings on 10 sessions.

**Insight 4 — Access Deserts Cluster in the Northern Interior and Central Valley**
The lowest Access Vulnerability Index (AVI) scores map onto agricultural communities, high poverty rates, and limited transportation infrastructure.

**Insight 5 — Policy Implications**
- **The Urban Medicaid Crisis:** Medi-Cal rate reform would disproportionately benefit the 38.8M urban residents currently locked out.
- **The Rural Fee Paradox:** Workforce incentives must address both provider supply and fee accessibility simultaneously.
- **Access Deserts Are Geographic, Not Random:** Northern Interior and Central Valley counties overlap with known provider shortage areas.

---

## Data

**Source:** *Two Dimensions of Access* dataset (OSF, v1.0, January 2026)
**Provider data:** Psychology Today public directory (collected 2024–2025)
**Coverage:** 57 of California's 58 counties
**Exclusion:** Modoc County (population 8,491; micropolitan) — no listed providers, excluded from all analyses

This directory captures listed outpatient therapists only. It excludes community health centers, unlisted providers, psychiatrists, and inpatient facilities. Counties with few observations may produce unstable estimates. Findings describe the listed-provider landscape, not total system capacity.

---

## Repository Structure

```
mental-health-access-ca/
├── index.html          # Self-contained interactive analysis (Chart.js, inline SVG maps)
├── analysis/
│   └── ca_mental_health_access.R   # Full R analysis script
├── data/               # Source data files (see OSF link)
└── README.md
```

---

## R Analysis

The `analysis/ca_mental_health_access.R` script reproduces all figures and summary statistics. Requirements: R ≥ 4.2, `tidyverse`, `sf`, `ggplot2`, `scales`.

```r
# Install dependencies
install.packages(c("tidyverse", "sf", "ggplot2", "scales", "janitor"))

# Run analysis
source("analysis/ca_mental_health_access.R")
```

---

## Citing This Work

> Singh, P. (2026). *Geographic Disparities in Mental Health Care Access Across California Counties*. Analysis based on the Two Dimensions of Access dataset (OSF, v1.0, January 2026).

---

## Disclaimer

Findings and interpretations are solely the author's own and do not represent any employer or affiliated organization.

---

*Analysis by Pallavi Singh · 2026*
