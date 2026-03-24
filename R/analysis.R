# ============================================================================
# Geographic Disparities in Mental Health Care Access Across California Counties
# Two Dimensions of Access dataset (OSF, v1.0, January 2026)
#
# Author: Pallavi Singh
# Date:   2026
#
# Findings and interpretations are solely the author's own and do not
# represent any employer or affiliated organization.
# ============================================================================

# ── 0. Dependencies ──────────────────────────────────────────────────────────
library(tidyverse)
library(scales)
library(janitor)

# Optional: spatial mapping
# library(sf)
# library(tigris)   # TIGER/Line shapefiles for CA counties


# ── 1. Load Data ─────────────────────────────────────────────────────────────
# Source: Two Dimensions of Access (OSF, v1.0, Jan 2026)
# Replace the path below with your local data file location.
# Expected columns (see OSF codebook for full variable list):
#   county, urban_rural_class, n_providers, population,
#   median_fee, disposable_income, pct_medicaid, pct_sliding_scale,
#   avi, afi

raw <- read_csv("data/two_dimensions_of_access_v1.csv") |>
  clean_names()

# ── 2. Data Cleaning & Derivations ───────────────────────────────────────────

df <- raw |>
  # Exclude Modoc County (no listed providers; micropolitan, pop. 8,491)
  filter(county != "Modoc County") |>
  mutate(
    # Urban–rural classification (recoded to 3 levels for display)
    urban_rural = case_when(
      urban_rural_class == "Urban"          ~ "Urban",
      urban_rural_class == "Micropolitan"   ~ "Micro",
      urban_rural_class == "Rural"          ~ "Rural",
      TRUE                                  ~ NA_character_
    ) |> factor(levels = c("Urban", "Micro", "Rural")),

    # 10-session fee burden as % of disposable income
    burden_10sess_pct = (median_fee * 10 / disposable_income) * 100,

    # Access Vulnerability Index (provider density per 100K residents)
    # AVI already present in source data; re-derive for transparency
    avi_derived = (n_providers / population) * 100000
  )

# Quick sanity check
stopifnot(nrow(df) == 57)


# ── 3. Summary Table (Insight 2 / Table 1) ───────────────────────────────────
# Insight 2: Fees Are High Everywhere — But Rural Counties Pay the Most

summary_by_type <- df |>
  group_by(urban_rural) |>
  summarise(
    n_counties            = n(),
    median_fee            = median(median_fee, na.rm = TRUE),
    median_disp_income    = median(disposable_income, na.rm = TRUE),
    avg_burden_10sess     = mean(burden_10sess_pct, na.rm = TRUE),
    mean_pct_medicaid     = mean(pct_medicaid, na.rm = TRUE),
    median_avi            = median(avi, na.rm = TRUE),
    total_population      = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    across(c(median_fee, median_disp_income), dollar),
    across(c(avg_burden_10sess, mean_pct_medicaid), ~paste0(round(.x, 1), "%")),
    median_avi = round(median_avi, 1),
    total_population = comma(total_population)
  )

print(summary_by_type)


# ── 4. Figure 1: Insurance Acceptance by Urban–Rural Type (Insight 1) ────────
# Insight 1: Urban Medicaid Acceptance Is Effectively Zero

ins_long <- df |>
  group_by(urban_rural) |>
  summarise(
    `Insurance Accepted`  = mean(pct_insurance,      na.rm = TRUE),
    `Medicaid Accepted`   = mean(pct_medicaid,       na.rm = TRUE),
    `Sliding Scale`       = mean(pct_sliding_scale,  na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(-urban_rural, names_to = "metric", values_to = "pct")

fig1 <- ggplot(ins_long, aes(x = urban_rural, y = pct, fill = metric)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.6) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  scale_fill_manual(
    values = c(
      "Insurance Accepted" = "#6C8EBF",
      "Medicaid Accepted"  = "#E07B4F",
      "Sliding Scale"      = "#82B366"
    )
  ) +
  labs(
    title    = "Insurance, Medicaid, and Sliding-Scale Acceptance by Urban–Rural Type",
    subtitle = "Figure 1 — Provider counties only (n = 57; Modoc excluded)",
    x        = NULL,
    y        = "Mean Acceptance Rate (%)",
    fill     = NULL,
    caption  = "Source: Two Dimensions of Access (OSF, v1.0, Jan 2026)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

print(fig1)
# ggsave("figures/fig1_insurance_acceptance.png", fig1, width = 8, height = 5, dpi = 150)


# ── 5. Figure 2: Fee Burden vs. Disposable Income (Insight 3) ────────────────
# Insight 3: In the Most Constrained Counties, 10 Sessions Cost Nearly 10%

fig2 <- ggplot(
  df,
  aes(x = disposable_income, y = burden_10sess_pct, color = urban_rural, label = county)
) +
  geom_point(size = 3, alpha = 0.85) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  scale_x_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_color_manual(values = c(Urban = "#4A6FA5", Micro = "#9C5CC0", Rural = "#C47B3A")) +
  labs(
    title    = "10-Session Therapy Burden vs. Disposable Income by County",
    subtitle = "Figure 2 — Dashed line = 5% burden threshold",
    x        = "Median Disposable Income",
    y        = "10-Session Cost as % of Disposable Income",
    color    = NULL,
    caption  = "Source: Two Dimensions of Access (OSF, v1.0, Jan 2026)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

print(fig2)
# ggsave("figures/fig2_fee_burden_scatter.png", fig2, width = 9, height = 6, dpi = 150)


# ── 6. Access Vulnerability Index Summary (Insight 4) ────────────────────────
# Insight 4: Access Deserts Cluster in the Northern Interior and Central Valley

avi_summary <- df |>
  arrange(avi) |>
  select(county, urban_rural, avi, afi, population) |>
  mutate(
    access_desert = avi <= 40,
    population    = comma(population)
  )

# Counties classified as access deserts (AVI ≤ 40)
cat("\n--- Access Desert Counties (AVI ≤ 40) ---\n")
avi_summary |> filter(access_desert) |> print(n = Inf)


# ── 7. Policy Implications Summary (Insight 5) ───────────────────────────────
# For reporting / export — mirrors the three policy cards in index.html

policy_summary <- tibble::tribble(
  ~card, ~title,                                ~headline_stat,              ~implication,
  "01",  "The Urban Medicaid Crisis",            "38.8M affected; 1.3% acceptance",
         "Medi-Cal rate reform needed to unlock urban Medicaid supply.",
  "02",  "The Rural Fee Paradox",                "$167.50 median fee; 4.9% income burden",
         "Workforce incentives must address both supply and fee accessibility.",
  "03",  "Access Deserts Are Geographic",        "AVI ≤ 40 in Northern Interior & Central Valley",
         "Overlap with agricultural communities, high poverty, limited transit."
)

print(policy_summary)


# ── 8. Export Summary CSV ─────────────────────────────────────────────────────
# Uncomment to write outputs
# write_csv(summary_by_type, "outputs/table1_summary_by_type.csv")
# write_csv(avi_summary,     "outputs/avi_county_level.csv")

message("\nAnalysis complete. Review figures and summary tables above.")
message("Source: Two Dimensions of Access (OSF, v1.0, Jan 2026)")
message("Author: Pallavi Singh | 2026")
