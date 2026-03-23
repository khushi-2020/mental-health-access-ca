# =============================================================================
# Mental Health Care Access — California Counties
# Author: Pallavi Singh
# Data:   Two Dimensions of Access (OSF, v1.0, Jan 2026) | osf.io/6ezrx
# =============================================================================
# Place the dataset CSV in data/ before running.
# Output figures are saved to figures/ (created automatically).
# =============================================================================

library(tidyverse)
library(here)
library(ggrepel)
library(scales)

# ── Load data ──────────────────────────────────────────────────────────────────
df_raw <- read_csv(here("data", "Two Dimension of access.csv"), show_col_types = FALSE)

# ── Filter to California, drop Modoc (no providers) ───────────────────────────
df <- df_raw |>
  filter(state_abbr == "CA") |>
  filter(!is.na(AVI_county)) |>
  filter(county_name != "Modoc County")  # 0 listed providers — excluded

# ── Classify rurality (USDA UIC 2024) ─────────────────────────────────────────
df <- df |>
  mutate(
    rurality = case_when(
      UIC_2024 %in% c("1","2","3","4") ~ "Urban",
      UIC_2024 %in% c("5","6")         ~ "Micropolitan",
      TRUE                              ~ "Rural"
    ),
    rurality = factor(rurality, levels = c("Urban", "Micropolitan", "Rural"))
  )

# ── Fee burden: 10 sessions as % of annual disposable income ──────────────────
df <- df |>
  mutate(
    fee_burden_pct = (median_fee * 10 / disposable_income) * 100
  )

# ── Create output directory ────────────────────────────────────────────────────
dir.create(here("figures"), showWarnings = FALSE)

# ── Shared theme ──────────────────────────────────────────────────────────────
theme_mh <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background  = element_rect(fill = "#071c2e", color = NA),
      panel.background = element_rect(fill = "#071c2e", color = NA),
      panel.grid.major = element_line(color = "rgba(20,200,180,0.07)", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      text             = element_text(color = "#b8d4ea"),
      axis.text        = element_text(color = "#6a9cbd"),
      axis.title       = element_text(color = "#6a9cbd"),
      plot.title       = element_text(color = "#eef6ff", face = "bold", size = 15),
      plot.subtitle    = element_text(color = "#6a9cbd", size = 11),
      plot.caption     = element_text(color = "#6a9cbd", size = 9),
      legend.background = element_rect(fill = "#071c2e", color = NA),
      legend.text      = element_text(color = "#b8d4ea"),
      legend.title     = element_text(color = "#6a9cbd"),
      strip.text       = element_text(color = "#eef6ff", face = "bold")
    )
}

TEAL  <- "#14c8b4"
AMBER <- "#fbbf24"
RED   <- "#f87171"

# =============================================================================
# INSIGHT 01 — Medicaid Metro Gap
# Urban Medicaid acceptance is 10× lower than micropolitan
# =============================================================================

ins_summary <- df |>
  group_by(rurality) |>
  summarise(
    pct_insurance  = mean(pct_insurance_acceptance, na.rm = TRUE),
    pct_medicaid   = mean(pct_medicaid,             na.rm = TRUE),
    pct_sliding    = mean(pct_sliding_scale,        na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(-rurality, names_to = "type", values_to = "pct") |>
  mutate(
    type = factor(type,
      levels = c("pct_insurance","pct_medicaid","pct_sliding"),
      labels = c("Insurance Acceptance","Medicaid Acceptance","Sliding Scale")
    )
  )

p1 <- ggplot(ins_summary, aes(x = rurality, y = pct, fill = type)) +
  geom_col(position = "dodge", width = 0.65, alpha = 0.9) +
  scale_fill_manual(values = c(TEAL, RED, "#a78bfa")) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 75)) +
  labs(
    title    = "Insurance Acceptance by Rurality",
    subtitle = "Urban Medicaid acceptance (1.3%) is 10× lower than micropolitan (13.0%)",
    x = NULL, y = "% of Providers", fill = NULL,
    caption  = "Source: Two Dimensions of Access, OSF v1.0 (Jan 2026)"
  ) +
  theme_mh() +
  theme(legend.position = "bottom")

ggsave(here("figures", "01_medicaid_metro_gap.png"), p1,
       width = 9, height = 6, dpi = 180, bg = "#071c2e")

cat("✓ Figure 1: Medicaid metro gap saved\n")

# =============================================================================
# INSIGHT 02 — Affordability Paradox
# Rural counties: highest fees, lowest income
# =============================================================================

fee_summary <- df |>
  group_by(rurality) |>
  summarise(
    median_fee    = median(median_fee,         na.rm = TRUE),
    median_income = median(disposable_income,  na.rm = TRUE),
    avg_burden    = mean(fee_burden_pct,       na.rm = TRUE),
    .groups = "drop"
  )

p2 <- ggplot(df, aes(x = rurality, y = median_fee, fill = rurality)) +
  geom_boxplot(alpha = 0.8, outlier.color = "#f87171", outlier.size = 2) +
  geom_hline(yintercept = 130, linetype = "dashed", color = "#6a9cbd", linewidth = 0.6) +
  annotate("text", x = 3.4, y = 132, label = "National median ($130)",
           color = "#6a9cbd", size = 3.2, hjust = 1) +
  scale_fill_manual(values = c(TEAL, AMBER, "#4ade80")) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title    = "Session Fees by Rurality",
    subtitle = "Rural counties charge the highest median fee despite lower incomes",
    x = NULL, y = "Median Session Fee",
    caption  = "Source: Two Dimensions of Access, OSF v1.0 (Jan 2026)"
  ) +
  theme_mh() +
  theme(legend.position = "none")

ggsave(here("figures", "02_affordability_paradox.png"), p2,
       width = 9, height = 6, dpi = 180, bg = "#071c2e")

cat("✓ Figure 2: Affordability paradox saved\n")

# =============================================================================
# INSIGHT 03 — Fee Burden Scatter
# ~10% of annual income for 10 sessions in most constrained counties
# =============================================================================

top_labels <- df |>
  arrange(desc(fee_burden_pct)) |>
  slice_head(n = 8)

p3 <- ggplot(df, aes(x = disposable_income, y = fee_burden_pct, color = rurality)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_label_repel(
    data      = top_labels,
    aes(label = str_remove(county_name, " County")),
    size = 3, fill = "#0e2d44", color = "#eef6ff",
    box.padding = 0.4, max.overlaps = 10
  ) +
  scale_color_manual(values = c(TEAL, AMBER, "#4ade80")) +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Fee Burden by Disposable Income",
    subtitle = "10 sessions as % of annual disposable income — Sierra County reaches 9.7%",
    x = "Disposable Income", y = "Fee Burden (10 Sessions)",
    color    = NULL,
    caption  = "Source: Two Dimensions of Access, OSF v1.0 (Jan 2026)"
  ) +
  theme_mh() +
  theme(legend.position = "bottom")

ggsave(here("figures", "03_fee_burden_scatter.png"), p3,
       width = 10, height = 7, dpi = 180, bg = "#071c2e")

cat("✓ Figure 3: Fee burden scatter saved\n")

# =============================================================================
# INSIGHT 04 — AVI Distribution by Rurality
# Micropolitan counties have the lowest median provider density
# =============================================================================

p4 <- ggplot(df, aes(x = rurality, y = AVI_county, fill = rurality)) +
  geom_boxplot(alpha = 0.8, outlier.color = "#f87171", outlier.size = 2) +
  scale_fill_manual(values = c(TEAL, AMBER, "#4ade80")) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title    = "Provider Availability Index (AVI) by Rurality",
    subtitle = "Micropolitan counties show lowest median AVI (providers per 100K residents)",
    x = NULL, y = "AVI",
    caption  = "Source: Two Dimensions of Access, OSF v1.0 (Jan 2026)"
  ) +
  theme_mh() +
  theme(legend.position = "none")

ggsave(here("figures", "04_avi_by_rurality.png"), p4,
       width = 9, height = 6, dpi = 180, bg = "#071c2e")

cat("✓ Figure 4: AVI boxplot saved\n")

# =============================================================================
# Summary table (console)
# =============================================================================

cat("\n── Summary by rurality ─────────────────────────────────────────────\n")
df |>
  group_by(rurality) |>
  summarise(
    n_counties     = n(),
    median_fee     = median(median_fee,         na.rm = TRUE),
    median_income  = median(disposable_income,  na.rm = TRUE),
    avg_burden_pct = mean(fee_burden_pct,       na.rm = TRUE),
    avg_medicaid   = mean(pct_medicaid,         na.rm = TRUE),
    median_avi     = median(AVI_county,         na.rm = TRUE),
    .groups = "drop"
  ) |>
  print(width = Inf)

cat("\n── Session info ────────────────────────────────────────────────────\n")
sessionInfo()
