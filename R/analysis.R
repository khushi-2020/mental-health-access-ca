# ============================================================================
# Geographic Disparities in Mental Health Care Access — California Counties
# Reproducible analysis script
#
# Data source: Two Dimensions of Access dataset (OSF, v1.0, Jan 2026)
#              Provider data from Psychology Today directory
# Author: Pallavi Singh | 2026
#
# To reproduce:
#   1. Download data from https://osf.io/6ezrx
#   2. Place CSV in data/ folder
#   3. Run this script from the project root
# ============================================================================

# ── Install packages (run once) ───────────────────────────────────────────────
# install.packages(c("tidyverse","scales","ggrepel","tigris",
#                    "sf","viridis","patchwork","gt","plotly","here"))

# ── Load packages ─────────────────────────────────────────────────────────────
library(tidyverse)
library(scales)
library(ggrepel)
library(tigris)
library(sf)
library(viridis)
library(patchwork)
library(gt)
library(plotly)
library(here)

options(tigris_use_cache = TRUE)
theme_set(theme_minimal(base_size = 13))

# ── Colour palette ────────────────────────────────────────────────────────────
pal <- c(Urban = "#2563EB", Micropolitan = "#F59E0B", Rural = "#059669")


# ============================================================================
# 0. LOAD & PREPARE DATA
# ============================================================================

DATA_PATH <- here("data", "Two Dimension of access.csv")

raw <- read_csv(DATA_PATH, show_col_types = FALSE)
cat("Full dataset:", nrow(raw), "rows x", ncol(raw), "columns\n")

# ── Filter to California ─────────────────────────────────────────────────────
ca_all <- raw |>
  filter(State == "CA") |>
  mutate(
    urban_rural = case_when(
      UIC_2024 %in% 1:4 ~ "Urban",
      UIC_2024 %in% 5:6 ~ "Micropolitan",
      UIC_2024 %in% 7:9 ~ "Rural"
    ),
    urban_rural = factor(urban_rural, levels = c("Urban", "Micropolitan", "Rural")),
    fips = str_pad(as.character(county_fips_str), 5, pad = "0"),
    fee_burden = if_else(
      !is.na(median_fee) & !is.na(disposable_income) & disposable_income > 0,
      (median_fee * 10) / disposable_income * 100,
      NA_real_
    ),
    afi_wins = AFI_county_wins_05,
    avi      = AVI_county
  )

ca <- ca_all |> filter(!is.na(median_fee))

cat("California counties (total):", nrow(ca_all), "\n")
cat("Provider counties:", nrow(ca), "\n")
cat("Excluded (no provider):", nrow(ca_all) - nrow(ca), "\n")

excluded <- ca_all |> filter(is.na(median_fee))
cat("Excluded county:", excluded$County_Name, "(pop",
    format(excluded$population, big.mark = ","), ", UIC", excluded$UIC_2024, ")\n\n")

national_median_fee <- median(raw$median_fee, na.rm = TRUE)
national_no_prov    <- mean(is.na(raw$median_fee)) * 100

cat("National median fee: $", national_median_fee, "\n")
cat("National no-provider rate:", round(national_no_prov, 1), "%\n")


# ============================================================================
# 1. INSIGHT 1 — THE AFFORDABILITY PARADOX
# ============================================================================

fee_summary <- ca |>
  group_by(urban_rural) |>
  summarise(
    n              = n(),
    median_fee     = median(median_fee, na.rm = TRUE),
    mean_fee       = mean(median_fee, na.rm = TRUE),
    median_income  = median(disposable_income, na.rm = TRUE),
    mean_burden    = mean(fee_burden, na.rm = TRUE),
    median_avi     = median(avi, na.rm = TRUE),
    mean_medicaid  = mean(pct_medicaid, na.rm = TRUE),
    mean_insurance = mean(pct_insurance_acceptance, na.rm = TRUE),
    mean_sliding   = mean(pct_sliding_scale, na.rm = TRUE),
    total_pop      = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 1)))

print(fee_summary)


# ============================================================================
# 2. INSIGHT 2 — THE MEDICAID METRO GAP
# ============================================================================

ins_summary <- ca |>
  group_by(urban_rural) |>
  summarise(
    Insurance = mean(pct_insurance_acceptance, na.rm = TRUE),
    Medicaid  = mean(pct_medicaid, na.rm = TRUE),
    Sliding   = mean(pct_sliding_scale, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 1)))

ins_long <- ins_summary |>
  pivot_longer(-urban_rural, names_to = "Metric", values_to = "pct") |>
  mutate(Metric = factor(Metric, levels = c("Insurance", "Medicaid", "Sliding")))

p_ins <- ggplot(ins_long, aes(x = urban_rural, y = pct, fill = Metric)) +
  geom_col(position = position_dodge(0.75), width = 0.65,
           colour = "white", linewidth = 0.3) +
  geom_text(
    aes(label = paste0(round(pct, 1), "%")),
    position = position_dodge(0.75), vjust = -0.4,
    size = 3.5, fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(Insurance = "#2563EB", Medicaid = "#DC2626", Sliding = "#7C3AED"),
    name = NULL
  ) +
  scale_y_continuous(labels = \(x) paste0(x, "%"), limits = c(0, 80), expand = c(0, 0)) +
  labs(
    title    = "Figure 1. Insurance, Medicaid & Sliding-Scale Acceptance",
    subtitle = "California provider counties. Urban Medicaid acceptance is 1.3%.",
    x = NULL, y = "Acceptance Rate (%)",
    caption  = "Source: Two Dimensions of Access dataset, 2024-2025"
  ) +
  theme(
    plot.title         = element_text(face = "bold"),
    plot.subtitle      = element_text(colour = "grey40"),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank()
  )

print(p_ins)


# ============================================================================
# 3. INSIGHT 3 — FEE BURDEN vs INCOME (INTERACTIVE SCATTER)
# ============================================================================

top5 <- ca |>
  filter(!is.na(fee_burden)) |>
  slice_max(fee_burden, n = 5)

p_scatter_interactive <- plot_ly(
  data = ca |> filter(!is.na(fee_burden)),
  x = ~disposable_income,
  y = ~fee_burden,
  color = ~urban_rural,
  colors = pal,
  type  = "scatter",
  mode  = "markers",
  marker = list(size = 10, opacity = 0.7),
  text = ~paste0(
    "<b>", County_Name, "</b><br>",
    "Income: $", format(round(disposable_income), big.mark = ","), "<br>",
    "Burden: ", round(fee_burden, 1), "%<br>",
    "Fee: $", round(median_fee, 0)
  ),
  hoverinfo = "text"
) |>
  add_annotations(
    data = top5,
    x    = ~disposable_income,
    y    = ~fee_burden,
    text = ~County_Name,
    showarrow  = TRUE,
    arrowhead  = 2,
    arrowsize  = 0.8,
    arrowcolor = "grey40",
    ax   = 30,
    ay   = -35,
    font = list(size = 11, color = "grey20")
  ) |>
  layout(
    title  = list(text = "Figure 2. 10-Session Therapy Burden vs. Disposable Income"),
    xaxis  = list(title = "Disposable Income ($)", tickformat = "$,.0f"),
    yaxis  = list(title = "10-Session Burden (% of Income)", ticksuffix = "%"),
    legend = list(orientation = "h", y = -0.2)
  )

print(p_scatter_interactive)


# ============================================================================
# 4. INSIGHT 4 — CHOROPLETH MAPS
# ============================================================================

ca_sf <- counties(state = "CA", cb = TRUE, resolution = "5m", year = 2022) |>
  st_transform(4326)

ca_map <- ca_sf |>
  left_join(
    ca_all |> select(fips, urban_rural, avi, afi_wins, fee_burden,
                     median_fee, disposable_income, pct_medicaid),
    by = c("GEOID" = "fips")
  ) |>
  mutate(
    has_provider = !is.na(median_fee),
    avi_plot     = if_else(has_provider, avi, NA_real_),
    afi_plot     = if_else(has_provider, afi_wins, NA_real_)
  )

p_avi <- ggplot(ca_map) +
  geom_sf(aes(fill = avi_plot), colour = "white", linewidth = 0.25) +
  scale_fill_distiller(palette = "Blues", direction = 1, na.value = "#E5E7EB",
                       name = "AVI\n(per 100K)", labels = number_format(accuracy = 1)) +
  labs(title = "Availability Index (AVI)",
       subtitle = "Darker = more providers per capita. Gray = no listed provider.") +
  theme_void(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(colour = "grey40", hjust = 0.5, size = 9))

p_afi <- ggplot(ca_map) +
  geom_sf(aes(fill = afi_plot), colour = "white", linewidth = 0.25) +
  scale_fill_distiller(palette = "Oranges", direction = 1, na.value = "#E5E7EB",
                       name = "AFI\n(% income)", labels = \(x) paste0(round(x, 1), "%")) +
  labs(title = "Affordability Index (AFI)",
       subtitle = "Darker = higher burden. Winsorised at 95th percentile.") +
  theme_void(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(colour = "grey40", hjust = 0.5, size = 9))

combined <- (p_avi + p_afi) +
  plot_annotation(
    title   = "Figure 3. California Mental Health Access by County",
    caption = "Source: Two Dimensions of Access dataset, 2024-2025",
    theme   = theme(plot.title   = element_text(face = "bold", size = 14, hjust = 0.5),
                    plot.caption = element_text(colour = "grey50"))
  )

print(combined)


# ============================================================================
# 5. INSIGHT 5 — AVI DISTRIBUTION (BOXPLOT)
# ============================================================================

p_avi_box <- ggplot(ca, aes(x = urban_rural, y = avi, fill = urban_rural)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 2, aes(colour = urban_rural)) +
  scale_fill_manual(values = pal) +
  scale_colour_manual(values = pal) +
  labs(
    title    = "Figure 4. Provider Availability by Urban-Rural Type",
    subtitle = "Micropolitan counties have the lowest median AVI.",
    x = NULL, y = "AVI (providers per 100,000)",
    caption  = "Source: Two Dimensions of Access dataset, 2024-2025"
  ) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey40"),
        legend.position = "none")

print(p_avi_box)


# ============================================================================
# EXPORT FIGURES
# ============================================================================

dir.create(here("figures"), showWarnings = FALSE)
ggsave(here("figures", "fig1_insurance_medicaid.png"),  p_ins,     width = 9,  height = 5.5, dpi = 300)
ggsave(here("figures", "fig3_maps_combined.png"),       combined,  width = 14, height = 8,   dpi = 300)
ggsave(here("figures", "fig3a_map_avi.png"),            p_avi,     width = 7,  height = 9,   dpi = 300)
ggsave(here("figures", "fig3b_map_afi.png"),            p_afi,     width = 7,  height = 9,   dpi = 300)
ggsave(here("figures", "fig4_avi_boxplot.png"),         p_avi_box, width = 8,  height = 5.5, dpi = 300)


# ============================================================================
# REPRODUCIBILITY CHECK
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("REPRODUCIBILITY CHECK\n")
cat(strrep("=", 70), "\n")
cat("CA total counties:            ", nrow(ca_all), "\n")
cat("CA provider counties:         ", nrow(ca), "\n")
cat("CA median session fee:        $", median(ca$median_fee, na.rm = TRUE), "\n")
cat("National median session fee:  $", national_median_fee, "\n")
cat("CA fee premium:               ",
    round((median(ca$median_fee, na.rm = TRUE) / national_median_fee - 1) * 100, 0), "%\n")
cat("Urban Medicaid acceptance:    ",
    round(mean(ca$pct_medicaid[ca$urban_rural == "Urban"], na.rm = TRUE), 1), "%\n")
cat("Micro Medicaid acceptance:    ",
    round(mean(ca$pct_medicaid[ca$urban_rural == "Micropolitan"], na.rm = TRUE), 1), "%\n")
cat("Max burden county:            ",
    ca |> filter(!is.na(fee_burden)) |> slice_max(fee_burden, n = 1) |> pull(County_Name),
    "(",
    ca |> filter(!is.na(fee_burden)) |> slice_max(fee_burden, n = 1) |> pull(fee_burden) |> round(1),
    "%)\n")

sessionInfo()
