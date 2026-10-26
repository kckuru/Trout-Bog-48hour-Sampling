##################################################
### Replicating Charles' figure (Panels B + C) ###
##################################################

# ============================================
# ORP PLOTS: 48-HOUR WINDOW (Aug 27 12:00 → Aug 29 12:00)
# Depth window = 1.75 ± 0.10 m
# ============================================

# --- Libraries ---
library(tidyverse)
library(lubridate)
library(viridis)
library(patchwork)
library(grid)

# --- 1. Define time window ---
start_time <- ymd_hm("2025-08-27 12:00")
end_time   <- ymd_hm("2025-08-29 12:00")

# --- 2. Filter df_ORP to 48-hour window ---
df_ORP_48h <- df_ORP %>%
  filter(SamplingDateTime >= start_time & SamplingDateTime <= end_time)

# --- 3. Create formatted labels and ordered factors ---
df_ORP_48h <- df_ORP_48h %>%
  mutate(
    SamplingTime_label = format(SamplingDateTime, "%b-%d\n%I:%M %p"),
    SamplingTime_factor = factor(
      SamplingTime_label,
      levels = unique(SamplingTime_label[order(SamplingDateTime)])
    )
  )

# --- 4. Main ORP profile (Panel B) ---
main_orp_48h <- ggplot(df_ORP_48h, aes(x = ORP, y = Depth, group = profile_id)) +
  geom_path(aes(color = SamplingTime_factor), linewidth = 1.2, alpha = 0.7) +
  scale_color_viridis_d(option = "turbo", name = "Sampling Time") +
  scale_x_continuous(limits = c(-300, 400), expand = c(0, 0)) +
  scale_y_reverse(limits = c(7, 0), expand = c(0, 0)) +
  labs(
    x = "ORP (mV) vs Ag/AgCl",
    y = "Depth (m)",
    title = "B. ORP Profiles (Aug 27 → 29)"
  ) +
  theme_bw(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right",
    legend.key.height = unit(0.4, "cm"),
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# --- 5. Zoomed data (Panel C): mean ± SD at 1.75 ± 0.10 m ---
df_zoom_48h <- df_ORP_48h %>%
  filter(Depth >= 1.65 & Depth <= 1.85) %>%   # 1.75 ± 0.10 m
  group_by(SamplingDateTime) %>%
  summarise(
    mean_orp = mean(ORP, na.rm = TRUE),
    sd_orp   = sd(ORP, na.rm = TRUE)
  ) %>%
  mutate(
    hour = hour(SamplingDateTime),
    light_dark = if_else(hour >= 6 & hour < 20, "Light", "Dark")
  )

# --- 6. Identify dark periods ---
df_zoom_48h <- df_zoom_48h %>%
  mutate(period_id = cumsum(light_dark != lag(light_dark, default = first(light_dark))))

dark_periods_48h <- df_zoom_48h %>%
  filter(light_dark == "Dark") %>%
  group_by(period_id) %>%
  summarise(start = min(SamplingDateTime), end = max(SamplingDateTime), .groups = "drop")

# --- 7. Inset plot (Panel C) ---
inset_orp_48h <- ggplot(df_zoom_48h, aes(x = SamplingDateTime, y = mean_orp)) +
  # Shade dark periods
  geom_rect(
    data = dark_periods_48h,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey70", alpha = 0.5
  ) +
  # ORP points and error bars
  geom_line(linewidth = 1, color = "black") +
  geom_point(size = 3, color = "black") +
  geom_errorbar(
    aes(ymin = mean_orp - sd_orp, ymax = mean_orp + sd_orp),
    width = 0.1, color = "black"
  ) +
  scale_x_datetime(
    limits = c(start_time, end_time),
    date_labels = "%b-%d\n%I %p",
    name = NULL
  ) +
  scale_y_continuous(name = "ORP (mV) at 1.75 ± 0.10 m vs Ag/AgCl") +
  labs(title = "C. ORP at 1.75 ± 0.10 m in Light/Dark Periods") +
  theme_bw(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# --- 8. Combine panels ---
combined_orp_48h <- main_orp_48h + inset_orp_48h +
  plot_layout(ncol = 2, widths = c(1.2, 1.4))

# --- 9. Print final figure ---
print(combined_orp_48h)


# ============================================
# ORP PLOTS: 24-HOUR WINDOW (Aug 27 12:00 → Aug 28 12:00)
# Depth window = 1.75 ± 0.10 m
# ============================================

library(tidyverse)
library(lubridate)
library(viridis)
library(patchwork)
library(grid)

# --- 1. Define time window ---
start_time <- ymd_hm("2025-08-27 12:00")
end_time   <- ymd_hm("2025-08-28 12:00")

# --- 2. Filter df_ORP to 24-hour window ---
df_ORP_24h <- df_ORP %>%
  filter(SamplingDateTime >= start_time & SamplingDateTime <= end_time)

# --- 3. Create formatted labels and factors ---
df_ORP_24h <- df_ORP_24h %>%
  mutate(
    SamplingTime_label = format(SamplingDateTime, "%b-%d\n%I:%M %p"),
    SamplingTime_factor = factor(
      SamplingTime_label,
      levels = unique(SamplingTime_label[order(SamplingDateTime)])
    )
  )

# --- 4. Main ORP profile (Panel B) ---
main_orp_24h <- ggplot(df_ORP_24h, aes(x = ORP, y = Depth, group = profile_id)) +
  geom_path(aes(color = SamplingTime_factor), linewidth = 1.2, alpha = 0.7) +
  scale_color_viridis_d(option = "turbo", name = "Sampling Time") +
  scale_x_continuous(limits = c(-300, 400), expand = c(0, 0)) +
  scale_y_reverse(limits = c(7, 0), expand = c(0, 0)) +
  labs(
    x = "ORP (mV) vs Ag/AgCl",
    y = "Depth (m)",
    title = "B. ORP Profiles (Aug 27 → 28)"
  ) +
  theme_bw(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right",
    legend.key.height = unit(0.4, "cm"),
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# --- 5. Zoomed data (Panel C): mean ± SD at 1.75 ± 0.10 m ---
target_remove <- ymd_hm("2025-08-28 16:00")  # remove outlier if needed

df_zoom_24h <- df_ORP_24h %>%
  filter(Depth >= 1.65 & Depth <= 1.85) %>%   # 1.75 ± 0.10 m
  filter(SamplingDateTime != target_remove) %>%
  group_by(SamplingDateTime) %>%
  summarise(
    mean_orp = mean(ORP, na.rm = TRUE),
    sd_orp   = sd(ORP, na.rm = TRUE)
  ) %>%
  mutate(
    hour = hour(SamplingDateTime),
    light_dark = if_else(hour >= 6 & hour < 20, "Light", "Dark")
  )

# --- 6. Identify dark periods ---
df_zoom_24h <- df_zoom_24h %>%
  mutate(period_id = cumsum(light_dark != lag(light_dark, default = first(light_dark))))

dark_periods_24h <- df_zoom_24h %>%
  filter(light_dark == "Dark") %>%
  group_by(period_id) %>%
  summarise(start = min(SamplingDateTime), end = max(SamplingDateTime), .groups = "drop")

# --- 7. Inset plot (Panel C) ---
inset_orp_24h <- ggplot(df_zoom_24h, aes(x = SamplingDateTime, y = mean_orp)) +
  # Shade dark periods
  geom_rect(
    data = dark_periods_24h,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey70", alpha = 0.5
  ) +
  # ORP points and error bars
  geom_line(linewidth = 1, color = "black") +
  geom_point(size = 3, color = "black") +
  geom_errorbar(
    aes(ymin = mean_orp - sd_orp, ymax = mean_orp + sd_orp),
    width = 0.1, color = "black"
  ) +
  scale_x_datetime(
    limits = c(start_time, end_time),
    date_labels = "%b-%d\n%I %p",
    name = NULL
  ) +
  scale_y_continuous(name = "ORP (mV) at 1.75 ± 0.10 m vs Ag/AgCl") +
  labs(title = "C. ORP at 1.75 ± 0.10 m in Light/Dark Periods") +
  theme_bw(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# --- 8. Combine panels ---
combined_orp_24h <- main_orp_24h + inset_orp_24h +
  plot_layout(ncol = 2, widths = c(1.2, 1.4))

# --- 9. Print final figure ---
print(combined_orp_24h)


