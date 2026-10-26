# ============================================================
# DO–ORP ANALYSIS WITH DEPTH (CLEANED + MIXED MODEL + CCF)
# ============================================================

library(tidyverse)
library(lubridate)
library(broom)
library(lme4)
library(lmerTest)
library(viridis)

# ------------------------------------------------------------
# FIX 1: Clean data (remove negative DO + missing depth)
# ------------------------------------------------------------

df_stats <- df %>%
  filter(!is.na(orp),
         !is.na(odo_1),
         !is.na(depth),
         odo_1 >= 0)   # removes anoxic sensor noise

# ------------------------------------------------------------
# FIX 2: Correlation by 0.5 m depth bins (no NA bins)
# ------------------------------------------------------------

cor_by_depth <- df_stats %>%
  mutate(depth_bin = cut(depth,
                         breaks = seq(0, 6, by = 0.5),
                         include.lowest = TRUE,
                         labels = paste0(seq(0, 5.5, by = 0.5), "–",
                                         seq(0.5, 6, by = 0.5), "m"))) %>%
  filter(!is.na(depth_bin)) %>%
  group_by(depth_bin) %>%
  summarise(
    n       = n(),
    r       = cor(odo_1, orp, use = "complete.obs"),
    p_value = cor.test(odo_1, orp)$p.value,
    .groups = "drop"
  ) %>%
  mutate(sig = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    TRUE            ~ "ns"
  ))

print(cor_by_depth)

# ------------------------------------------------------------
# FIX 3: Mixed-effects models (account for cast structure)
# ------------------------------------------------------------

# Base model
m1 <- lmer(orp ~ odo_1 + depth + (1 | sampling_date_time),
           data = df_stats)

summary(m1)

# Interaction model (does DO–ORP relationship vary with depth?)
m2 <- lmer(orp ~ odo_1 * depth + (1 | sampling_date_time),
           data = df_stats)

summary(m2)

# Model comparison
anova(m1, m2)

# ------------------------------------------------------------
# FIX 4: Clean scatter plot (no bad DO, no NA panels)
# ------------------------------------------------------------

df_stats %>%
  mutate(depth_bin = cut(depth,
                         breaks = c(0, 1, 2, 3, 4, 6),
                         labels = c("0–1m","1–2m","2–3m","3–4m","4–7m"),
                         include.lowest = TRUE)) %>%
  filter(!is.na(depth_bin)) %>%
  ggplot(aes(x = odo_1, y = orp, color = depth)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE,
              color = "black", linewidth = 0.8) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  facet_wrap(~depth_bin, scales = "free") +
  labs(x = "DO (mg/L)",
       y = "ORP (mV)",
       color = "Depth (m)",
       title = "DO vs ORP by depth layer") +
  theme_classic(base_size = 13)

# ------------------------------------------------------------
# FIX 5: Visualize correlation vs depth
# ------------------------------------------------------------

cor_by_depth %>%
  mutate(depth_mid = seq(0.25, 5.75, by = 0.5)) %>%
  ggplot(aes(x = r, y = depth_mid)) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "grey50") +
  geom_errorbarh(aes(xmin = 0, xmax = r),
                 height = 0,
                 linewidth = 0.8) +
  geom_point(aes(color = sig, size = n)) +
  scale_y_reverse(breaks = seq(0, 6, 0.5)) +
  scale_color_manual(values = c("***" = "firebrick",
                                "**"  = "orange",
                                "*"   = "gold3",
                                "ns"  = "grey60")) +
  labs(x = "Pearson r (DO vs ORP)",
       y = "Depth (m)",
       color = "Significance",
       size = "n",
       title = "DO–ORP correlation by depth") +
  theme_classic(base_size = 13)
