####################################################
### Overlay Aug 28 2025 12 PM DO profiles
### Keeley vs. Shaomei
####################################################

# --------------------------------------------------
# 1. Read Keeley/JL/SH Aug 28 profile
# --------------------------------------------------

keeley_0828 <- read_csv(
  "~/Documents/Kuru_Projects/Trout_Bog_EET_general/48_hour_sampling/48hr_sampling_summer25/48hr_sampling_summer25_v2/August/BoPro_48hr_08282025_12pm_JL_SH.csv",
  show_col_types = FALSE
) %>%
  select(
    Depth,
    DO = ODO.1
  ) %>%
  filter(
    !is.na(Depth),
    !is.na(DO),
    Depth >= 0
  ) %>%
  mutate(
    Profile = "EXO"
  )


####################################################
### Shaomei Aug 28 2025 DO profile
####################################################

shaomei_raw <- read_csv(
  "~/Documents/Kuru_Projects/Trout_Bog_EET_general/48_hour_sampling/August/Shaomei_8-28-TB_12pm.csv",
  locale = locale(encoding = "Windows-1252"),
  show_col_types = FALSE
)

shaomei_0828 <- shaomei_raw %>%
  transmute(
    Depth = .[[10]],
    DO = .[[14]]
  ) %>%
  filter(
    !is.na(Depth),
    !is.na(DO),
    Depth >= 0
  ) %>%
  mutate(
    Profile = "ProDSS"
  )


keeley_0828 <- read_csv(
  "~/Documents/Kuru_Projects/Trout_Bog_EET_general/48_hour_sampling/August/BoPro_48hr_08282025_12pm_JL_SH.csv",
  show_col_types = FALSE
) %>%
  select(
    Depth,
    DO = ODO.1
  ) %>%
  filter(
    !is.na(Depth),
    !is.na(DO),
    Depth >= 0
  ) %>%
  mutate(
    Profile = "EXO"
  )


do_profiles <- bind_rows(
  keeley_0828,
  shaomei_0828
)



p_do_profiles <- ggplot(
  do_profiles,
  aes(
    x = DO,
    y = Depth,
    color = Profile
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_y_reverse(
    limits = c(7, 0),
    breaks = seq(0, 7, by = 1)
  ) +
  scale_x_continuous(
    limits = c(-1, 8),
    breaks = seq(0, 8, by = 1)
  ) +
  labs(
    title = "Dissolved Oxygen Profiles — Trout Bog Lake",
    subtitle = "August 28, 2025 | 12 PM sampling",
    x = "Dissolved Oxygen (mg/L)",
    y = "Depth (m)",
    color = "Profile"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      color = "grey30",
      fill = NA,
      linewidth = 0.5
    ),
    legend.position = "right"
  )

print(p_do_profiles)




####################################################
### Overlay Aug 18 vs. Aug 28 early-morning DO profiles
### EXO vs. ProDSS
####################################################

# --------------------------------------------------
# 1. Read Keeley's Aug 18, 4 AM profile
# --------------------------------------------------

keeley_0828_4am <- read_csv(
  "~/Documents/Kuru_Projects/Trout_Bog_EET_general/48_hour_sampling/August/BoPro_48hr_08282025_4am_ES_PH.csv",
  show_col_types = FALSE
) %>%
  select(
    Depth,
    DO = ODO.1
  ) %>%
  filter(
    !is.na(Depth),
    !is.na(DO),
    Depth >= 0
  ) %>%
  mutate(
    Profile = "EXO — Aug 18, 4 AM"
  )


# --------------------------------------------------
# 2. Read Shaomei's Aug 28, 5 AM profile
# --------------------------------------------------

shaomei_raw_5am <- read_csv(
  "~/Documents/Kuru_Projects/Trout_Bog_EET_general/48_hour_sampling/August/Shaomei_8-28-TB_5am.csv",
  locale = locale(encoding = "Windows-1252"),
  show_col_types = FALSE
)


# --------------------------------------------------
# 3. Extract Shaomei's depth + DO
# --------------------------------------------------

shaomei_0828_5am <- shaomei_raw_5am %>%
  transmute(
    Depth = .[[10]],
    DO = .[[14]]
  ) %>%
  filter(
    !is.na(Depth),
    !is.na(DO),
    Depth >= 0
  ) %>%
  mutate(
    Profile = "ProDSS — Aug 28, 5 AM"
  )


# --------------------------------------------------
# 4. Combine profiles
# --------------------------------------------------

do_profiles_morning <- bind_rows(
  keeley_0828_4am,
  shaomei_0828_5am
)


# --------------------------------------------------
# 5. Plot DO profiles
# --------------------------------------------------

p_do_profiles_morning <- ggplot(
  do_profiles_morning,
  aes(
    x = DO,
    y = Depth,
    color = Profile
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  
  scale_y_reverse(
    limits = c(7, 0),
    breaks = seq(0, 7, by = 1)
  ) +
  
  scale_x_continuous(
    limits = c(-1, 8),
    breaks = seq(0, 8, by = 1)
  ) +
  
  labs(
    title = "Dissolved Oxygen Profiles — Trout Bog Lake",
    subtitle = "Early-morning profiles: August 18, 4 AM vs. August 28, 5 AM",
    x = "Dissolved Oxygen (mg/L)",
    y = "Depth (m)",
    color = "Profile"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      color = "grey30",
      fill = NA,
      linewidth = 0.5
    ),
    legend.position = "right"
  )

print(p_do_profiles_morning)
