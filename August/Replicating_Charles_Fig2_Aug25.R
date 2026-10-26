############################################################
#### Replicate line plots for DO, Turbidity, and ORP #####
############################################################

library(tidyverse)
library(lubridate)
library(patchwork)
library(akima)
library(stringr)
library(janitor)
library(sp)
library(grDevices)
library(suncalc)
library(metR) 

# Function to extract datetime from filename
Extract_Datetime_From_Filename <- function(filename) {
  base <- basename(filename)
  
  pattern <- "(\\d{2})(\\d{2})(\\d{4})_(\\d{1,2})(am|pm)"
  matches <- regmatches(base, regexec(pattern, base, ignore.case = TRUE))[[1]]
  
  if (length(matches) == 0) {
    warning(paste("No datetime found in", filename))
    return(NA)
  }
  
  month <- matches[2]
  day <- matches[3]
  year <- matches[4]
  hour <- as.numeric(matches[5])
  ampm <- matches[6]
  
  if (tolower(ampm) == "pm" && hour != 12) hour <- hour + 12
  if (tolower(ampm) == "am" && hour == 12) hour <- 0
  
  datetime <- as.POSIXct(sprintf("%s-%s-%s %02d:00:00", year, month, day, hour),
                         format="%Y-%m-%d %H:%M:%S", tz="UTC")
  return(datetime)
}

# Read CSV files
file_list <- list.files(pattern = "\\.csv$")
df_all <- map_dfr(file_list, function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  df$filename <- file
  df$SamplingDateTime <- Extract_Datetime_From_Filename(file)
  df$profile_id <- paste0(file, "_", df$SamplingDateTime)
  return(df)
})

# Arrange depths and order legend
df_all <- df_all %>%
  arrange(SamplingDateTime, Depth) %>%
  mutate(label = format(SamplingDateTime, "%b-%d %I:%M %p")) %>%
  mutate(label = factor(label, levels = unique(label[order(SamplingDateTime)])))

# Filter datasets within limits
df_DO <- df_all %>% filter(!is.na(ODO.1), ODO.1 >= -0.5 & ODO.1 <= 8)
df_Turb <- df_all %>% filter(!is.na(Turbidity), Turbidity >= 0 & Turbidity <= 5)
df_ORP <- df_all %>% filter(!is.na(ORP), ORP >= -300 & ORP <= 400)

########################################
# A. Dissolved Oxygen plot
########################################
DO_lineplot <- ggplot(df_DO, aes(x = ODO.1, y = Depth, group = profile_id, color = label)) +
  geom_path(linewidth = 1) +
  scale_x_continuous(limits = c(-0.5, 8)) +
  scale_y_reverse(limits = c(7, 0), expand = c(0, 0)) +
  labs(x = "DO (mg/L)", y = "Depth (m)", title = "A. Dissolved Oxygen") +
  theme_bw(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

########################################
# B. Turbidity plot
########################################
turb_lineplot <- ggplot(df_Turb, aes(x = Turbidity, y = Depth, group = profile_id, color = label)) +
  geom_path(linewidth = 1) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_reverse(limits = c(7, 0), expand = c(0, 0)) +
  labs(x = "Turbidity (FNU)", y = "Depth (m)", title = "B. Turbidity") +
  theme_bw(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

########################################
# C. ORP plot
########################################
ORP_lineplot <- ggplot(df_ORP, aes(x = ORP, y = Depth, group = profile_id, color = label)) +
  geom_path(linewidth = 1) +
  scale_x_continuous(limits = c(-300, 400)) +
  scale_y_reverse(limits = c(7, 0), expand = c(0, 0)) +
  labs(x = "ORP (mV)", y = "Depth (m)", title = "C. ORP") +
  theme_bw(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Print plots
print(DO_lineplot)
print(turb_lineplot)
print(ORP_lineplot)


# -- Trying to mess around with making the ORP plot look good
library(viridis)

ORP_lineplot2 <- ggplot(df_all, aes(x = ORP, y = Depth, group = profile_id, color = label)) +
  geom_path(linewidth = 1.2) +
  geom_point(size = 2, alpha = 0.7) +
  scale_x_continuous(limits = c(-300, 400)) +
  scale_y_reverse(limits = c(7, 0), expand = c(0, 0)) +
  scale_color_viridis_d(option = "plasma") +
  labs(
    x = "ORP (mV)",
    y = "Depth (m)",
    title = "C. ORP (Oxidation-Reduction Potential)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5), # changed here
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.5), # changed here
    panel.grid.minor = element_blank()
  )

ORP_lineplot2

##################################################################
#### Line plots with Dark/Light coloring (Legend unchanged) ######
##################################################################

library(tidyverse)
library(lubridate)

# Function to extract datetime from filename
Extract_Datetime_From_Filename <- function(filename) {
  base <- basename(filename)
  pattern <- "(\\d{2})(\\d{2})(\\d{4})_(\\d{1,2})(am|pm)"
  matches <- regmatches(base, regexec(pattern, base, ignore.case = TRUE))[[1]]
  
  if (length(matches) == 0) {
    warning(paste("No datetime found in", filename))
    return(NA)
  }
  
  month <- matches[2]
  day <- matches[3]
  year <- matches[4]
  hour <- as.numeric(matches[5])
  ampm <- matches[6]
  
  if (tolower(ampm) == "pm" && hour != 12) hour <- hour + 12
  if (tolower(ampm) == "am" && hour == 12) hour <- 0
  
  datetime <- as.POSIXct(sprintf("%s-%s-%s %02d:00:00", year, month, day, hour),
                         format="%Y-%m-%d %H:%M:%S", tz="UTC")
  return(datetime)
}

# Read CSV files
file_list <- list.files(pattern = "\\.csv$")
df_all <- map_dfr(file_list, function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  df$filename <- file
  df$SamplingDateTime <- Extract_Datetime_From_Filename(file)
  df$profile_id <- paste0(file, "_", df$SamplingDateTime)
  return(df)
})

# Arrange and label
df_all <- df_all %>%
  arrange(SamplingDateTime, Depth) %>%
  mutate(label = format(SamplingDateTime, "%b-%d %I:%M %p")) %>%
  mutate(label = factor(label, levels = unique(label[order(SamplingDateTime)]))) %>%
  group_by(label, SamplingDateTime) %>%
  mutate(light_dark = case_when(
    hour(SamplingDateTime) >= 20 | hour(SamplingDateTime) <= 4  ~ "Dark",
    hour(SamplingDateTime) >= 8 & hour(SamplingDateTime) <= 16  ~ "Light",
    TRUE ~ "Other"
  )) %>%
  ungroup()

# Filter datasets within limits
df_DO <- df_all %>% filter(!is.na(ODO.1), ODO.1 >= -0.5 & ODO.1 <= 8)
df_Turb <- df_all %>% filter(!is.na(Turbidity), Turbidity >= 0 & Turbidity <= 5)
df_ORP <- df_all %>% filter(!is.na(ORP), ORP >= -300 & ORP <= 400)

# Define colors
dark_light_colors <- c("Light" = "yellow", "Dark" = "darkgray", "Other" = "gray50")

# Map colors to labels
color_values <- df_all %>%
  distinct(label, light_dark) %>%
  mutate(color = ifelse(light_dark == "Light", dark_light_colors["Light"],
                        ifelse(light_dark == "Dark", dark_light_colors["Dark"], dark_light_colors["Other"]))) %>%
  pull(color) %>%
  setNames(df_all %>% distinct(label) %>% pull(label))

########################################
# A. Dissolved Oxygen plot
########################################
DO_lineplot_darklight <- ggplot(df_DO, aes(x = ODO.1, y = Depth, group = profile_id, color = label)) +
  geom_path(linewidth = 1) +
  scale_color_manual(values = color_values) +
  scale_x_continuous(limits = c(-0.5, 8)) +
  scale_y_reverse(limits = c(7, 0), expand = c(0, 0)) +
  labs(x = "DO (mg/L)", y = "Depth (m)", title = "A. Dissolved Oxygen") +
  theme_bw(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

########################################
# B. Turbidity plot
########################################
turb_lineplot_darklight <- ggplot(df_Turb, aes(x = Turbidity, y = Depth, group = profile_id, color = label)) +
  geom_path(linewidth = 1) +
  scale_color_manual(values = color_values) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_reverse(limits = c(7, 0), expand = c(0, 0)) +
  labs(x = "Turbidity (FNU)", y = "Depth (m)", title = "B. Turbidity") +
  theme_bw(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

########################################
# C. ORP plot
########################################
ORP_lineplot_darklight <- ggplot(df_ORP, aes(x = ORP, y = Depth, group = profile_id, color = label)) +
  geom_path(linewidth = 1) +
  scale_color_manual(values = color_values) +
  scale_x_continuous(limits = c(-300, 400)) +
  scale_y_reverse(limits = c(7, 0), expand = c(0, 0)) +
  labs(x = "ORP (mV)", y = "Depth (m)", title = "C. ORP") +
  theme_bw(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Print plots
print(DO_lineplot_darklight)
print(turb_lineplot_darklight)
print(ORP_lineplot_darklight)



########## NEW IDEAS 3/27/26 ##########
# -------------------------------
# PANEL B: Multi-cast ORP profiles (colored by time)
# -------------------------------
library(viridis)

df_orpprofile <- df %>%
  filter(!is.na(ORP), !is.na(Depth)) %>%
  arrange(SamplingDateTime, Depth)

# Color casts by time of day
p_B <- ggplot(df_orpprofile,
              aes(x = ORP, y = Depth,
                  group = SamplingDateTime,
                  color = as.numeric(SamplingDateTime))) +
  geom_path(alpha = 0.7, linewidth = 0.8) +
  scale_y_reverse(limits = c(6, 0), expand = c(0, 0)) +
  scale_color_viridis_c(
    option = "turbo",
    labels = function(x) format(as_datetime(x), "%b %d %H:%M")
  ) +
  labs(
    x = "ORP (mV) vs Ag/AgCl",
    y = "Depth (m)",
    color = "Sample time",
    tag = "B"
  ) +
  theme_classic(base_size = 13) +
  # Optional: red rectangle highlighting a depth of interest (e.g., 1.9–2.1m)
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = target_depth - depth_tol,
           ymax = target_depth + depth_tol,
           fill = NA, color = "red", linewidth = 0.8)
# -------------------------------
# PANEL C: ORP at target depth over time with light/dark shading
# -------------------------------

target_depth  <- 1.5   # meters — adjust to your chemocline depth
depth_tol     <- 0.25  # ± tolerance in meters

# Sunrise/sunset times for your sampling window — adjust to your location/dates
# Trout Bog, WI approximately 46°N
# -------------------------------
# LIGHT / DARK PERIODS (FULL COVERAGE)
# -------------------------------

periods <- tibble(
  xmin = c(start_time,
           ymd_hm("2025-08-27 19:45"),
           ymd_hm("2025-08-28 06:15"),
           ymd_hm("2025-08-28 19:44"),
           ymd_hm("2025-08-29 06:15")),
  
  xmax = c(ymd_hm("2025-08-27 19:45"),
           ymd_hm("2025-08-28 06:15"),
           ymd_hm("2025-08-28 19:44"),
           ymd_hm("2025-08-29 06:15"),
           end_time),
  
  period = c("Light", "Dark", "Light", "Dark", "Light")
)

# Summarize ORP near target depth across casts
df_C <- df %>%
  filter(abs(Depth - target_depth) <= depth_tol, !is.na(ORP)) %>%
  group_by(SamplingDateTime) %>%
  summarise(
    mean_ORP = mean(ORP, na.rm = TRUE),
    sd_ORP   = sd(ORP,   na.rm = TRUE),
    n        = n(),
    .groups  = "drop"
  ) %>%
  mutate(se_ORP = sd_ORP / sqrt(n))

p_C <- ggplot(df_C, aes(x = SamplingDateTime, y = mean_ORP)) +
  
  # Dark shading
  geom_rect(
    data = periods %>% filter(period == "Dark"),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey70", alpha = 0.4
  ) +
  
  # Light/Dark labels
  geom_text(
    data = periods,
    aes(x = xmin + (xmax - xmin)/2,
        y = max(df_C$mean_ORP) + 5,
        label = period),
    inherit.aes = FALSE,
    size = 3.5
  ) +
  
  # Data
  geom_errorbar(
    aes(ymin = mean_ORP - sd_ORP,
        ymax = mean_ORP + sd_ORP),
    width = 0.05
  ) +
  geom_point(size = 2.5) +
  
  scale_x_datetime(
    limits = c(start_time, end_time),
    date_labels = "%b %d\n%I %p",
    date_breaks = "4 hours",
    expand = c(0, 0)
  ) +
  
  labs(
    x = "August 27–29, 2025",
    y = paste0("ORP (mV) at ", target_depth, " m ± ", depth_tol, " m"),
    tag = "C"
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# -------------------------------
# COMBINE B + C (like Olmsted et al.)
# -------------------------------
p_B + p_C + plot_layout(widths = c(1, 1.5))




