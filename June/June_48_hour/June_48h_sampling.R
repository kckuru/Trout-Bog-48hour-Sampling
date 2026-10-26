library(tidyverse)
library(lubridate)
library(patchwork)

# Set working directory
setwd("/Users/keeleykuru/Documents/Kuru_Projects/Trout_Bog_EET_general/48_hour_sampling/June")

# List CSV files
file_list <- list.files(pattern = "\\.csv$")

# Function to extract sampling datetime from filename like "BoPro_48hr_06042025_4pm_JM_KK.csv"
extract_datetime_from_filename <- function(filename) {
  # Extract date and time parts using regex
  # Assumes date is 8 digits (MMDDYYYY), time is number + am/pm, e.g. 4pm
  pattern <- ".*_(\\d{8})_(\\d+)(am|pm)_.*\\.csv"
  matches <- str_match(filename, pattern)
  if (any(is.na(matches))) {
    return(NA)
  }
  
  date_str <- matches[2] # Makes sure the date is correct (ex: "06042025")
  time_num <- as.integer(matches[3]) # Matches time (ex: 4)
  am_pm <- matches[4] # Matches"am" or "pm"
  
  # Convert date string to Date object
  date_parsed <- mdy(date_str)
  
  # Convert time to 24h hour time
  hour_24 <- ifelse(am_pm == "pm" & time_num != 12, time_num + 12,
                    ifelse(am_pm == "am" & time_num == 12, 0, time_num))
  
  # Build datetime as POSIXct with hour set
  datetime <- as_datetime(date_parsed) + hours(hour_24)
  
  return(datetime)
}

# Load and combine CSV files, adding a SamplingDateTime column from filename
df_all <- map_df(file_list, function(f) {
  df <- read_csv(f, show_col_types = FALSE)
  sampling_dt <- extract_datetime_from_filename(f)
  df %>% mutate(SourceFile = f, SamplingDateTime = sampling_dt)
})

# Filter out any rows missing Depth or SamplingDateTime
df_all <- df_all %>% filter(!is.na(Depth), !is.na(SamplingDateTime)) %>%
  filter(Depth >=0)

# A factor label for easier legend
df_all <- df_all %>%
  mutate(SamplingLabel = format(SamplingDateTime, "%m/%d/%Y %H:%M"))

# 13 colors to make a gradient
label_levels <- df_all %>%
  distinct(SamplingDateTime, SamplingLabel) %>%
  arrange(SamplingDateTime) %>%
  pull(SamplingLabel)

color_palette <- colorRamps::matlab.like2(length(label_levels))
names(color_palette) <- label_levels

install.packages("colorRamps")

# Panel A: ODO.1 vs Depth
p_odo_48h <- ggplot(df_all, aes(x = ODO.1, y = Depth, 
                                color = SamplingLabel, 
                                group = SamplingLabel)) +
  geom_path(size = 1) +
  scale_y_reverse() +
  scale_color_manual(values = color_palette, name = "Sampling Time") +
  labs(title = "A. ODO", x = "ODO (mg/L)", y = "Depth (m)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

print(p_odo_48h)

# Panel B: ORP vs Depth
p_orp_48h <- ggplot(df_all, aes(x = ORP, y = Depth, 
                                color = SamplingLabel, 
                                group = SamplingLabel)) +
  geom_path(size = 1) +
  scale_y_reverse() +
  scale_color_manual(values = color_palette, name = "Sampling Time") +
  labs(title = "B. ORP", x = "ORP (mV)", y = "Depth (m)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

print(p_orp_48h)

# pH vs Depth
p_pH_48h <- ggplot(df_all, aes(x = pH, y = Depth, 
                               color = SamplingLabel, 
                               group = SamplingLabel)) +
  geom_path(size = 1) +
  scale_y_reverse() +
  scale_color_manual(values = color_palette, name = "Sampling Time") +
  labs(title = "C. pH", x = "pH", y = "Depth (m)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

print(p_pH_48h)

# Temp vs Depth
p_temp_48h <- ggplot(df_all, aes(x = Temp, y = Depth, 
                                 color = SamplingLabel, 
                                 group = SamplingLabel)) +
  geom_path(size = 1) +
  scale_y_reverse() +
  scale_color_manual(values = color_palette, name = "Sampling Time") +
  labs(title = "D. Temp", x = "Temp (°C)", y = "Depth (m)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

print(p_temp_48h)

# Combine temp vs depth and ORP vs depth
combined_temp_orp_plot <- (p_temp_48h | p_orp_48h) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
# Print combined plot
print(combined_temp_orp_plot)

# Combine plots with patchwork
combined_plot <- (p_odo_48h / p_pH_48h | p_orp_48h / p_temp_48h) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
# Print combined plot
print(combined_plot)

## ---------------------------------------------------------------------- ##
  ## Attempting to color based off of light (times from 08:00 to 22:00) and dark (times between 22:00 and 08:00)
  library(tidyverse)
library(lubridate)
library(patchwork)

# Set working directory
setwd("/Users/keeleykuru/Documents/Kuru_Projects/Trout_Bog_EET/48_hour_sampling/June")

# List CSV files
file_list <- list.files(pattern = "\\.csv$")

# Function to extract sampling datetime from filename like "BoPro_48hr_06042025_4pm_JM_KK.csv"
extract_datetime_from_filename <- function(filename) {
  # Extract date and time parts using regex
  # Assumes date is 8 digits (MMDDYYYY), time is number + am/pm, e.g. 4pm
  pattern <- ".*_(\\d{8})_(\\d+)(am|pm)_.*\\.csv"
  matches <- str_match(filename, pattern)
  if (any(is.na(matches))) {
    return(NA)
  }
  
  date_str <- matches[2] # Makes sure the date is correct (ex: "06042025")
  time_num <- as.integer(matches[3]) # Matches time (ex: 4)
  am_pm <- matches[4] # Matches "am" or "pm"
  
  # Convert date string to Date object
  date_parsed <- mdy(date_str)
  
  # Convert time to 24h hour time
  hour_24 <- ifelse(am_pm == "pm" & time_num != 12, time_num + 12,
                    ifelse(am_pm == "am" & time_num == 12, 0, time_num))
  
  # Build datetime as POSIXct with hour set
  datetime <- as_datetime(date_parsed) + hours(hour_24)
  
  return(datetime)
}

# Load and combine CSVs
df_all <- map_df(file_list, function(f) {
  df <- read_csv(f, show_col_types = FALSE)
  sampling_dt <- extract_datetime_from_filename(f)
  df %>% mutate(SourceFile = f, SamplingDateTime = sampling_dt)
})

# Filter and format
df_all <- df_all %>%
  filter(!is.na(Depth), !is.na(SamplingDateTime)) %>%
  filter(Depth >= 0) %>%
  mutate(SamplingLabel = format(SamplingDateTime, "%m/%d/%Y %H:%M"),
         Hour = hour(SamplingDateTime),
         TimePeriod = if_else(Hour >= 22 | Hour < 8, "Night (22:00 - 08:00)", "Day (08:00 - 22:00)"))

# Define custom colors for day/night
time_colors <- c("Day (08:00 - 22:00)" = "#f0e442", "Night (22:00 - 08:00)" = "#0072b2")

# ODO vs Depth
p_odo_48h_lightdark <- ggplot(df_all, aes(x = ODO.1, y = Depth, 
                                          color = TimePeriod, 
                                          group = SamplingLabel)) +
  geom_path(size = 1) +
  scale_y_reverse() +
  scale_color_manual(values = time_colors, name = "Time of Day") +
  labs(title = "A. ODO", x = "ODO (mg/L)", y = "Depth (m)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

# ORP vs Depth
p_orp_48h_lightdark <- ggplot(df_all, aes(x = ORP, y = Depth, 
                                          color = TimePeriod, 
                                          group = SamplingLabel)) +
  geom_path(size = 1) +
  scale_y_reverse() +
  scale_color_manual(values = time_colors, name = "Time of Day") +
  labs(title = "B. ORP", x = "ORP (mV)", y = "Depth (m)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

# pH vs Depth
p_pH_48h_lightdark <- ggplot(df_all, aes(x = pH, y = Depth, 
                                         color = TimePeriod, 
                                         group = SamplingLabel)) +
  geom_path(size = 1) +
  scale_y_reverse() +
  scale_color_manual(values = time_colors, name = "Time of Day") +
  labs(title = "C. pH", x = "pH", y = "Depth (m)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

# Temp vs Depth
p_temp_48h_lightdark <- ggplot(df_all, aes(x = Temp, y = Depth, 
                                           color = TimePeriod, 
                                           group = SamplingLabel)) +
  geom_path(size = 1) +
  scale_y_reverse() +
  scale_color_manual(values = time_colors, name = "Time of Day") +
  labs(title = "D. Temp", x = "Temp (°C)", y = "Depth (m)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

# Combined plots
combined_temp_orp_plot_lightdark <- (p_temp_48h_lightdark | p_orp_48h_lightdark) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
combined_plot_lightdark <- (p_odo_48h_lightdark / p_pH_48h_lightdark | p_orp_48h_lightdark / p_temp_48h_lightdark) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Print plots
print(p_odo_48h_lightdark)
print(p_orp_48h_lightdark)
print(p_pH_48h_lightdark)
print(p_temp_48h_lightdark)
print(combined_temp_orp_plot_lightdark)
print(combined_plot_lightdark)

-----------------------------------------------------
  library(tidyverse)
library(lubridate)

# Filter data for 1 m ± 0.25 m depth
df_1m <- df_all %>%
  filter(Depth >= 0.75, Depth <= 1.25)

# Summarize mean and standard deviation of ORP at 1 m
df_1m_summary <- df_1m %>%
  group_by(SamplingDateTime) %>%
  summarise(
    ORP_mean = mean(ORP, na.rm = TRUE),
    ORP_sd = sd(ORP, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Define desired 4-hour sampling interval for x-axis
start_time <- ymd_hm("2025-06-04 16:00")
end_time <- ymd_hm("2025-06-06 16:00")
breaks_4h <- seq(from = start_time, to = end_time, by = "4 hours")

# Plot: ORP at 1 m depth ± 1 SD
p_orp_1m_48h <- ggplot(df_1m_summary, aes(x = SamplingDateTime, y = ORP_mean)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = ORP_mean - ORP_sd, ymax = ORP_mean + ORP_sd),
                width = 0.1, color = "blue") +
  scale_x_datetime(
    breaks = breaks_4h,
    date_labels = "%m/%d %H:%M"
  ) +
  labs(
    title = "ORP at 1 +/- 0.25m Depth Over 48-hour Sampling",
    x = "Sampling Date & Time",
    y = "ORP (mV)"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(p_orp_1m_48h)

------------------------------------------------------
  library(tidyverse)
library(lubridate)

# Filter data for 1 m ± 0.25 m depth
df_1m <- df_all %>%
  filter(Depth >= 0.75, Depth <= 1.25)

# Summarize mean and standard deviation of temp at 1 m
df_1m_summary_temp <- df_1m %>%
  group_by(SamplingDateTime) %>%
  summarise(
    temp_mean = mean(Temp, na.rm = TRUE),
    temp_sd = sd(Temp, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Define desired 4-hour sampling interval for x-axis
start_time <- ymd_hm("2025-06-04 16:00")
end_time <- ymd_hm("2025-06-06 16:00")
breaks_4h <- seq(from = start_time, to = end_time, by = "4 hours")

# Plot: temp at 1 m depth ± 1 SD
p_temp_1m_48h <- ggplot(df_1m_summary_temp, aes(x = SamplingDateTime, y = temp_mean)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = temp_mean - temp_sd, ymax = temp_mean + temp_sd),
                width = 0.1, color = "blue") +
  scale_x_datetime(
    breaks = breaks_4h,
    date_labels = "%m/%d %H:%M"
  ) +
  labs(
    title = "Temp at 1 +/- 0.25m Depth Over 48-hour Sampling",
    x = "Sampling Date & Time",
    y = "Temp (C)"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(p_temp_1m_48h)

--------------------------------------------------
  
  library(tidyverse)
library(lubridate)

# Filter data for 1 m ± 0.25 m depth
df_1m <- df_all %>%
  filter(Depth >= 0.75, Depth <= 1.25)

# Summarize mean and standard deviation of temp at 1 m
df_1m_summary_odo <- df_1m %>%
  group_by(SamplingDateTime) %>%
  summarise(
    odo_mean = mean(ODO.1, na.rm = TRUE),
    odo_sd = sd(ODO.1, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Define desired 4-hour sampling interval for x-axis
start_time <- ymd_hm("2025-06-04 16:00")
end_time <- ymd_hm("2025-06-06 16:00")
breaks_4h <- seq(from = start_time, to = end_time, by = "4 hours")

# Plot: ODO at 1 m depth ± 1 SD
p_odo_1m_48h <- ggplot(df_1m_summary_odo, aes(x = SamplingDateTime, y = odo_mean)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = odo_mean - odo_sd, ymax = odo_mean + odo_sd),
                width = 0.1, color = "blue") +
  scale_x_datetime(
    breaks = breaks_4h,
    date_labels = "%m/%d %H:%M"
  ) +
  labs(
    title = "ODO at 1 +/- 0.25m Depth Over 48-hour Sampling",
    x = "Sampling Date & Time",
    y = "ODO (mg/L)"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(p_odo_1m_48h)

-------------------------------
  ### continuous gradient heatmap (like Lau et al.) with blank spaces for missing data
  ## These heatmaps do not have interpolated data
  library(tidyverse)
library(lubridate)
library(patchwork)
library(viridis)

install.packages("viridis")

# Set working directory
setwd("/Users/keeleykuru/Documents/Kuru_Projects/Trout_Bog_EET/48_hour_sampling/June")

# List CSV files
file_list <- list.files(pattern = "\\.csv$")

# Extract Sampling DateTime from filename
extract_datetime_from_filename <- function(filename) {
  pattern <- ".*_(\\d{8})_(\\d+)(am|pm)_.*\\.csv"
  matches <- str_match(filename, pattern)
  if (any(is.na(matches))) return(NA)
  
  date_str <- matches[2]
  time_num <- as.integer(matches[3])
  am_pm <- matches[4]
  
  date_parsed <- mdy(date_str)
  
  hour_24 <- ifelse(am_pm == "pm" & time_num != 12, time_num + 12,
                    ifelse(am_pm == "am" & time_num == 12, 0, time_num))
  
  datetime <- as_datetime(date_parsed) + hours(hour_24)
  return(datetime)
}

# Load all CSVs and attach SamplingDateTime 
df_all <- map_df(file_list, function(f) {
  df <- read_csv(f, show_col_types = FALSE)
  sampling_dt <- extract_datetime_from_filename(f)
  df %>% mutate(SourceFile = f, SamplingDateTime = sampling_dt)
})

# Clean and format
df_all <- df_all %>%
  filter(!is.na(Depth), !is.na(SamplingDateTime)) %>%
  filter(Depth >= 0) %>%
  mutate(SamplingLabel = format(SamplingDateTime, "%m/%d/%Y %H:%M"))

# Define heatmap plotting function
make_heatmap_2 <- function(df, var, var_label, title = NULL, limits = NULL) {
  ggplot(df, aes(x = SamplingDateTime, y = Depth, fill = !!sym(var))) +
    geom_tile(height = 0.25) +
    scale_y_reverse(
      breaks = seq(0, 7.5, by = 1),
      limits = c(7.5, 0),
      expand = c(0, 0)
    ) +
    scale_x_datetime(
      date_breaks = "4 hours",
      date_labels = "%m/%d\n%H:%M",
      expand = c(0, 0)
    ) +
    scale_fill_gradientn(
      colours = c("#440154", "#3b528b", "#21908d", "#5dc963", "#fde725", "orange", "red"),
      name = var_label,
      limits = limits,
      oob = scales::squish,
      na.value = "grey80"
    ) +
    labs(
      title = title,
      x = "Sampling Date and Time",
      y = "Depth (m)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}
# Create individual plots (adjust limits as needed for consistency)
p_temp_2 <- make_heatmap_2(df_all, "Temp", "Temp (°C)", "A. Temperature", limits = c(4, 22))
p_odo_2  <- make_heatmap_2(df_all, "ODO.1", "DO (mg/L)", "B. Dissolved Oxygen", limits = c(0, 10))
p_orp_2  <- make_heatmap_2(df_all, "ORP", "ORP (mV)", "C. ORP", limits = c(-100, 300))
p_pH_2   <- make_heatmap_2(df_all, "pH", "pH", "D. pH", limits = c(4.5, 6.5))

# Combine into one figure
combined_plot_2 <- (p_temp_2 | p_odo_2) / (p_orp_2 | p_pH_2) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Show combined plot
print(combined_plot_2)


---------------------------------------------------------------
  ### Make adjustments to the above plot (i.e. Make a "smooth gradient" using data interpolation)
  ## This is the best heatmap(s) I have made so far
  
  library(tidyverse) # data manipulation and plotting packages
library(lubridate) # used for handling dates and times
library(patchwork) # combining multiple ggplot2 plots
library(viridis) # color palettes
library(akima) # interpolation

# Set working directory
setwd("/Users/keeleykuru/Documents/Kuru_Projects/Trout_Bog_EET/48_hour_sampling/June")

# List CSV files
file_list <- list.files(pattern = "\\.csv$")

# Extract Sampling DateTime from filename
extract_datetime_from_filename <- function(filename) {
  pattern <- ".*_(\\d{8})_(\\d+)(am|pm)_.*\\.csv"
  matches <- str_match(filename, pattern)
  if (any(is.na(matches))) return(NA)
  
  date_str <- matches[2]
  time_num <- as.integer(matches[3])
  am_pm <- matches[4]
  
  date_parsed <- mdy(date_str)
  hour_24 <- ifelse(am_pm == "pm" & time_num != 12, time_num + 12,
                    ifelse(am_pm == "am" & time_num == 12, 0, time_num))
  
  datetime <- as_datetime(date_parsed) + hours(hour_24)
  return(datetime)
}

# Load data, df_all is combined raw data from all CSV files
df_all <- map_df(file_list, function(f) {
  df <- read_csv(f, show_col_types = FALSE)
  sampling_dt <- extract_datetime_from_filename(f) # datetime is extracted from the filename
  df %>% mutate(SourceFile = f, SamplingDateTime = sampling_dt)
}) %>%
  filter(!is.na(Depth), !is.na(SamplingDateTime)) %>% # filter out rows missing depth or SamplingDateTime
  filter(Depth >= 0) # only keep rows where depth is greater than or equal to 0


### interpolation of data forcing it to fit a range, still a work in progress
interpolate_var <- function(df, varname) {
  df <- df %>% filter(!is.na(.data[[varname]])) # remove rows where varname is NA
  
  time_origin <- min(df$SamplingDateTime) 
  time_numeric <- as.numeric(difftime(df$SamplingDateTime, time_origin, units = "hours"))
  
  # Make interpolation match full range
  time_range_hours <- as.numeric(difftime(
    ymd_hm("2025-06-06 15:30"),
    ymd_hm("2025-06-04 16:00"),
    units = "hours"
  ))
  # time axis divided into 300 evenly spaced points
  time_grid <- seq(
    from = 0,
    to = time_range_hours,
    length.out = 400
  )
  # depth axis divided into 150 evenly spaced points
  depth_grid <- seq(0, max(df$Depth), length.out = 200)
  
  interp_result <- interp(
    x = time_numeric,
    y = df$Depth,
    z = df[[varname]],
    xo = time_grid,
    yo = depth_grid,
    duplicate = "mean",
    linear = TRUE,
    extrap = TRUE
  )
  
  expand.grid(x = interp_result$x, y = interp_result$y) %>%
    mutate(
      SamplingDateTime = ymd_hm("2025-06-04 15:30") + dhours(x),
      Depth = y,
      !!varname := as.vector(interp_result$z)
    ) %>%
    select(SamplingDateTime, Depth, all_of(varname))
}


# Interpolate all variables
df_temp <- interpolate_var(df_all, "Temp")
df_odo  <- interpolate_var(df_all, "ODO.1")
df_orp  <- interpolate_var(df_all, "ORP")
df_pH   <- interpolate_var(df_all, "pH")
df_turb <- interpolate_var(df_all, "Turbidity")

# Make heatmap
make_heatmap_clean <- function(df, var, var_label, title = NULL, limits = NULL) {
  time_seq <- seq(from = ymd_hm("2025-06-04 16:00"),
                  to   = ymd_hm("2025-06-06 16:00"),
                  by   = "4 hours")
  
  ggplot(df, aes(x = SamplingDateTime, y = Depth, fill = .data[[var]])) +
    geom_tile() +
    scale_y_reverse(
      breaks = seq(0, 7.3, by = 1),
      limits = c(7.3, 0),
      expand = c(0, 0)
    ) +
    scale_x_datetime(
      breaks = time_seq,
      labels = format(time_seq, "%H:%M"),
      limits = c(ymd_hm("2025-06-04 16:00"), ymd_hm("2025-06-06 16:00") + dhours(1)),
      expand = c(0, 0)
    ) +
    scale_fill_gradientn(
      colours = c("#440154", "#3b528b", "#21908d", "#5dc963", "#fde725", "orange", "red"),
      name = var_label,
      limits = limits,
      oob = scales::squish,
      na.value = "gray80"
    ) +
    labs(
      title = title,
      x = NULL,
      y = "Depth (m)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      legend.position = "right"
    )
}


# Make plots
p_temp <- make_heatmap_clean(df_temp, "Temp", "Temp (°C)", "A. Temperature", limits = c(4, 22))
p_odo  <- make_heatmap_clean(df_odo, "ODO.1", "DO (mg/L)", "B. Dissolved Oxygen", limits = c(0, 8.5))
p_orp  <- make_heatmap_clean(df_orp, "ORP", "ORP (mV)", "C. ORP", limits = c(-100, 310))
p_pH   <- make_heatmap_clean(df_pH, "pH", "pH", "D. pH", limits = c(4, 6))
p_turb <- make_heatmap_clean(df_turb, "Turbidity", "Turbidity (FNU)", "E. Turbidity", limits = c(0, 500))


# Combine into a 2x2 grid with separate legends
combined_plot_heatmap <- (p_temp | p_odo) / (p_orp | p_pH) +
  plot_layout(guides = "keep")

print(combined_plot_heatmap)


