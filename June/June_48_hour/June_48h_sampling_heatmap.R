### Make adjustments to the above plot (i.e. Make a "smooth gradient" using data interpolation)
## This is the best heatmap(s) I have made so far

library(tidyverse) # data manipulation and plotting packages
library(lubridate) # used for handling dates and times
library(patchwork) # combining multiple ggplot2 plots
library(viridis) # color palettes
library(akima) # interpolation

# Set working directory
setwd("/Users/keeleykuru/Documents/Kuru_Projects/Trout_Bog_EET_general/48_hour_sampling/June")

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
      breaks = seq(0, 6, by = 1), # only down to 6m depth
      limits = c(6, 0), # only down to 6m depth
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
p_pH   <- make_heatmap_clean(df_pH, "pH", "pH", "E. pH", limits = c(4, 6))
p_turb <- make_heatmap_clean(df_turb, "Turbidity", "Turbidity (FNU)", "D. Turbidity", limits = c(0, 5))


# Combine into a 2x2 grid with separate legends
june_combined_plot_heatmap <- (p_temp | p_odo ) / (p_orp | p_turb) +
  plot_layout(guides = "keep")

print(june_combined_plot_heatmap)

