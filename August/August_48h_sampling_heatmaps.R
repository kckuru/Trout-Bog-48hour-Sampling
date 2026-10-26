####################################################
### Creating heatmaps for Aug 2025 48hr sampling ###
###################################################

## --- excluding strange sampling on 08/28 at 16:00

library(tidyverse)
library(lubridate)
library(patchwork)
library(viridis)
library(akima)
library(stringr)

# Set working directory
setwd("/Users/keeleykuru/Documents/Kuru_Projects/Trout_Bog_EET_general/48_hour_sampling/August")

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
  
  as_datetime(date_parsed) + hours(hour_24)
}

# Load data with UTF-16 fallback
df_all_aug <- map_df(file_list, function(f) {
  tryCatch({
    read_csv(f, show_col_types = FALSE) %>%
      mutate(
        SourceFile = f,
        SamplingDateTime = extract_datetime_from_filename(f)
      )
  }, error = function(e) {
    message("Reading as UTF-16: ", f)
    read_delim(
      f,
      delim = ",",
      show_col_types = FALSE,
      locale = locale(encoding = "UTF-16")
    ) %>%
      mutate(
        SourceFile = f,
        SamplingDateTime = extract_datetime_from_filename(f)
      )
  })
}) %>%
  filter(!is.na(Depth), !is.na(SamplingDateTime)) %>%
  filter(Depth >= 0) %>%
  filter(!(SamplingDateTime == ymd_hm("2025-08-28 16:00")))

# Interpolation function
interpolate_var_aug <- function(df, varname) {
  df <- df %>% filter(!is.na(.data[[varname]]))
  
  time_origin <- min(df$SamplingDateTime)
  time_numeric <- as.numeric(difftime(df$SamplingDateTime, time_origin, units = "hours"))
  
  time_range_hours <- as.numeric(difftime(
    ymd_hm("2025-08-29 12:00"),
    ymd_hm("2025-08-27 12:00"),
    units = "hours"
  ))
  
  time_grid <- seq(0, time_range_hours, length.out = 400)
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
      SamplingDateTime = ymd_hm("2025-08-27 12:00") + dhours(x),
      Depth = y,
      !!varname := as.vector(interp_result$z)
    ) %>%
    select(SamplingDateTime, Depth, all_of(varname))
}

# Interpolate variables
df_temp_aug <- interpolate_var_aug(df_all_aug, "Temp")
df_odo_aug  <- interpolate_var_aug(df_all_aug, "ODO.1")
df_orp_aug  <- interpolate_var_aug(df_all_aug, "ORP")
df_ph_aug   <- interpolate_var_aug(df_all_aug, "pH")
df_turb_aug <- interpolate_var_aug(df_all_aug, "Turbidity")

# Heatmap function
make_heatmap_aug <- function(df, var, var_label, title = NULL, limits = NULL) {
  
  time_seq <- seq(
    from = ymd_hm("2025-08-27 12:00"),
    to   = ymd_hm("2025-08-29 12:00"),
    by   = "4 hours"
  )
  
  ggplot(df, aes(x = SamplingDateTime, y = Depth, fill = .data[[var]])) +
    geom_tile() +
    scale_y_reverse(
      breaks = seq(0, 6, by = 1),
      limits = c(6, 0),
      expand = c(0, 0)
    ) +
    scale_x_datetime(
      breaks = time_seq,
      labels = format(time_seq, "%H:%M"),
      limits = c(ymd_hm("2025-08-27 12:00"), ymd_hm("2025-08-29 12:00")),
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
      panel.border = element_rect(color = "grey30", fill = NA, linewidth = 0.5),
      legend.position = "right",
      plot.margin = margin(t = 5, r = 5, b = 38, l = 5)
    ) +
    coord_cartesian(clip = "off") +
    annotate(
      "text",
      x = c(ymd_hm("2025-08-27 18:00"), ymd_hm("2025-08-28 18:00")),
      y = 6.85,
      label = c("Aug 27", "Aug 28"),
      size = 4
    )
}

# Make plots
p_temp_aug <- make_heatmap_aug(df_temp_aug, "Temp", "Temp (°C)", "A. Temperature", limits = c(4, 22))
p_odo_aug  <- make_heatmap_aug(df_odo_aug, "ODO.1", "DO (mg/L)", "B. Dissolved Oxygen", limits = c(0, 8.5))
p_orp_aug  <- make_heatmap_aug(df_orp_aug, "ORP", "ORP (mV)", "C. ORP", limits = c(-100, 310))
p_ph_aug   <- make_heatmap_aug(df_ph_aug, "pH", "pH", "E. pH", limits = c(4, 6))
p_turb_aug <- make_heatmap_aug(df_turb_aug, "Turbidity", "Turbidity (FNU)", "D. Turbidity", limits = c(0, 4))

# Combine August heatmaps
aug_combined_plot_heatmap <- (p_temp_aug | p_odo_aug ) / (p_orp_aug | p_turb_aug) +
  plot_layout(guides = "keep")

print(aug_combined_plot_heatmap)


# Comparing June and August turbidity
p_turb_june_compare <- p_turb +
  ggtitle("June 2025") +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

p_turb_aug_compare <- p_turb_aug +
  ggtitle("August 2025") +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

compare_june_aug_turb <- (p_turb_june_compare | p_turb_aug_compare) +
  plot_layout(guides = "collect")

print(compare_june_aug_turb)
