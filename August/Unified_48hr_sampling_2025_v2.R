# ============================================================
# Trout Bog 48-Hour Sampling — August 2025
# Heatmaps: Turbidity, ORP, DO Profile
# With Chlorobium proxy line = turbidity maximum depth
# ============================================================

library(tidyverse)
library(lubridate)
library(patchwork)
library(akima)
library(stringr)
library(janitor)
library(sp)
library(grDevices)

# -------------------------------
# Working directory
# -------------------------------
setwd("/Users/keeleykuru/Documents/Kuru_Projects/Trout_Bog_EET_general/48_hour_sampling/August")

# -------------------------------
# Settings
# -------------------------------
start_time <- ymd_hm("2025-08-27 12:00")
end_time   <- ymd_hm("2025-08-29 12:00")
do_time    <- ymd_hm("2025-08-28 12:00")

# -------------------------------
# Helper: extract datetime from filename
# -------------------------------
extract_datetime <- function(filename) {
  pattern <- ".*_(\\d{8})_(\\d+)(am|pm)_.*\\.csv"
  m <- str_match(filename, pattern)
  if (any(is.na(m))) return(NA)
  
  date <- mdy(m[2])
  hr   <- as.integer(m[3])
  ap   <- m[4]
  
  hr24 <- ifelse(
    ap == "pm" & hr != 12, hr + 12,
    ifelse(ap == "am" & hr == 12, 0, hr)
  )
  
  as_datetime(date) + hours(hr24)
}

# -------------------------------
# Load files
# -------------------------------
files <- list.files(pattern = "\\.csv$", full.names = TRUE)
files <- files[str_detect(files, "_\\d{8}_\\d+(am|pm)_")]

df_raw <- map_df(files, function(f) {
  tryCatch({
    read_csv(f, show_col_types = FALSE) %>%
      mutate(SamplingDateTime = extract_datetime(f))
  }, error = function(e) {
    message("Reading as UTF-16: ", f)
    read_delim(
      f,
      delim = ",",
      show_col_types = FALSE,
      locale = locale(encoding = "UTF-16")
    ) %>%
      mutate(SamplingDateTime = extract_datetime(f))
  })
})

df_raw <- df_raw %>% clean_names()

message(
  "DO-related columns: ",
  paste(names(df_raw)[str_detect(tolower(names(df_raw)), "odo|do")], collapse = ", ")
)

# -------------------------------
# Clean data
# -------------------------------
df <- df_raw %>%
  filter(
    !is.na(depth),
    !is.na(sampling_date_time),
    depth >= 0
  ) %>%
  filter(!(sampling_date_time == ymd_hm("2025-08-28 16:00"))) %>%
  filter(
    is.na(turbidity) | (turbidity >= 0 & turbidity <= 100),
    is.na(orp)       | (orp >= -300 & orp <= 500)
  )

first_cast <- min(df$sampling_date_time, na.rm = TRUE)
last_cast  <- max(df$sampling_date_time, na.rm = TRUE)

message("Data spans: ", first_cast, " → ", last_cast)

# -------------------------------
# Hull function
# -------------------------------
make_hull <- function(df, var) {
  df_var <- df %>%
    filter(!is.na(.data[[var]])) %>%
    mutate(
      t_num = as.numeric(difftime(sampling_date_time, start_time, units = "hours"))
    )
  
  idx <- chull(df_var$t_num, df_var$depth)
  hull <- df_var[c(idx, idx[1]), c("t_num", "depth")]
  hull
}

hull_turb <- make_hull(df, "turbidity")
hull_orp  <- make_hull(df, "orp")

# -------------------------------
# Interpolation function
# -------------------------------
interp_var <- function(df, var, hull) {
  df_var <- df %>%
    filter(!is.na(.data[[var]]))
  
  t_num <- as.numeric(difftime(df_var$sampling_date_time, start_time, units = "hours"))
  t_len <- as.numeric(difftime(end_time, start_time, units = "hours"))
  
  grid <- interp(
    x = t_num,
    y = df_var$depth,
    z = df_var[[var]],
    xo = seq(0, t_len, length.out = 400),
    yo = seq(0, max(df_var$depth, na.rm = TRUE), length.out = 200),
    duplicate = "mean"
  )
  
  expand.grid(x = grid$x, y = grid$y) %>%
    mutate(
      sampling_date_time = start_time + dhours(x),
      depth = y,
      !!var := as.vector(grid$z),
      in_hull = point.in.polygon(x, y, hull$t_num, hull$depth) > 0
    ) %>%
    mutate(
      !!var := ifelse(in_hull, .data[[var]], NA)
    )
}

# -------------------------------
# Interpolate turbidity and ORP
# -------------------------------
df_turb <- interp_var(df, "turbidity", hull_turb)
df_orp  <- interp_var(df, "orp", hull_orp)

# -------------------------------
# Chlorobium proxy:
# smoothed depth of maximum turbidity
# -------------------------------

chlorobium_depth <- df_turb %>%
  filter(!is.na(turbidity), depth >= 0.8, depth <= 2.5) %>%  # restrict to plausible layer
  group_by(sampling_date_time) %>%
  slice_max(turbidity, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(sampling_date_time) %>%
  mutate(
    depth_smooth = predict(
      loess(depth ~ as.numeric(sampling_date_time), span = 0.25)
    )
  )

# -------------------------------
# X-axis breaks
# -------------------------------
breaks_4h <- seq(start_time, last_cast, by = "4 hours")

# -------------------------------
# Heatmap function
# -------------------------------
make_heatmap <- function(data, var, label, limits, show_y_axis = TRUE, fill_scale = NULL) {
  
  day_centers <- c(
    ymd_hm("2025-08-27 18:00"),
    ymd_hm("2025-08-28 18:00")
  )
  
  p <- ggplot(data, aes(sampling_date_time, depth, fill = .data[[var]])) +
    geom_tile() +
    scale_y_reverse(
      limits = c(6, 0),
      expand = c(0, 0)
    ) +
    scale_x_datetime(
      limits = c(start_time, last_cast),
      expand = c(0, 0),
      breaks = breaks_4h,
      labels = function(x) format(x, "%H:%M")
    ) +
    labs(fill = label, x = NULL, y = "Depth (m)") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "right",
      panel.grid = element_blank(),
      panel.border = element_rect(color = "grey30", fill = NA, linewidth = 0.5),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      legend.title = element_text(size = 11),
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
      axis.title.y = element_text(size = 14),
      plot.margin = margin(t = 5, r = 5, b = 42, l = 5)
    ) +
    coord_cartesian(clip = "off") +
    annotate(
      "text",
      x = day_centers,
      y = 6.9,
      label = c("Aug 27", "Aug 28"),
      size = 4
    )
  
  if (is.null(fill_scale)) {
    p <- p +
      scale_fill_gradientn(
        colours = c(
          "#440154", "#3b528b", "#21908d",
          "#5dc963", "#fde725", "orange", "red"
        ),
        limits = limits,
        oob = scales::squish,
        na.value = NA
      )
  } else {
    p <- p + fill_scale
  }
  
  if (!show_y_axis) {
    p <- p +
      theme(
        axis.title.y = element_blank(),
        axis.text.y  = element_blank()
      )
  }
  
  p
}

# -------------------------------
# Turbidity heatmap with Chlorobium line
# -------------------------------
p_turb_2 <- make_heatmap(
  df_turb,
  "turbidity",
  "Turbidity (FNU)",
  c(0, 4),
  show_y_axis = TRUE
) +
  ggtitle("Turbidity")

# -------------------------------
# ORP heatmap with Chlorobium line
# -------------------------------
p_orp_2 <- make_heatmap(
  df_orp,
  "orp",
  "ORP (mV)",
  c(-200, 350),
  show_y_axis = FALSE,
  fill_scale = scale_fill_gradientn(
    colours = c(
      "#440154", "#3b528b", "#21908d",
      "#5dc963", "#fde725", "orange", "red"
    ),
    limits = c(-100, 300),
    oob = scales::squish,
    na.value = NA
  )
) +
  geom_line(
    data = chlorobium_depth,
    aes(x = sampling_date_time, y = depth),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 1.2
  ) +
  ggtitle("ORP")

# -------------------------------
# DO profile
# -------------------------------
do_col <- "odo_1"

df_do <- df %>%
  filter(
    sampling_date_time == do_time,
    !is.na(.data[[do_col]]),
    .data[[do_col]] >= 0,
    .data[[do_col]] <= 20,
    depth > 0.05
  ) %>%
  arrange(depth)

deepest_depth <- max(df_do$depth, na.rm = TRUE)

if (deepest_depth < 5.9) {
  df_do <- df_do %>%
    bind_rows(
      tibble(
        depth = 6.0,
        !!do_col := 0,
        sampling_date_time = do_time
      )
    )
}

do_xlim_max <- ceiling(max(df_do[[do_col]], na.rm = TRUE) * 1.1)

p_do_2 <- ggplot(df_do, aes(x = .data[[do_col]], y = depth)) +
  geom_path(linewidth = 1.2, color = "black") +
  geom_point(size = 2, color = "black") +
  scale_y_reverse(
    limits = c(6, 0),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(0, do_xlim_max),
    expand = c(0, 0)
  ) +
  labs(
    title = "DO profile",
    x = "DO (mg/L)",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "grey30", fill = NA, linewidth = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title.y = element_blank(),
    axis.text.y  = element_blank()
  )

# -------------------------------
# Combine figure (Turb, ORP with max turb line, DO)
# -------------------------------
fig_with_DO_2 <- (p_turb_2 | p_orp_2 | p_do_2) +
  plot_layout(widths = c(1, 1, 0.6), guides = "collect") +
  plot_annotation(
    title = "Chlorobium forms a persistent layer near the redox transition",
    subtitle = "August 2025 48-hour profile; black line = turbidity maximum depth"
  )

print(fig_with_DO_2)

