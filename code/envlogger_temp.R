# ##############################################################################

# Authored by Rui Seabra
# Modified by Hana Burdge
# To plot water temperatures recorded by envloggers to relate to kelp distribution in the study area over time

# ##############################################################################

library(tidyverse)
library (tidyr)
library(purrr)
library(dplyr)
#install.packages("devtools")
#devtools::install_github("ruiseabra/envlogger")
library(envlogger)
library(patchwork)
library(scales)
#install.packages("suncalc")
library(suncalc)
Sys.setenv(TZ = "UTC")

################################################################################

# Mapping Logger Locations 

################################################################################

library(readr)
library(leaflet)

logger_locations <- read_csv("data/logger_locations.csv")

logger_locations <- logger_locations %>%
  mutate(site_id = case_when(
    site_id == "BATI12 (BATI14)" ~ "BATI12",
    TRUE ~ site_id
  ))

logger_locations <- logger_locations %>% 
  filter(!site_id %in% c("SC5", "SC6", "SC21", "SC7", "SC20", "BATI29", "SC11", "BATI2")) %>% 
  mutate(region = case_when(
    site_id %in% c("SC3", "SC18", "SC17") ~ "1",
    site_id %in% c("SC4", "BATI12", "BATI3") ~ "2",
    site_id %in% c("SC1", "SC2", "BATI5") ~ "3",
    site_id %in% c("SC9", "BATI9", "SC10") ~ "4"
  )) 

# Set colours for regions to use in the rest of plots 
region_cols <- c(
  "1" = "#66B2FF",  # bright pastel blue
  "2" = "#FF6666",  # bright pastel red
  "3" = "lightgreen",  # bright pastel green
  "4" = "#FFB266"   # bright pastel orange
)

pal <- colorFactor(
  palette = region_cols,
  domain = logger_locations$region  # must match your data column
)

library(leaflet)
library(dplyr)

# Split data: BATI5 vs others
top_labels <- logger_locations %>% filter(site_id != "BATI5")
bottom_label <- logger_locations %>% filter(site_id == "BATI5")

# Plot map
logger_locations_map <- leaflet(logger_locations) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addCircleMarkers(
    ~lon, ~lat,
    radius = 3,
    color = ~pal(region),  # use palette function
    fillOpacity = 0.6
  ) %>%
  addLabelOnlyMarkers(
    data = top_labels,
    ~lon, ~lat,
    label = ~site_id,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "top",
      textOnly = TRUE,
      style = list("color" = "black", "font-size" = "14px", "padding" = "2px")
    )
  ) %>%
  addLabelOnlyMarkers(
    data = bottom_label,
    ~lon, ~lat,
    label = ~site_id,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "bottom",
      textOnly = TRUE,
      style = list("color" = "black", "font-size" = "14px", "padding" = "2px")
    )
  ) %>%
  addLegend(
    "topright",
    pal = pal,
    values = ~region,
    title = "Region"
  )
# Show map
logger_locations_map



mapshot(logger_locations_map, file = "figures/logger_locations_map.png", vwidth = 845, vheight = 390, zoom = 10)

################################################################################

# Cleaning Logger Data

################################################################################


FOLDER <- "data"
LAT <- 50.7457
LON <- -126.4989

# function to get sunrise and sunset times
get_dn <- function(dat, ylim) {
  day <- getSunlightTimes(
    lat  = dat$lat[1],
    lon  = dat$lon[1],
    date = dat$t %>% as.Date() %>% unique(),
    keep = c("sunrise", "sunset")) %>%
    tibble() %>%
    rename(rise = sunrise, set = sunset) %>%
    mutate(
      t = map2(rise, set, ~dat$t[between(dat$t, .x, .y)])
    ) %>%
    pull(t) %>%
    do.call(c, .)

  dn <- tibble(
    t = dat %>%
      pull(t) %>%
      unique() %>%
      sort(),
    ymin = ylim[1] - 2,
    ymax = ylim[2]
  )
  dn$ymax[(dn$t %in% day)] <- ylim[1] - 2

dn
}

# BEGIN HERE
# gather data ----
dat <- FOLDER %>%
  READ_ENV(just_rep = TRUE, approx_hourly = TRUE, full_days = TRUE) %>%
  filter(map_dbl(data, nrow) > 100) %>%
  select(-xts, -min, -max, -pressure, -overlap, -t0, -t1) %>%
  drop_na()

# the plot_env() function expects logger names to encode specific details about the microhabitats where the loggers have been deployed
# to bypass this, we can set those values to fixed values
dat <- dat %>%
  mutate(
    sh = id,
  ) %>%
  add_column(
    lvl = "m",
    exp = "h",
    mic = "mh"
  )

dat <- dat %>%
  unnest(cols = data) %>%
  add_column(
    lat = LAT,
    lon = LON
  ) %>%
  relocate(
    lat, lon, .after = sh
  )

# Add tide station names 
dat <- dat %>% 
  mutate(tide_station = case_when(
    id %in% c("sc17", "sc4", "bati3", "bati12", "sc3", "sc18", "bati12") ~ "Cedar Island",
    id %in% c("bati29", "sc9", "sc10", "bati9", "sc7", "sc20", "sc21", "sc1", "sc2") ~ "Montagu Point",
    id %in% c("sc5", "sc6") ~ "Kwatsi Bay", 
    id %in% c("sc11") ~ "Siwash Bay"
  ))

# load in tides 
library(tidyverse)
tid <- readRDS("data/tides_hana.RDS")
tid <- tid %>% 
  rename(tide_station = site) %>% 
  select(-c("lat", "lon"))

# just high tides
filter(tid, hi)
# just low tides from SCFS
filter(tid, lo, tide_station == "Salmon Coast Field Station")
# ranked from lowest low tide
filter(tid, lo, tide_station == "Salmon Coast Field Station") %>%
  arrange(h) # first 9 all in December or January
# the plot attached
tid %>%
  filter(lo) %>%
  ggplot() +
  geom_line(aes(t, h / 100)) +
  facet_grid(rows = vars(tide_station)) +
  xlab("") + ylab("height (m)") +
  ggtitle("height of low tides (relative to average sea level)")

dat_final <- dat %>%
  left_join(
    tid,
    by = c("t", "tide_station")
  )


# ------------------------------------------------------------------------------
# FUNCTION FOR PLOTTING TIDES WITH TEMP

# PLOT ----
#W <- unit(33.87, "cm") / 3
#H <- unit(19.05, "cm") / 3

# Create a function to make plotting easier 
#plot_env_tides <- function(
    #logger,
    #T10 = NULL, T11 = NULL,
    ##T20 = NULL, T21 = NULL,
    #wh = 25, DN = TRUE,
    #fn = NULL, Wmult = 0.5, Hmult = 0.9
#) 
#  {
  #filters final df for only rows specific to logger that you want to plot
  #dat_final_used <- filter(dat_final, sh == logger)
  
  # to define the start and end times of the plot 
  #t10 <- if (is.null(T10)) min(dat_final$t) else as.POSIXct(T10)
  #t11 <- if (is.null(T11)) max(dat_final$t) else as.POSIXct(T11)
  #detail <- !any(is.null(T20), is.null(T21))
  #if (detail) {
    #t20 <- as.POSIXct(T20)
    #t21 <- as.POSIXct(T21)
#  }
  
#  print(dat_final_used$t %>% range())
  
  # Keeps only rows in the timeframe that wants to be plotted 
#  dat_final_used <- filter(dat_final_used, between(t, t10, t11))
  
  # define x and y limits 
#  if (detail) xlim <- c(as.POSIXct(t20), as.POSIXct(t21))
#  ylim <- range(dat_final_used$temp)
  
  # now safe to call get_dn - for tide envelopes 
#  dn   <- get_dn(dat_final_used, ylim)
#  ylim <- dn %>% select(-t) %>% unlist() %>% range()
  
  # main plot 
#  p1 <- ggplot(dat_final_used)

#  if (detail) {
#    p1 <- p1 +
#      annotate(
#        geom = "rect",
#        xmin = xlim[1], xmax = xlim[2],
#        ymin = ylim[1], ymax = ylim[2],
#        col = "grey20", fill = NA, linewidth = 0.25
#      )
#  }

  # add main line plot - this is temp here 
#  p1 <- p1 +
#    geom_line(aes(t, temp), col = "grey10") +
#    scale_x_datetime(expand = c(0,0), labels = label_date_short()) +
#    theme_bw() +
#    theme(
#      axis.title         = element_blank(),
#      panel.grid.major.x = element_blank(),
#      plot.margin        = unit(rep(0.5, 4), "cm"),
#      strip.background   = element_blank(),
#      strip.text         = element_blank()
#    ) +
#    ylim(ylim)

#  if (detail) {
#    p2 <- dat_final_used %>%
#      filter(between(t, t20, t21)) %>%
#      ggplot()

#    if (DN) {
#      p2 <- p2 +
#        geom_ribbon(
#          data = dn,
#          mapping = aes(x = t, ymin = ymin, ymax = ymax),
#          inherit.aes = FALSE,
#          fill = "grey90", 
#          show.legend = FALSE
#        )
#    }

#    dat_final_used <- dat_final_used %>%
#      mutate(h_rescaled = scales::rescale(h, to = c(ylim[1] + 1, ylim[1] + wh)))
    
#    p2 <- p2 +
#      geom_ribbon(
#        aes(t, ymin = ylim[1], ymax = h_rescaled),
#        fill = "steelblue1", alpha = 0.5
#      ) +
#      geom_line(aes(t, temp), col = "grey10") +
#      scale_x_datetime(limits = c(t20, t21), labels = label_date_short()) +
#      theme_bw() +
#      theme(
#      panel.grid.major.x = element_blank(),
#      strip.background = element_blank(),
#        strip.text   = element_blank(),
#        axis.title   = element_blank(),
#        plot.margin  = unit(rep(0.5, 4), "cm")
#      ) +
#      ylim(ylim) +
#      coord_cartesian(expand = FALSE)
#  }

#  p <- p1
#  if (detail) p <- p1 / p2
  #p <- p + plot_layout(ncol = 1)

#  if (!is.null(fn)) {
#    ggsave(
#      file  = str_c(logger, "_", fn, ".pdf"),
#      plot  = p,
#      width = W * Wmult, height = H * Hmult)
#  }

#  print(p)
#}

## plots ----
#plot_env_tides(
#  logger = "bati3",
#  T20 = "2023-07-01", T21 = "2023-07-15",
#  fn = "1")

#plot_env_tides(
#  logger = "bati3",
#  T20 = "2024-01-06", T21 = "2024-01-23",
#  fn = "2")



# ------------------------------------------------------------------------------
# PLOTTING TEMP 

# select only necessary columns 
all_data <- select(dat_final, id, t, temp, h, lo, hi)

# all data for a given id and only during high tide
all_data_high_tide <- filter(all_data, hi)

# look at what the data looks like
ggplot(all_data_high_tide, aes(x = t, y = temp, colour = id)) +
  geom_line() +
  facet_wrap(~ id, scales = "free_y", ncol = 2)  # SC3 looks a little weird 

# Convert t to POSIXct if it isn't already
all_data_high_tide <- all_data_high_tide %>%
  mutate(
    t = as.POSIXct(t),        # convert to datetime
    date = as.Date(t)          # extract the date only
  )

# Need to make sure temp line is not drawn if logger is missed one year 
# assigning stations to region and averaging across
all_data_high_tide_region_avg <- all_data_high_tide %>% 
  filter(!id %in% c("sc5", "sc6", "sc21", "sc7", "sc20", "bati29", "sc11")) %>% 
  mutate(region = case_when(
    id %in% c("sc3", "sc18", "sc17") ~ "1",
    id %in% c("sc4", "bati12", "bati3") ~ "2",
    id %in% c("sc1", "sc2", "bati5") ~ "3",
    id %in% c("sc9", "bati9", "sc10") ~ "4"
  )) %>% 
  group_by(region, date) %>% 
  summarise(avg_temp = mean(temp, na.rm = TRUE)) 

# plotting temperature by region
library(scales)  # for date formatting
temp_time_facet <- ggplot(all_data_high_tide_region_avg) +
  geom_line(aes(date, avg_temp)) +
  facet_wrap(~ region, scales = "free_y", ncol = 1) +
  scale_x_date(
    date_labels = "%b %Y",      # label format: Month Year
    date_breaks = "1 month"     # place a tick every month
  ) +
  labs(
    x = "",                 # x-axis label
    y = "Mean Temperature (°C)"  # y-axis label
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # rotate labels to avoid overlap
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),   # rotate labels to avoid overlap
        axis.title.x = element_text(size = 16),  # x-axis label
        axis.title.y = element_text(size = 16),  # y-axis label
        axis.text.y  = element_text(size = 14),   # y-axis tick labels
        legend.title = element_text(size = 16),  # legend title font size
        legend.text = element_text(size = 14),    # legend item font size
        strip.text = element_text(size = 16) 
  )


temp_time_facet

ggsave("figures/temp_region_plot_facet.png", plot = temp_time_facet, width = 12, height = 8, dpi = 300)

# plotting it a bit differently 
library(RColorBrewer)
temp_time_colour <- ggplot(all_data_high_tide_region_avg, aes(x = date, y = avg_temp, colour = region)) +
  geom_line(alpha = 0.9) +
  scale_x_date(
    date_labels = "%b %Y",      # label format: Month Year
    date_breaks = "1 month"     # place a tick every month
  ) +
  scale_color_manual(name = "Region", values = brewer.pal(4, "Set3")) +
  labs(
    x = "",                 # x-axis label
    y = "Mean Temperature (°C)"  # y-axis label
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),   # rotate labels to avoid overlap
      axis.title.x = element_text(size = 16),  # x-axis label
      axis.title.y = element_text(size = 16),  # y-axis label
      axis.text.y  = element_text(size = 14),   # y-axis tick labels
      legend.title = element_text(size = 16),  # legend title font size
      legend.text = element_text(size = 14)    # legend item font size
    )

temp_time_colour

ggsave("figures/temp_region_plot_colour.png", plot = temp_time_colour, width = 14, height = 6, dpi = 300)


# ------------------------------------------------------------------------------

# CACULATING TEMPERATURE ANOMALIES
 
# ------------------------------------------------------------------------------
library(dplyr)
library(readr)
library(lubridate) 

## Putting Pine Island climatology together 
# List all CSV files that match your pattern
files <- list.files("data", pattern = "pine_island_.*\\.csv$", full.names = TRUE)

all_data <- lapply(files, function(f) {
  read_csv(f, col_types = cols(.default = "c"))  # read everything as character
}) %>% bind_rows()

head(all_data)

# Save combined data as a new CSV
write_csv(all_data, "data/pine_island_combined.csv")

# Read in combined csv 
pine_island <- read_csv("data/pine_island_combined.csv")

pine_island <- pine_island %>%
  select(
    station_date = `Date/Time`,
    station_temp = `Mean Temp (°C)`
  ) %>%
  filter(!is.na(station_temp))   

logger_daily <- all_data_high_tide_region_avg 
  
# Rename columns so they match for joining
pine_island <- pine_island %>%
  rename(date = station_date,
         temp_station = station_temp)
logger_daily <- logger_daily %>%
  rename(temp_logger = avg_temp)

pine_island <- pine_island %>%
  mutate(doy = yday(date))
logger_daily <- logger_daily %>%
  mutate(doy = yday(date))

## Calculate climatology for DOY
daily_clim <- pine_island %>%
  group_by(doy) %>%
  summarise(temp_clim = mean(temp_station, na.rm = TRUE))

# Overlap period: 2022–2024
overlap <- logger_daily %>%
  filter(date <= as.Date("2024-12-31")) %>%
  left_join(pine_island, by = "date", relationship = "many-to-many")

bias_per_region <- overlap %>%
  group_by(region) %>%
  summarise(bias_offset = mean(avg_temp - temp_station, na.rm = TRUE))



