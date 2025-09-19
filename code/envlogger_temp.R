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
  mutate(region = case_when(
    site_id %in% c("SC3", "SC4", "BATI12 (BATI14)", "BATI3", "BATI2") ~ "Dynamic",
    site_id %in% c("SC17", "SC18", "SC1", "BATI5", "SC2", "SC9", "BATI9", "SC10", "SC20", 
                   "SC7", "SC21", "SC6", "SC5", "BATI29", "SC11") ~ "Inlet"
  ))

library(leaflet)
library(dplyr)

# Split data: BATI5 vs others
top_labels <- logger_locations %>% filter(site_id != "BATI5")
bottom_label <- logger_locations %>% filter(site_id == "BATI5")

# Create a color palette for the 'region' column
pal <- colorFactor(
  palette = c("blue", "red"),   # choose colors for your regions
  domain = logger_locations$region
)

# Create map
logger_locations_map <- leaflet(logger_locations) %>%
  addTiles() %>%  # default base map
  addProviderTiles("CartoDB.Positron") %>% 
  addCircleMarkers(
    ~lon, ~lat,        
    radius = 2,                  
    color = ~pal(region),       # color by region
  ) %>% 
  # Labels for most points (on top)
  addLabelOnlyMarkers(
    data = top_labels,
    ~lon, ~lat,
    label = ~site_id,
    labelOptions = labelOptions(
      noHide = TRUE,             
      direction = "top",         
      textOnly = TRUE,           
      style = list(
        "color" = "black",
        "font-size" = "11px",
        "padding" = "2px")
    )
  ) %>%
  # Label for BATI5 (underneath)
  addLabelOnlyMarkers(
    data = bottom_label,
    ~lon, ~lat,
    label = ~site_id,
    labelOptions = labelOptions(
      noHide = TRUE,             
      direction = "bottom",      # below the point
      textOnly = TRUE,           
      style = list(
        "color" = "black",
        "font-size" = "11px",
        "padding" = "2px")
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



mapshot(logger_locations_map, file = "figures/logger_locations_map.png", vwidth = 840, vheight = 400, zoom = 10)

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
W <- unit(33.87, "cm") / 3
H <- unit(19.05, "cm") / 3

# Create a function to make plotting easier 
#plot_env_tides <- function(
    logger,
    T10 = NULL, T11 = NULL,
    T20 = NULL, T21 = NULL,
    wh = 25, DN = TRUE,
    fn = NULL, Wmult = 0.5, Hmult = 0.9
) 
  {
  #filters final df for only rows specific to logger that you want to plot
  dat_final_used <- filter(dat_final, sh == logger)
  
  # to define the start and end times of the plot 
  t10 <- if (is.null(T10)) min(dat_final$t) else as.POSIXct(T10)
  t11 <- if (is.null(T11)) max(dat_final$t) else as.POSIXct(T11)
  detail <- !any(is.null(T20), is.null(T21))
  if (detail) {
    t20 <- as.POSIXct(T20)
    t21 <- as.POSIXct(T21)
  }
  
  print(dat_final_used$t %>% range())
  
  # Keeps only rows in the timeframe that wants to be plotted 
  dat_final_used <- filter(dat_final_used, between(t, t10, t11))
  
  # define x and y limits 
  if (detail) xlim <- c(as.POSIXct(t20), as.POSIXct(t21))
  ylim <- range(dat_final_used$temp)
  
  # now safe to call get_dn - for tide envelopes 
  dn   <- get_dn(dat_final_used, ylim)
  ylim <- dn %>% select(-t) %>% unlist() %>% range()
  
  # main plot 
  p1 <- ggplot(dat_final_used)

  if (detail) {
    p1 <- p1 +
      annotate(
        geom = "rect",
        xmin = xlim[1], xmax = xlim[2],
        ymin = ylim[1], ymax = ylim[2],
        col = "grey20", fill = NA, linewidth = 0.25
      )
  }

  # add main line plot - this is temp here 
  p1 <- p1 +
    geom_line(aes(t, temp), col = "grey10") +
    scale_x_datetime(expand = c(0,0), labels = label_date_short()) +
    theme_bw() +
    theme(
      axis.title         = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.margin        = unit(rep(0.5, 4), "cm"),
      strip.background   = element_blank(),
      strip.text         = element_blank()
    ) +
    ylim(ylim)

  if (detail) {
    p2 <- dat_final_used %>%
      filter(between(t, t20, t21)) %>%
      ggplot()

    if (DN) {
      p2 <- p2 +
        geom_ribbon(
          data = dn,
          mapping = aes(x = t, ymin = ymin, ymax = ymax),
          inherit.aes = FALSE,
          fill = "grey90", 
          show.legend = FALSE
        )
    }

    dat_final_used <- dat_final_used %>%
      mutate(h_rescaled = scales::rescale(h, to = c(ylim[1] + 1, ylim[1] + wh)))
    
    p2 <- p2 +
      geom_ribbon(
        aes(t, ymin = ylim[1], ymax = h_rescaled),
        fill = "steelblue1", alpha = 0.5
      ) +
      geom_line(aes(t, temp), col = "grey10") +
      scale_x_datetime(limits = c(t20, t21), labels = label_date_short()) +
      theme_bw() +
      theme(
        panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        strip.text   = element_blank(),
        axis.title   = element_blank(),
        plot.margin  = unit(rep(0.5, 4), "cm")
      ) +
      ylim(ylim) +
      coord_cartesian(expand = FALSE)
  }

  p <- p1
  if (detail) p <- p1 / p2
  #p <- p + plot_layout(ncol = 1)

  if (!is.null(fn)) {
    ggsave(
      file  = str_c(logger, "_", fn, ".pdf"),
      plot  = p,
      width = W * Wmult, height = H * Hmult)
  }

  print(p)
}

## plots ----
plot_env_tides(
  logger = "bati3",
  T20 = "2023-07-01", T21 = "2023-07-15",
  fn = "1")

plot_env_tides(
  logger = "bati3",
  T20 = "2024-01-06", T21 = "2024-01-23",
  fn = "2")



# ------------------------------------------------------------------------------
# PLOTTING TEMP 

# select only necessary columns 
all_data <- select(dat_final, id, t, temp, h, lo, hi)

# all data for a given id and only during high tide
all_data_high_tide <- filter(all_data, hi)

# assigning stations to region and averaging across
all_data_high_tide_region_avg <- all_data_high_tide %>% 
  mutate(region = case_when(
    id %in% c("sc3", "sc4", "bati12", "bati3") ~ "Dynamic",
    id %in% c("sc17", "sc18", "sc1", "sc2", "bati5", "bati9", "sc9", "sc10", "sc20", "sc7", 
              "sc21", "sc6", "sc5", "bati29", "sc11") ~ "Inlet"
  )) %>% 
  group_by(region, t) %>% 
  summarise(avg_temp = mean(temp, na.rm = TRUE))

# plotting temperature by region
library(scales)  # for date formatting
ggplot(all_data_high_tide_region_avg) +
  geom_line(aes(t, avg_temp)) +
  facet_wrap(~ region, scales = "free_y", ncol = 1) +
  scale_x_datetime(
    date_labels = "%b %Y",      # label format: Month Year
    date_breaks = "1 month"     # place a tick every month
  ) +
  labs(
    x = "",                 # x-axis label
    y = "Average Temperature (°C)"  # y-axis label
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # rotate labels to avoid overlap
  )

ggsave("figures/temp_region_plot_facet.png", plot = last_plot(), width = 20, height = 12, dpi = 300)

# plotting it a bit differently 
ggplot(all_data_high_tide_region_avg, aes(x = t, y = avg_temp, colour = region)) +
  geom_line() +
  scale_x_datetime(
    date_labels = "%b %Y",      # label format: Month Year
    date_breaks = "1 month"     # place a tick every month
  ) +
  scale_color_manual(name = "Region",
                     values = c("Dynamic" = "blue", "Inlet" = "red")) +
  labs(
    x = "",                 # x-axis label
    y = "Average Temperature (°C)"  # y-axis label
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # rotate labels to avoid overlap
  ) 


ggsave("figures/temp_region_plot_colour.png", plot = last_plot(), width = 20, height = 12, dpi = 300)









