# ##############################################################################

# Authored by Rui Seabra
# Modified by Hana Burdge
# To plot water temperatures recorded by envloggers to relate to kelp distribution in the study area over time

# ##############################################################################

library(tidyverse)
library (tidyr)
library(purrr)
library(dplyr)
install.packages("devtools")
devtools::install_github("ruiseabra/envlogger")
library(envlogger)
library(patchwork)
library(scales)
install.packages("suncalc")
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

## grab tides ----
fn <- "tides.RDS"
if (!file.exists(fn)) {
  # this only has to be executed if "tides.RDS" is not present
  # I already calculated the tides for Salmon Coast Field Station on Gilford Island, Canada for the period 2022-2026
  # the tides are calculated using fes2022
  # (https://www.aviso.altimetry.fr/en/data/products/auxiliary-products/global-tide-fes/release-fes22.html)
  # this software is quite complex to install
  # feel free to try to install it as it will give you a lot more flexibility if you can run it yourself
  # otherwise, if you want tides for a different location/period, just let me know lat, lon, start and end dates and I can quickly calculate it for you
  source("~/Dropbox/RS/bio/datasets/fes2022b/fes2022b.R")
  tid <- tides(
    lat = LAT,
    lon = LON,
    start_date = as.Date("2022-01-01"),
    end_date   = as.Date("2027-01-01"),
    step_mins  = 60, extrapolated = TRUE) %>%
    ungroup()
  saveRDS(tid, fn)
}
tid <- readRDS(fn)

dat <- dat %>%
    left_join(
      tid,
      by = "t"
    )

DAT <- dat

# PLOT ----
W <- unit(33.87, "cm") / 3
H <- unit(19.05, "cm") / 3

plot_env_tides <- function(
    logger,
    T10 = NULL, T11 = NULL,
    T20 = NULL, T21 = NULL,
    wh = 25, DN = TRUE,
    fn = NULL, Wmult = 0.5, Hmult = 0.9
) {
  dat <- filter(dat, sh == logger)

  t10 <- if (is.null(T10)) min(dat$t) else as.POSIXct(T10)
  t11 <- if (is.null(T11)) max(dat$t) else as.POSIXct(T11)
  detail <- !any(is.null(T20), is.null(T21))
  if (detail) {
    t20 <- as.POSIXct(T20)
    t21 <- as.POSIXct(T21)
  }

  print(dat$t %>% range())

  dat <- filter(dat, between(t, t10, t11))

  if (detail) xlim <- c(as.POSIXct(t20), as.POSIXct(t21))
  ylim <- range(dat$temp)

  dn   <- get_dn(dat, ylim)
  ylim <- dn %>% select(-t) %>% unlist() %>% range()

  p1 <- ggplot(dat)

  if (detail) {
    p1 <- p1 +
      annotate(
        geom = "rect",
        xmin = xlim[1], xmax = xlim[2],
        ymin = ylim[1], ymax = ylim[2],
        col = "grey20", fill = NA, linewidth = 0.25
      )
  }

  p1 <- p1 +
    geom_line(aes(t, temp), col = "grey10") +
    scale_x_datetime(expand = c(0,0), label = label_date_short()) +
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
    p2 <- dat %>%
      filter(between(t, t20, t21)) %>%
      ggplot()

    if (DN) {
      p2 <- p2 +
        geom_ribbon(
          data = dn,
          aes(t, ymin = ymin, ymax = ymax), fill = "grey90"
        )
    }

    p2 <- p2 +
      geom_ribbon(
        aes(
          t,
          ymax = scales::rescale(h, to = c(ylim[1] + 1, ylim[1] + wh)),
          ymin = ylim[1]),
        fill = "steelblue1", alpha = 0.5
      ) +
      geom_line(aes(t, temp), col = "grey10") +
      scale_x_datetime(limits = c(t20, t21), label = label_date_short()) +
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
  if (detail) p <- p + p2
  p <- p + plot_layout(ncol = 1)

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

## your data ----
all_data <- select(DAT, id, t, temp, h, lo, hi)

# all data for a given id
filter(all_data, id == "bati14")

# all data for a given id and only during high tide
bati14_high_tide <- filter(all_data, id == "bati14", hi)

ggplot(bati14_high_tide) +
  geom_line(aes(t, temp))











