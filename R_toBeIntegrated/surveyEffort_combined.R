###
## Load required packages
###
pkgs <- c("tidyverse", "sf", 'purrr', 'plotly')
sapply(pkgs, require, character = T)

###
## Vessel Survey Track Logs
###

# 2023 and 2024 need to be regenerated!!
spDF23 <- st_read("./Data/SHP/TrackPoints_2023.shp")
spDF24 <- st_read("./Data/SHP/TrackPoints_2024.shp")
spDF25 <- st_read("./Data/SHP/TrackPoints_2025.shp")

spDF23 <- spDF23 %>%
  mutate(
    Longitude = st_coordinates(spDF23)[, 1],
    Latitude = st_coordinates(spDF23)[, 2],
  )

spDF24 <- spDF24 %>%
  mutate(
    Longitude = st_coordinates(spDF24)[, 1],
    Latitude = st_coordinates(spDF24)[, 2],
  )

spDF25 <- spDF25 %>%
  mutate(
    Longitude = st_coordinates(spDF25)[, 1],
    Latitude = st_coordinates(spDF25)[, 2],
  )

spDF <- rbind(spDF23, spDF24, spDF25) %>%
  dplyr::select(
    surveyid = surveyd,
    dateTime = dttm_lc,
    longitude = Longitude,
    latitude = Latitude
  ) %>%
  mutate(
    survey = paste(year(dateTime), surveyid, sep = "_")
  ) %>%
  filter(
    !is.na(dateTime) & longitude < 0 #& lubridate::year(dateTime) == 2024
  ) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326), remove = F)

# Mapbox Token
Sys.setenv(
  "MAPBOX_TOKEN" = "pk.eyJ1IjoicG1haG9ub2FhIiwiYSI6ImNsY2pkbGc5NzBnM3Izd21tMmtqejMzZmYifQ.HktBKnzglbHhNNPciTb-uw"
) # for Orca

# Plot
spDF_full <- spDF
spDF <- spDF_full %>% filter(year(dateTime) == 2025)

spLI <- spDF %>%
  arrange(dateTime) %>%
  group_by(survey) %>%
  summarize(dt = as.Date(dateTime)[1], do_union = FALSE) %>%
  st_cast("MULTILINESTRING")

spLI %>% st_length() %>% sum()


plot_mapbox(
  x = ~longitude,
  y = ~latitude,
  mode = "markers",
  colors = c("#d9bdff", "#99009c")
) %>%

  plotly::add_paths(
    data = group_by(as.data.frame(spDF), survey),
    x = ~longitude,
    y = ~latitude,
    #colors = I("red"),
    # line = list(color='#96fff6',
    #             dash='dashed',
    #             width = 0.6),
    #text = ~dateTime,
    #hoverinfo = "none",
    #showlegend = F
  ) %>%

  layout(
    mapbox = list(
      style = 'dark',
      zoom = 4.75,
      center = list(lon = -124, lat = 45.5)
    )
  ) %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))
