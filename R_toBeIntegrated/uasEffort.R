pkgs <- c("tidyverse", "sf", 'purrr', 'furrr', 'plotly')
sapply(pkgs, require, character = T)

# For jobs in parallel
future::plan("multisession", workers = 6)

# FlightReader Repository
dir <- "C:/Users/pmaho/Documents/FlightReader/Repository/Aircraft/"

# Converted flight logs
files <- list.files(dir, "*-aircraft.csv", recursive = T)

# Read files
#files <- files[1:5]
out <- imap_dfr(files,
  ~ read.csv(paste0(dir, .x)) %>%
    mutate(Flight = .y) %>%
    dplyr::select(
      Flight,
      CUSTOM.date.local = CUSTOM.date..local.,
      CUSTOM.updateTime.local = CUSTOM.updateTime..local.,
      starts_with("OSD."),
      DETAILS.appName, DETAILS.appVersion,
      DETAILS.aircraftName, DETAILS.aircraftSerial, SERIAL.rc, SERIAL.battery
    ),
  .progress = T
)


# Clean up
spDF <- out %>%
  filter(OSD.latitude != 0 & OSD.longitude != 0)

# Mapbox Plot
Sys.setenv("MAPBOX_TOKEN" = "pk.eyJ1IjoicG1haG9ub2FhIiwiYSI6ImNsY2pkbGc5NzBnM3Izd21tMmtqejMzZmYifQ.HktBKnzglbHhNNPciTb-uw") # for Orca

map <- plot_mapbox(x = ~OSD.longitude, y = ~OSD.latitude,
            mode = "markers",
            colors = c("#d9bdff", "#99009c")) %>%

  plotly::add_paths(
    data = group_by(as.data.frame(spDF), Flight),
    x = ~OSD.longitude, y = ~OSD.latitude,
    #colors = I("red"),
    line = list(color='#fc4903',
                dash='solid',
                width = 2),
    #text = ~dateTime,
    #hoverinfo = "none",
    #showlegend = F
  ) %>%

  layout(
    mapbox = list(
      style = 'dark',
      zoom = 4.75,
      center = list(lon = -124, lat = 45.5)))  %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

# save_image(map, "C:/Users/pmaho/Desktop/uasEffort.png",
#            width = 1000, height = 1600)
