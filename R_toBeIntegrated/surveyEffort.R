pkgs <- c("tidyverse", "sf", 'purrr', 'plotly')
sapply(pkgs, require, character = T)


###
## See imagesToCRC for updated code
###

# Captain's Logger
#clDir <- "J:/LAN_structure_template/Data/Cetacean/VesselSurveys/CaptainsLogger/"
clDir <- "G:/2023_PNW_data/CaptainsLogger/"
#clDir <- "F:/2024_PNW_data/CaptainsLogger/"
#clDir <- "D:/2025_PNW_data/CaptainsLogger/"
clFiles <- list.files(clDir, "*.csv$", recursive = T, full.names = T)
df1 <- map_dfr(1:length(clFiles), function(x) {
  read.csv(clFiles[x]) %>% mutate(survey = x)
})

clDF <- df1 %>%
  mutate(
    dateTime_GMT = lubridate::mdy_hms(
      str_sub(df1$DateTime_GMT, start = 1, end = -4),
      tz = "GMT"
    ),
    dateTime_local = lubridate::with_tz(dateTime_GMT, "Etc/GMT+7"),
    source = "IMU Logger"
  ) %>%
  filter(!is.na(dateTime_GMT) & Longitude < 0) %>%
  filter(lubridate::year(dateTime_GMT) < 2029) #%>%
#st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326))

# Garmin
#garDir <- "J:/LAN_structure_template/Data/Cetacean/VesselSurveys/Garmin/"
garDir <- "G:/2023_PNW_data/Garmin/"
#garDir <- "F:/2024_PNW_data/Garmin/"
#garDir <- "D:/2025_PNW_data/Garmin/"
garFiles <- list.files(garDir, "*.GPX$", recursive = T, full.names = T)

df <- map_dfr(1:length(garFiles), function(x) {
  st_read(garFiles[x], "track_points")
})

garDF <- df %>%
  filter(!duplicated(.)) %>%
  mutate(
    source = "Garmin Chartplotter",
    #dateTime_GMT = lubridate::ymd_hms(time, tz = "GMT"),
    #dateTime_local = lubridate::with_tz(dateTime_GMT, tz = "Etc/GMT+7")
    dateTime_local = lubridate::ymd_hms(time, tz = "Etc/GMT+7"),
    dateTime_GMT = lubridate::with_tz(dateTime_local, tz = "GMT")
  ) %>%
  filter(!is.na(dateTime_local) & lubridate::year(dateTime_local) >= 2022) %>%
  arrange(dateTime_local) %>%
  dplyr::select(
    time,
    track_fid,
    track_seg_id,
    track_seg_point_id,
    source,
    dateTime_GMT,
    dateTime_local,
    geometry,
    gpxtpx_TrackPointExtension
  ) %>%
  mutate(
    longitude = sf::st_coordinates(.)[, 1],
    latitude = sf::st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

# Combining the two track logs
oDF <- clDF %>%
  dplyr::select(
    survey,
    source,
    datetime_gmt = dateTime_GMT,
    datetime_local = dateTime_local,
    longitude = Longitude,
    latitude = Latitude
  ) %>%
  add_row(
    garDF %>%
      dplyr::select(
        survey = track_seg_id,
        source,
        datetime_gmt = dateTime_GMT,
        datetime_local = dateTime_local,
        longitude,
        latitude
      )
  )

clDates <- unique(
  oDF %>%
    filter(!grepl("Garmin", source)) %>%
    pull(datetime_local) %>%
    as.Date() %>%
    as.character()
)
garminDates <- unique(
  oDF %>%
    filter(grepl("Garmin", source)) %>%
    pull(datetime_local) %>%
    as.Date() %>%
    as.character()
)
ind <- which(!(clDates %in% garminDates))


oDF2 <- oDF %>%
  mutate(Date = as.Date(datetime_local)) %>%
  filter(
    grepl("Garmin", source) |
      (!grepl("Garmin", source) & as.character(Date) %in% clDates[ind])
  ) %>%
  filter(longitude < -123.5) %>%
  arrange(datetime_local, survey) %>%
  filter(!duplicated(datetime_local)) %>%
  mutate(
    tdiff = c(0, diff(datetime_local)) / 3600,
    breaks = if_else(tdiff > 2, 1, 0),
    survey_num = cumsum(breaks),
    surveyid = paste(year(datetime_local), survey_num, sep = "_")
  ) %>%
  #mutate(surveyid = paste(Date, survey, sep = "_")) %>%
  #mutate(survey = as.numeric(factor(Date))) %>%
  #rename(surveyid = survey) %>%
  dplyr::select(
    surveyid,
    source,
    datetime_gmt,
    datetime_local,
    longitude,
    latitude
  )

# View(oDF2 %>%
#   mutate(Date = as.Date(datetime_local)) %>%
#   group_by(Date, source) %>%
#   summarize(minDT = min(datetime_local),
#             maxDT = max(datetime_local),
#             N = n()))

oPts <- st_as_sf(oDF2, coords = c("longitude", "latitude"), crs = 4326)
oLines <- oPts %>%
  arrange(datetime_local) %>%
  group_by(surveyid) %>%
  summarize(
    date = as.Date(datetime_local[1]),
    do_union = F,
    .group = "keep"
  ) %>%
  st_cast("MULTILINESTRING")

# st_write(oPts, "./Data/SHP/TrackPoints_2025.shp", delete_dsn = T)
# st_write(oLines, "./Data/SHP/TrackLines_2025.shp", delete_dsn = T)

#st_write(oPts, "C:/Users/peter.mahoney/Desktop/ForKurt_BOEM/NOAA_MML_CalCurr_SurveyTrack_points.shp")
#st_write(oLines, "C:/Users/peter.mahoney/Desktop/ForKurt_BOEM/NOAA_MML_CalCurr_SurveyTrack_lines.shp")

# Detections
#ommDF <- read.csv("E:/Whale_Observations_2023-11-01-17-38-35.csv") %>%
ommDF2023 <- read.csv(
  "E:/2024_PNW_data/Whale_Observations_all_pre-mid2023.csv"
) %>%
  dplyr::select(
    dateTime = Observations.Observation.time,
    Longitude = Observations._Record.your.current.location_longitude,
    Latitude = Observations._Record.your.current.location_latitude,
    species = Observations.Species.,
    otherSpecies = Observations.Other.Species.,
    count = Observations.Count..N.
  ) %>%
  mutate(dateTime = lubridate::ymd_hms(dateTime)) %>%
  filter(
    species %in%
      c("Gray Whale", "Humpback Whale") &
      lubridate::year(dateTime) > 2021
  ) %>%
  arrange(dateTime)

mmDF2023 <- read.csv(
  "E:/2024_PNW_data/MMVS_Observations_2023-11-01-17-38-35.csv"
) %>%
  dplyr::select(
    dateTime = Observation.time,
    Longitude = X_Record.your.current.location_longitude,
    Latitude = X_Record.your.current.location_latitude,
    species = Species.,
    otherSpecies = Other.Species.,
    count = Count..N.
  ) %>%
  filter(species %in% c("Gray Whale", "Humpback Whale")) %>%
  mutate(
    species = factor(species, levels = c("Humpback Whale", "Gray Whale"))
  )

mmDF <- read.csv(
  "E:/2024_PNW_data/MMVS_Observations_2024-10-15-03-19-23.csv"
) %>%
  dplyr::select(
    dateTime = Observation.time,
    Longitude = X_Record.your.current.location_longitude,
    Latitude = X_Record.your.current.location_latitude,
    species = Species.,
    otherSpecies = Other.Species.,
    count = Count..N.
  ) %>%
  filter(species %in% c("Gray Whale", "Humpback Whale")) %>%
  mutate(
    species = factor(species, levels = c("Humpback Whale", "Gray Whale"))
  )

obsDF <- rbind(ommDF2023, mmDF2023, mmDF) %>%
  mutate(
    year = lubridate::year(dateTime)
  ) %>%
  filter(year >= 2023)
#obsDF <- mmDF

# mapboxToken <- paste(readLines("../.mapbox_token"), collapse="")    # You need your own token
# Sys.setenv("MAPBOX_TOKEN" = mapboxToken) # for Orca
#mapboxToken <- paste(readLines("../.mapbox_token"), collapse="")    # You need your own token
Sys.setenv(
  "MAPBOX_TOKEN" = "pk.eyJ1IjoicG1haG9ub2FhIiwiYSI6ImNsY2pkbGc5NzBnM3Izd21tMmtqejMzZmYifQ.HktBKnzglbHhNNPciTb-uw"
) # for Orca

plot_mapbox(
  x = ~Longitude,
  y = ~Latitude,
  mode = "markers",
  colors = c("#d9bdff", "#99009c")
) %>%

  plotly::add_paths(
    data = group_by(as.data.frame(spDF), survey),
    x = ~Longitude,
    y = ~Latitude,
    #colors = I("red"),
    # line = list(color='#96fff6',
    #             dash='dashed',
    #             width = 0.6),
    #text = ~dateTime,
    #hoverinfo = "none",
    #showlegend = F
  ) %>%

  # add_trace(
  #   data = obsDF,
  #   x = ~Longitude, y = ~Latitude,
  #   color = ~species,
  #   # colors = c("blue", "red"),
  #   marker = list(
  #     #color = ~as.factor(species),
  #     #colors = c("blue", "red"),
  #     #color = "#99009c",
  #     size = ~ifelse(count > 4, 12, 3*count),
  #     opacity = 0.6,
  #     line = list(
  #       color = '#ffffff',
  #       width = 5
  #     )
  #   ),
  #   text = ~paste(species, ": N =", count),
  #   hoverinfo = "text"#,
  #   #showlegend = F
  # ) %>%

  layout(
    mapbox = list(
      style = 'dark',
      zoom = 4.75,
      center = list(lon = -124, lat = 45.5)
    )
  ) %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))


# by species
plot_mapbox(
  x = ~Longitude,
  y = ~Latitude,
  mode = "markers",
  colors = c("#99009c", "#e06919")
) %>%

  # plotly::add_paths(
  #   data = group_by(as.data.frame(spDF2023), surveyd),
  #   x = ~Longitude, y = ~Latitude,
  #   #colors = I("red"),
  #   line = list(color= "#e0a3ff", #'#96fff6',
  #               opacity = 0.6,
  #               dash='dashed',
  #               width = 0.6),
  #   #text = ~dateTime,
  #   #hoverinfo = "none",
  #   #showlegend = F
  # ) %>%

  plotly::add_paths(
    data = group_by(as.data.frame(spDF), survey),
    x = ~Longitude,
    y = ~Latitude,
    #colors = I("red"),
    line = list(
      color = "#e0a3ff", #'#96fff6',
      dash = 'dashed',
      opacity = 0.6,
      width = 0.6
    ),
    #text = ~dateTime,
    #hoverinfo = "none",
    #showlegend = F
  ) %>%

  add_trace(
    data = obsDF %>%
      mutate(
        species = factor(species, levels = c("Humpback Whale", "Gray Whale"))
      ), # %>% filter(species == "Gray Whale"),
    x = ~Longitude,
    y = ~Latitude,
    color = ~species,
    marker = list(
      #color = ~as.factor(species),
      #colors = c("blue", "red"),
      #color = "#99009c",
      size = ~ ifelse(count > 4, 12, 3 * count),
      opacity = 0.6,
      line = list(
        color = '#ffffff',
        width = 5
      )
    ),
    text = ~ paste(species, ": N =", count),
    hoverinfo = "text" #,
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


# by year
plot_mapbox(
  x = ~Longitude,
  y = ~Latitude,
  mode = "markers",
  colors = c("#85484e", "#f2071f")
) %>%

  plotly::add_paths(
    data = group_by(as.data.frame(spDF2023), surveyd),
    x = ~Longitude,
    y = ~Latitude,
    #colors = I("red"),
    line = list(
      color = "#85484e", #'#96fff6',
      opacity = 0.6,
      dash = 'dashed',
      width = 0.6
    ),
    #text = ~dateTime,
    #hoverinfo = "none",
    #showlegend = F
  ) %>%

  plotly::add_paths(
    data = group_by(as.data.frame(spDF), survey),
    x = ~Longitude,
    y = ~Latitude,
    #colors = I("red"),
    line = list(
      color = "#f2071f", #'#96fff6',
      dash = 'dashed',
      width = 0.6
    ),
    #text = ~dateTime,
    #hoverinfo = "none",
    #showlegend = F
  ) %>%

  add_trace(
    data = obsDF %>%
      mutate(
        species = factor(species, levels = c("Humpback Whale", "Gray Whale"))
      ), # %>% filter(species == "Gray Whale"),
    x = ~Longitude,
    y = ~Latitude,
    color = ~year,
    marker = list(
      #color = ~as.factor(species),
      #colors = c("blue", "red"),
      #color = "#99009c",
      size = ~ ifelse(count > 4, 12, 3 * count),
      opacity = 0.6,
      line = list(
        color = '#ffffff',
        width = 5
      )
    ),
    text = ~ paste(species, ": N =", count),
    hoverinfo = "text" #,
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


# obsSummary
# ommDF <- read.csv("E:/Whale_Observations_all_pre-mid2023.csv") %>%
#   dplyr::select(dateTime = Observations.Observation.time,
#                 Longitude = Observations._Record.your.current.location_longitude,
#                 Latitude =  Observations._Record.your.current.location_latitude,
#                 species = Observations.Species.,
#                 otherSpecies = Observations.Other.Species.,
#                 count = Observations.Count..N.) %>%
#   mutate(dateTime = lubridate::ymd_hms(dateTime)) %>%
#   filter(lubridate::year(dateTime) == 2023) %>%
#   arrange(dateTime)

#mmDF <- read.csv("E:/MMVS_Observations_2023-11-01-17-38-35.csv") %>%
mmDF <- read.csv(
  "E:/2024_PNW_data/MMVS_Observations_2024-10-15-03-19-23.csv"
) %>%
  dplyr::select(
    dateTime = Observation.time,
    Longitude = X_Record.your.current.location_longitude,
    Latitude = X_Record.your.current.location_latitude,
    species = Species.,
    otherSpecies = Other.Species.,
    count = Count..N.
  ) #%>%
#filter(species %in% c("Gray Whale", "Humpback Whale"))

#obsDF2 <- rbind(ommDF, mmDF) %>%
obsDF2 <- mmDF %>%
  filter(
    !grepl("Albatross", otherSpecies) &
      !grepl("mola", otherSpecies) &
      !grepl("seal", otherSpecies) &
      !grepl("SEAL", otherSpecies) &
      !grepl("Seal", otherSpecies) &
      !grepl("booby", otherSpecies) &
      !grepl("Vitulina", otherSpecies)
  )

Nencounters <- nrow(obsDF2)
Nen_gray <- sum(obsDF2$species == "Gray Whale")
N_all <- sum(obsDF2$count)
N_excl_WSD <- obsDF2 %>%
  filter(
    !grepl("White", otherSpecies) &
      !grepl("Pws", otherSpecies) &
      !grepl("white", otherSpecies)
  ) %>%
  pull(count) %>%
  sum()
Ninds_gray <- obsDF2 %>%
  filter(species == "Gray Whale") %>%
  pull(count) %>%
  sum()


#mmDF <- read.csv("E:/MMVS_Observations_2023-11-01-17-38-35.csv") %>%
mmDF <- read.csv(
  "E:/2024_PNW_data/MMVS_Observations_2024-10-15-03-19-23.csv"
) %>%
  dplyr::select(
    dateTime = Observation.time,
    Longitude = X_Record.your.current.location_longitude,
    Latitude = X_Record.your.current.location_latitude,
    species = Species.,
    otherSpecies = Other.Species.,
    count = Count..N.,
    AID = Drone.Data.Acquired.Aerial.ID,
    BIC = Drone.Data.Acquired.Aerial.BC..w.LiDAR.
  ) %>%
  filter(species == "Gray Whale")

mmDF %>% filter(AID == 1) %>% pull(count) %>% sum()
mmDF %>% filter(BIC == 1) %>% pull(count) %>% sum()


obsDF2 <- obsDF %>%
  filter(
    !grepl("Albatross", otherSpecies) &
      !grepl("mola", otherSpecies) &
      !grepl("seal", otherSpecies) &
      !grepl("SEAL", otherSpecies) &
      !grepl("Seal", otherSpecies) &
      !grepl("booby", otherSpecies) &
      !grepl("Vitulina", otherSpecies)
  ) %>%
  filter(year == 2024)

Nencounters <- nrow(obsDF2)
Nen_gray <- sum(obsDF2$species == "Gray Whale")
(Nen_hump <- sum(obsDF2$species == "Humpback Whale"))
N_all <- sum(obsDF2$count)
N_excl_WSD <- obsDF2 %>%
  filter(
    !grepl("White", otherSpecies) &
      !grepl("Pws", otherSpecies) &
      !grepl("white", otherSpecies)
  ) %>%
  pull(count) %>%
  sum()
Ninds_gray <- obsDF2 %>%
  filter(species == "Gray Whale") %>%
  pull(count) %>%
  sum()
(Ninds_hump <- obsDF2 %>%
  filter(species == "Humpback Whale") %>%
  pull(count) %>%
  sum())


#mmDF <- read.csv("E:/MMVS_Observations_2023-11-01-17-38-35.csv") %>%
mmDF <- read.csv(
  "E:/2024_PNW_data/MMVS_Observations_2024-10-15-03-19-23.csv"
) %>%
  dplyr::select(
    dateTime = Observation.time,
    Longitude = X_Record.your.current.location_longitude,
    Latitude = X_Record.your.current.location_latitude,
    species = Species.,
    otherSpecies = Other.Species.,
    count = Count..N.,
    AID = Drone.Data.Acquired.Aerial.ID,
    BIC = Drone.Data.Acquired.Aerial.BC..w.LiDAR.
  ) %>%
  #filter(species == "Gray Whale")
  filter(species == "Humpback Whale")

mmDF %>% filter(AID == 1) %>% pull(count) %>% sum()
mmDF %>% filter(BIC == 1) %>% pull(count) %>% sum()


mmDF <- read.csv(
  "D:/2025_PNW_data/MMS_Observations_2025-11-12-03-42-44.csv"
) %>%
  dplyr::select(
    dateTime = Sightings.New.Sighting.Observation.time,
    Longitude = Sightings.New.Sighting._Record.your.current.location_longitude,
    Latitude = Sightings.New.Sighting._Record.your.current.location_latitude,
    species = Sightings.New.Sighting.Species.,
    otherSpecies = Sightings.New.Sighting.Other.Species.,
    count = Sightings.New.Sighting.Count..N.,
    photographed = Sightings.New.Sighting.Photographed..N.
  ) #%>%
#filter(species == "Gray Whale")
#filter(species == "Humpback Whale")

summ <- mmDF %>%
  group_by(species, otherSpecies) %>%
  summarize(
    Nobs = n(),
    Ncnt = sum(count),
    Nphoto = sum(photographed)
  )

obsDF <- mmDF %>%
  filter(species == "Gray Whale")

# by species
plot_mapbox(
  x = ~Longitude,
  y = ~Latitude,
  mode = "markers",
  colors = c("#99009c", "#e06919")
) %>%

  add_trace(
    data = obsDF %>%
      mutate(
        species = factor(species, levels = c("Humpback Whale", "Gray Whale"))
      ), # %>% filter(species == "Gray Whale"),
    x = ~Longitude,
    y = ~Latitude,
    color = ~species,
    marker = list(
      #color = ~as.factor(species),
      #colors = c("blue", "red"),
      color = "#99009c",
      size = ~ ifelse(count > 4, 12, 3 * count),
      opacity = 0.6,
      line = list(
        color = '#ffffff',
        width = 5
      )
    ),
    text = ~ paste(species, ": N =", count),
    hoverinfo = "text" #,
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

# Routes
# spDF <- df %>%
#   mutate(dateTime =  lubridate::mdy_hms(str_sub(df$DateTime_GMT, start = 1, end = -4))) %>%
#   filter(!is.na(dateTime) & Longitude < 0 &
#            lubridate::year(dateTime) == 2024) %>%
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326))
#
# spLI <- spDF %>%
#   arrange(dateTime) %>%
#   group_by(survey) %>%
#   summarize(dt = as.Date(dateTime)[1],do_union=FALSE) %>%
#   st_cast("MULTILINESTRING")
#
# spLI %>% st_length() %>% sum()
spDF2023 <- st_read(
  "C:/Users/peter.mahoney/Desktop/WhitmireReq_BOEM_OffshoreWind_2023/shp/NOAA_MML_CalCurr_SurveyTrack_points.shp"
)
spDF2023 <- spDF2023 %>%
  mutate(
    Longitude = st_coordinates(spDF2023)[, 1],
    Latitude = st_coordinates(spDF2023)[, 2],
  )

spDF <- oDF2 %>%
  dplyr::select(
    survey = surveyid,
    dateTime = datetime_local,
    Longitude = longitude,
    Latitude = latitude
  ) %>%
  filter(
    !is.na(dateTime) & Longitude < 0 & lubridate::year(dateTime) == 2024
  ) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326), remove = F)

spLI <- spDF %>%
  arrange(dateTime) %>%
  group_by(survey) %>%
  summarize(dt = as.Date(dateTime)[1], do_union = FALSE) %>%
  st_cast("MULTILINESTRING")

spLI %>% st_length() %>% sum()
