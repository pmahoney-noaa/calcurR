pkgs <- c("tidyverse", "sf", 'purrr', 'plotly')
sapply(pkgs, require, character = T)

# Old MMVS
mmDF_p2023 <- read.csv("F:/2024_PNW_data/Whale_Observations_all_pre-mid2023.csv") %>%
  mutate(
    behavioralSummary = NA,
    travelDir = NA,
    foragingType = NA,
    droneFlown = NA,
  ) %>%
  dplyr::select(dateTime = Observations.Observation.time,
                Longitude = Observations._Record.your.current.location_longitude,
                Latitude = Observations._Record.your.current.location_latitude,
                species = Observations.Species.,
                otherSpecies = Observations.Other.Species.,
                count = Observations.Count..N.,
                photographed = Observations.Photographed..N.,
                depth = Observations.Depth,
                behavioralSummary, travelDir, foragingType, droneFlown,
                comments = Observations.Comments
  ) %>%
  filter(species %in% c("Gray Whale")) %>%
  filter(count > 0)

# MMVS
mmDF_2023 <- read.csv("F:/2024_PNW_data/MMVS_Observations_2023-11-01-17-38-35.csv") %>%
  dplyr::select(dateTime = Observation.time,
                Longitude = X_Record.your.current.location_longitude,
                Latitude = X_Record.your.current.location_latitude,
                species = Species.,
                otherSpecies = Other.Species.,
                count = Count..N.,
                photographed = Photographed..N.,
                depth = Depth,
                behavioralSummary = Behavior,
                travelDir = Direction.of.Travel.,
                foragingType = Foraging.Type.,
                droneFlown = Drone.Flown.,
                comments = Comments
  ) %>%
  filter(species %in% c("Gray Whale")) %>%
  filter(count > 0)

mmDF_2024 <- read.csv("F:/2024_PNW_data/MMVS_Observations_2024-10-15-03-19-23.csv") %>%
  dplyr::select(dateTime = Observation.time,
                Longitude = X_Record.your.current.location_longitude,
                Latitude = X_Record.your.current.location_latitude,
                species = Species.,
                otherSpecies = Other.Species.,
                count = Count..N.,
                photographed = Observations.Photographed..N.,
                depth = Observations.Depth,
                behavioralSummary = Observations.Behavioral.Summary.Behavior,
                travelDir = Observations.Behavioral.Summary.Direction.of.Travel.,
                foragingType = Observations.Behavioral.Summary.Foraging.Type.,
                droneFlown = Observations.Drone.Flight.Summary.Drone.Flown.,
                comments = Observations.Observation.Comments.Comments
                ) %>%
  filter(species %in% c("Gray Whale")) %>%
  filter(count > 0)


mmDF <- rbind(mmDF_p2023, mmDF_2023, mmDF_2024) %>%
  filter(
    Latitude <= 43.35
  )


write.csv(mmDF, "C:/Users/pmaho/Desktop/Er_surveyData_HSU_CRC_collab.csv")
