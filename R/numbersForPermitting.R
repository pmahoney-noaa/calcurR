numbersForPermitting <- function () {}

pkgs <- c("tidyverse")
sapply(pkgs, require, character = T)


d1 <- read.csv("D:/Whale_Observations_all_pre-mid2023.csv")
d1 <- d1 %>%
  select(
    ObservationID = Observations.obs_id, DateTime = Observations.Observation.time,
    Longitude = Observations._Record.your.current.location_longitude, Latitude = Observations._Record.your.current.location_latitude,
    Species = Observations.Species., Other = Observations.Other.Species.,
    Count = Observations.Count..N.
  ) %>%
  mutate(
    DateTime = lubridate::ymd_hms(DateTime),
    Year = lubridate::year(DateTime),
    Species = ifelse(Species == "Other", Other, Species)
    )


d2 <- read.csv("D:/MMVS_Observations_2023-11-01-17-38-35.csv")
d2 <- d2 %>%
  select(
    ObservationID = obs_id, DateTime = Observation.time,
    Longitude = X_Record.your.current.location_longitude, Latitude =  X_Record.your.current.location_latitude,
    Species = Species., Other = Other.Species.,
    Count = Count..N.
  ) %>%
  mutate(
    DateTime = lubridate::ymd_hms(DateTime),
    Year = lubridate::year(DateTime),
    Species = ifelse(Species == "Other", Other, Species)
  )



## Numbers reported
year <- 2023
wa_or <- 46.2623
or_ca <- 41.9982


d <- d1 %>%
  rbind(d2) %>%
  filter(Year == year) %>%
  mutate(
    State = ifelse(Latitude > wa_or, "WA", ifelse(Latitude < or_ca, "CA", "OR"))
  )

ds <- d %>%
  group_by(Species, State) %>%
  summarize(
    N = sum(Count)
  ) %>%
  arrange(Species,)
View(ds)


d %>% filter(Species == "Orca")



