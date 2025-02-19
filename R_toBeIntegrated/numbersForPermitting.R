numbersForPermitting <- function () {}

pkgs <- c("tidyverse")
sapply(pkgs, require, character = T)

#
# d1 <- read.csv("D:/Whale_Observations_all_pre-mid2023.csv")
# d1 <- d1 %>%
#   select(
#     ObservationID = Observations.obs_id, DateTime = Observations.Observation.time,
#     Longitude = Observations._Record.your.current.location_longitude, Latitude = Observations._Record.your.current.location_latitude,
#     Species = Observations.Species., Other = Observations.Other.Species.,
#     Count = Observations.Count..N.
#   ) %>%
#   mutate(
#     DateTime = lubridate::ymd_hms(DateTime),
#     Year = lubridate::year(DateTime),
#     Species = ifelse(Species == "Other", Other, Species)
#     )


d2 <- read.csv("E:/2024_PNW_data/MMVS_Observations_2024-10-15-03-19-23.csv")
d2 <- d2 %>%
  select(
    ObservationID = Observations.obs_id, DateTime = Observation.time,
    Longitude = X_Record.your.current.location_longitude,
    Latitude =  X_Record.your.current.location_latitude,
    Species = Species., Other = Other.Species.,
    Count = Count..N.,
    Photographed = Observations.Photographed..N.,
    Biopsied = Observations...Biopsy.Summary...Biopsy.taken.,
    BiopsiedN = Observations...Biopsy.Summary...Number.of.Individuals.Attempted.
  ) %>%
  mutate(
    DateTime = lubridate::ymd_hms(DateTime),
    Year = lubridate::year(DateTime),
    Species = ifelse(Species == "Other", Other, Species)
  )



## Numbers reported
year <- 2024
wa_or <- 46.2623
or_ca <- 41.9982


d <- d2 %>%
  #d1 %>%
  #rbind(d2) %>%
  filter(Year == year) %>%
  mutate(
    State = ifelse(Latitude > wa_or, "WA", ifelse(Latitude < or_ca, "CA", "OR"))
  )

ds <- d %>%
  group_by(Species, State) %>%
  summarize(
    N = sum(Count),
    Nphoto = sum(Photographed, na.rm = T),
    Nbiops = sum(BiopsiedN, na.rm = T)
  ) %>%
  arrange(Species,)
View(ds)


d %>% filter(Species == "Orca")



