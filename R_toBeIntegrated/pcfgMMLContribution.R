# Load Required Packages
pkgs <- c('tidyverse', 'lubridate', 'mapboxer')
sapply(pkgs, require, character = T)

# Load CSV version of CRC output
ER <- read.csv('./Data/ER-ID-forMML-through2023.csv') %>%
  dplyr::select(ID, ResearchGroup = Record.Source, Year, Date,
                Sreg = Sreg2, Reg = Reg3., Lat = Dec.Lat, Lon = Dec.Long) %>%
  mutate(Date = dmy(Date),
         Month = month(Date))

# Filter out a few 2021 data
#filter(Year <= 2020)


# Filter season and region
ERx <- ER %>%
  filter(!is.na(Month)) %>%
  mutate(ResearchGroup = if_else(ResearchGroup %in% c("BG", "NMML"), "NMML/BG", ResearchGroup)) %>%
  filter(Month %in% c(6:11) & Reg %in% c(2:7, 10:12))




# Unique IDs by year across all research groups
yr <- 2022
uIDs <- purrr::map(yr:max(ERx$Year), function (x) {
  iIDs <- ERx %>%
    filter(Year == x) %>%
    pull(ID) %>% unique()

  return(list(iIDs))
})

summ <- purrr::map_dfr(yr:max(ERx$Year), function (x) {
  iRG <- ERx %>%
    filter(Year == x) %>%
    pull(ResearchGroup) %>% unique()

  out <- purrr::map_dfr(iRG, function (y) {
    iIDs <- ERx %>%
      filter(Year == x & ResearchGroup != y) %>%
      pull(ID) %>% unique()

    ERx %>%
      filter(Year == x & ResearchGroup == y) %>%
      group_by(ResearchGroup, Year) %>%
      summarize(
        nEnc = n(),
        uIDs = length(unique(ID)),
        uuIDs = sum(!(unique(ID) %in% iIDs))
      ) %>%
      arrange(Year, -uuIDs)
  }) %>%
    arrange(Year, -uuIDs)

  return(out)
})

# summ <- ERx %>%
#   filter(Year >= yr) %>%
#   group_by(ResearchGroup, Year) %>%
#   summarize(
#     nEnc = n(),
#     uIDs = length(unique(ID)),
#   ) %>%
#   arrange(Year, -uIDs)
# View(summ)

cn <- summ %>%
  filter(ResearchGroup == "CN" & Year >= 2010) %>%
  summarize(
    nEnc = sum(nEnc),
    mnUIDs = mean(uIDs),
    mnUUIDs = mean(uuIDs)
  )

mml <- summ %>%
  filter(ResearchGroup %in% "NMML/BG" & Year >= 2010) %>%
  summarize(
    nEnc = sum(nEnc),
    mnUIDs = mean(uIDs),
    mnUUIDs = mean(uuIDs)
  )
