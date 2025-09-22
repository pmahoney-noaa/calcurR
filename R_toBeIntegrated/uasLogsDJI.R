pkgs <- c("tidyverse", "sf", 'purrr', 'furrr', 'plotly', 'lubridate')
sapply(pkgs, require, character = T)

# For jobs in parallel
future::plan("multisession", workers = 6)

# FlightReader Repository
dir <- "C:/Users/pmaho/Documents/FlightReader/Repository/Aircraft/"

# Converted flight logs
files <- list.files(dir, "*-aircraft.csv", recursive = T)

# Read files
#files <- files[1:5]
logs <- imap_dfr(
  files,
  ~ read.csv(paste0(dir, .x)) %>%
    mutate(Flight = .y) %>%
    dplyr::select(
      Flight,
      CUSTOM.date.local = CUSTOM.date..local.,
      CUSTOM.updateTime.local = CUSTOM.updateTime..local.,
      starts_with("OSD."),
      DETAILS.appName,
      DETAILS.appVersion,
      DETAILS.aircraftName,
      DETAILS.aircraftSerial,
      SERIAL.rc,
      SERIAL.battery
    ),
  .progress = T
)


# Flight params
PICs <- c("PJM", "JH")
VOs <- c("PJM", "JH")
MC <- c("TO")
TOL <- 1
SOFA <- "N25-34"
sns <- c("1581F45TB21BR2AE", "1581F67QE2359001")
regs <- c("FA33FKWNFR", "FA3H3H73AM")

# MML Flight tracking
# PIC, VO, MC, Trainer, Date, Year, Time, TOL, Minutes, SOFA, S/N, FAA Reg, Boat/Land, Comments

flightSumm <- logs %>%
  mutate(
    Date = mdy(CUSTOM.date.local),
    Year = year(Date),
    Time = parse_date_time(CUSTOM.updateTime.local, "HMS p"),
    Minutes = OSD.flyTime..s. / 60,
    SN = DETAILS.aircraftSerial
  ) %>%
  filter(
    Year == 2025
  ) %>%
  group_by(Flight) %>%
  summarize(
    PIC = NA,
    VO = NA,
    MC = "TO",
    Trainer = "",
    Date = unique(Date),
    Year = min(Year),
    Time = format(Time[1], "%H:%M:%S"),
    TOL = TOL,
    Minutes = max(Minutes),
    SOFA = SOFA,
    SN = unique(SN),
    FAA_reg = NA,
    Base = "Boat",
    Comments = ""
  )

flightSumm <- flightSumm %>%
  filter(month(Date) > 8) %>%
  mutate(
    Date = format(Date, "%m/%d/%Y"),
    Minutes = round(Minutes, 1)
  ) %>%
  mutate(PIC = rep(PICs, length.out = n())) %>%
  mutate(VO = if_else(PIC == "PJM", "JH", "PJM")) %>%
  mutate(FAA_reg = if_else(SN == "1581F45TB21BR2AE", "FA33FKWNFR", FAA_reg)) %>%
  mutate(FAA_reg = if_else(SN == "1581F67QE2359001", "FA3H3H73AM", FAA_reg)) %>%
  filter(Minutes >= 1 & !is.na(FAA_reg))

write.csv(
  flightSumm,
  "C:/Users/pmaho/Desktop/2025_MML_flightRecords_PNW_Sept.csv",
  row.names = F
)
