pkgs <- c("tidyverse", "purrr")
sapply(pkgs, require, character = T)

dir <- "C:/Users/peter.mahoney/Desktop/AstroLogs/"
files <- list.files(dir, "_info.csv", full.names = T)

full <- map_dfr(1:length(files), function(x) {
  read.csv(files[x]) %>%
    mutate(
      flight = x,
      dateTime = as.POSIXct((Unix.Time..ms. / 1000), tz = "America/Los_Angeles")
      ) %>%
    group_by(flight) %>%
    summarize(
      startTime = dateTime[1], DurationMin = as.numeric(dateTime[n()] - dateTime[1]),
      latitude = Latitude[1], longitude = Longitude[1]
    ) %>%
    mutate(
      #gmt = lubridate::with_tz(startTime, tzone = "GMT"),
      PIC = "PJM", VO = "JH", MC = "TO", Instructor = "",
      Date = format(startTime, "%m/%d/%Y"),
      Time = format(startTime, "%H:%M:%S"),
      Year = lubridate::year(startTime),
      NumLandings = 1,
      NIF_no = "N24-35",
      SN = "1092A4E5",
      FAA = "FA3TP7RE9M",
      FlownFrom = "BOAT"
    ) %>%
    select(Flight = flight, PIC, VO, MC, Instructor,
           Date, Year, Time, NumLandings, DurationMin,
           NIF_no, SN, FAA, FlownFrom)
})

write.csv(full, "C:/Users/peter.mahoney/Desktop/AstroLogs.csv", row.names = F)

#UID	PIC VO (SIC)	MC	Instructor (serving as PIC)	Date	year	Start_GMT	Number of Landings	Duration_min	NIF_no	S/N of Platform	FAA Registration #	Flown from

