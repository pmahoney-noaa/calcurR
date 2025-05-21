pkgs <- c("tidyverse", "RSQLite", "lubridate", "sf", "glue", "exifr", "purrr", "XML")
sapply(pkgs, require, character = T)

## Required Functions
getDates <- function(df, col) {
  dates <- lubridate::date(df %>% pull(col))
  return(unique(dates))
}

matchTimesForLocation <- function(idf, ldf, tbuffer = 60) {
  map_df(idf$dateTime, function(x) {
    tmin <- ldf %>%
      filter(DateTime_Local >= (x - tbuffer) & DateTime_Local <= (x + tbuffer))

    if (nrow(tmin) == 0) {
      #tmin <- ldf %>% slice(1) %>% mutate(diffT = NA) %>% select(DateTime_Local, Longitude, Latitude, diffT)
      tmin <- data.frame(DateTime_Local = x, Longitude = NA, Latitude = NA, diffT = NA)
    } else {
      tmin <-  tmin %>%
        mutate(diffT = abs(as.numeric(DateTime_Local - x))) %>%
        filter(diffT == min(diffT)) %>% slice(1) %>%
        select(DateTime_Local, Longitude, Latitude, diffT)
    }

    return(tmin)
  })

}


##
### Image database connection
##

#dbLoc <- "../ImageDatabase/digikam4.db"
dbLoc <- "c:/Users/pmaho/Documents/ImageDatabase/digikam4.db"
conn <- dbConnect(RSQLite::SQLite(), dbLoc, flags=SQLITE_RO)
dbListTables(conn)

dbGetQuery(conn, "SELECT * FROM AlbumRoots")       # Root directory
dbGetQuery(conn, "SELECT * FROM Albums")           # Album Info
dbGetQuery(conn, "SELECT * FROM ImageInformation") # Rating, format, times, Height, width, orientation
dbGetQuery(conn, "SELECT * FROM ImageMetadata")    # Camera make, model, lens, settings
dbGetQuery(conn, "SELECT * FROM ImagePositions")   # Lat, long, alt
dbGetQuery(conn, "SELECT * FROM ImageTags")        # Image tagid (numeric), long format
dbGetQuery(conn, "SELECT * FROM Images")           # Album, name, modDate, size
dbGetQuery(conn, "SELECT * FROM Tags")             # tag id, pid, name

# Build query to pull for ID
spp <- "Gray Whale"

# Parent ID for gray whales
pid <- dbGetQuery(conn, "SELECT id, name FROM Tags
                         WHERE name = ?", spp) %>% pull(id)

# # Tag ID for pulling images "For ID"
# tid <- dbGetQuery(conn, "SELECT id, pid, name FROM Tags
#                          WHERE name = 'For ID' AND pid = ?", pid) %>% pull(id)

# Tag ID for pulling images "Oblique" and "Fluke"
tid <- dbGetQuery(conn, "SELECT id, pid, name FROM Tags
                         WHERE pid = ? AND name != 'Aerial'", pid) %>% pull(id)

# Pull image ids that intended for matching
iid <- dbGetQuery(conn, "SELECT imageid FROM ImageTags
                         WHERE tagid = ?", list(tid)) %>% pull(imageid)

##
### Full Query
##
imgs <- dbGetQuery(conn, "SELECT Images.id, Images.name,
                          ImageInformation.creationDate, ImageInformation.format,
                          ImageMetadata.model, ImageMetadata.lens, ImageMetadata.aperture, ImageMetadata.focalLength35, ImageMetadata.exposureTime, ImageMetadata.sensitivity,
                          ImagePositions.latitudeNumber, ImagePositions.longitudeNumber, ImagePositions.altitude,
                          Images.album, AlbumRoots.specificPath, Albums.relativePath
                  FROM Images
                  JOIN ImageInformation ON Images.id = ImageInformation.imageid
                  JOIN ImageMetadata ON Images.id = ImageMetadata.imageid
                  LEFT OUTER JOIN ImagePositions ON Images.id = ImagePositions.imageid
                  JOIN Albums ON Images.album = Albums.id
                  JOIN AlbumRoots ON Albums.albumRoot = AlbumRoots.id")
imgTags <- dbGetQuery(conn, "SELECT *
                      FROM ImageTags
                      JOIN Tags ON ImageTags.tagid = Tags.id") %>%
  filter(!grepl("None", name)) %>%
  #slice(1:500) %>%
  group_by(imageid) %>%
  summarize(Tags = paste0(name, collapse = "/"), )


# Subset images for submission
startDate <- lubridate::ymd_hms("2024-01-01 00:00:00")
endDate <- lubridate::ymd_hms("2025-01-01 00:00:00")
io <- imgs %>%
  mutate(dateTime = lubridate::ymd_hms(creationDate, tz = "America/Los_Angeles")) %>%
  left_join(imgTags, by = join_by(id == imageid)) %>%
  filter(
    id %in% iid &
      dateTime >= startDate &
      dateTime <= endDate)




##
### Location pairing
##

dates <- getDates(io, "dateTime")

# Capt's Logger
files <- list.files("F:/2024_PNW_data/CaptainsLogger", "CaptainsLog")

# fi <- sapply(format(dates, "%m%d%Y"), FUN = function(x) {
#   grep(x, files, value = T)
# })
#
# sdf <- fi %>%
#   Filter(length, .) %>% # Removes days without an active CaptainsLogger
# map_df(~read_csv(paste("D:/2023_PNW_data/CaptainsLogger", ., sep = "/"))) %>%
# mutate(
#   DateTime_GMT = lubridate::mdy_hms(DateTime_GMT, tz = "GMT"),
#   DateTime_Local = lubridate::with_tz(DateTime_GMT, tzone = "America/Los_Angeles")
# )

sdf <-  files %>%
  map_df(~read_csv(paste("F:/2024_PNW_data/CaptainsLogger", ., sep = "/"))) %>%
  mutate(
    DateTime_GMT = lubridate::mdy_hms(DateTime_GMT, tz = "GMT"),
    DateTime_Local = lubridate::with_tz(DateTime_GMT, tzone = "America/Los_Angeles")
  )

locsCL <- matchTimesForLocation(io, sdf, tbuffer = 60)


# Garmin
gpxFiles <- list.files("F:/2024_PNW_data/Garmin/", "GPX", recursive = T)
gpx <-  gpxFiles %>%
  map_df(~sf::st_read(paste("F:/2024_PNW_data/Garmin", ., sep = "/"), layer = "track_points"))

gpxo <- gpx %>%
  filter(!is.na(time)) %>%
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude = st_coordinates(.)[,2]
  ) %>%
  dplyr::select(DateTime_Local = time, Longitude, Latitude) %>%
  as.data.frame() %>% dplyr::select(-geometry) %>%
  filter(!duplicated(.))

locsGPX <- matchTimesForLocation(io, gpxo, tbuffer = 60)

# Merging location information
locs <- map_df(1:nrow(locsCL), function (x) { #locsCL$DateTime_Local
  evalBool <- locsCL %>%
    slice(x) %>% pull(Latitude) %>% is.na()

  if (evalBool) {
    irec <- locsGPX %>%
      slice(x)
  } else {
    irec <- locsCL %>%
      slice(x)
  }

  return(irec)
})


io <- io %>%
  mutate(
    latitudeNumber = ifelse(is.na(latitudeNumber), locs$Latitude, latitudeNumber),
    longitudeNumber = ifelse(is.na(longitudeNumber), locs$Longitude, longitudeNumber)
  )

##
### for merging with CRC image set
##
# ioo <- io %>%
#   separate(
#     relativePath, c("A", "B", "C"), "/", remove = F
#   ) %>%
#   dplyr::select(
#     rec = id, survey = B, cameraRoll = C, imageFile = name, dateTime = dateTime, latitude = latitudeNumber, longitude = longitudeNumber, relativePath
#   )
# write.csv(ioo, "F:/2024_MML_Er_Images.csv", row.names = F)


##
### Copy into daily folders and add location EXIF
##

destDir <- "D:/2024_Er_MML/Originals"
dir.create(destDir, recursive = T)

dirs <- io %>%
  dplyr::select(relativePath) %>% pull(relativePath) %>%
  unique()

for (d in dirs) {
  dir.create(paste0(destDir, d, "/"), recursive = T)
}

dest <- io %>%
  dplyr::select(
    id, name, dateTime, specificPath, relativePath,
    longitude = longitudeNumber, latitude = latitudeNumber
  ) %>%
  mutate(
    date = as.character(lubridate::date(dateTime)),
    localName = paste(paste0("F:", specificPath, relativePath), name, sep = "/"),
    destName = paste0(destDir, relativePath, "/", name)
  ) %>%
  dplyr::select(-specificPath, -relativePath)

file.copy(dest$localName, dest$destName, copy.date = TRUE)

# # Add location to EXIF (if doesn't exist)
# write_exif <- function(df) {
#   #exiftool_call("-ISO=3200", "test.tif")
#   lapply(1:nrow(df), function (x) {
#     lonRef <- ifelse(df$longitude[x] <= 0, "W", "E")
#     latRef <- ifelse(df$latitude[x] <= 0, "S", "N")
#     exiftool_call(paste(paste0("-GPSLongitude=", double_quote(abs(df$longitude[x]))),
#                         paste0("-GPSLongitudeRef=", double_quote(lonRef)),
#                         paste0("-GPSLatitude=", double_quote(abs(df$latitude[x]))),
#                         paste0("-GPSLatitudeRef=", double_quote(latRef))), df$destName[x])
#   })
# }
# write_exif(dest)

##
### Field Notes
##
# Old form
df <- read.csv("D:/Whale_Observations_all_pre-mid2023.csv") %>%
  mutate(
    dateTime_GMT = lubridate::ymd_hms(Observations.Observation.time, tz = "GMT"),
    dateTime_Local = lubridate::with_tz(dateTime_GMT, tzone = "America/Los_Angeles"),
    year = lubridate::year(dateTime_GMT),
    startFrame = NA, JHC = NA, PMC = NA,
  ) %>%
  filter(
    Observations.Species. == "Gray Whale" & lubridate::date(dateTime_GMT) %in% dates
  ) %>%
  dplyr::select(
    Species = Observations.Species.,
    dateTime_GMT, dateTime_Local,
    Count = Observations.Count..N., Photographed = Observations.Photographed..N.,
    PMC, JHC,
    startFrame,
    endFrame = Observations.Photo.frames,
    Latitude = Observations._Record.your.current.location_latitude,
    Longitude = Observations._Record.your.current.location_longitude,
    Comments = Observations.Comments
  )

# New form implemented in 2023
ff <- read.csv("F:/2024_PNW_data/MMVS_PhotoPt1_2024-10-15-03-19-23.csv") %>%#read.csv("D:/MMVS_PhotoFrames_2023-11-01-17-38-35.csv") %>%
  rename(
    mindex = X_parent_index,
    sFrame = Observations...Camera.Summary...photo_section_table.Start.Frame,
    eFrame = Observations...Camera.Summary...photo_section_table.End.Frame) %>%
  dplyr::select(mindex, sFrame, eFrame)
# fs <- read.csv("D:/MMVS_PhotoSections_2023-11-01-17-38-35.csv") %>%
#   rename(
#     mindex = X_parent_index,
#     Left = Sections.photographed.Left,
#     Right = Sections.photographed.Right,
#     Fluke = Sections.photographed.Fluke,
#     numberSections = Number.of.Individuals) %>%
#   dplyr::select(mindex, Left, Right, Fluke, numberSections)

dfn <- read.csv("F:/2024_PNW_data/MMVS_Observations_2024-10-15-03-19-23.csv") %>%#read.csv("D:/MMVS_Observations_2023-11-01-17-38-35.csv") %>%
  mutate(
    dateTime_GMT = lubridate::ymd_hms(Observation.time, tz = "GMT"),
    dateTime_Local = lubridate::with_tz(dateTime_GMT, tzone = "America/Los_Angeles"),
    year = lubridate::year(dateTime_GMT),
    startFrame = NA, endFrame = NA
  ) %>%
  filter(
    Species. == "Gray Whale" & lubridate::date(dateTime_GMT) %in% dates
  ) %>%
  dplyr::select(
    Species = Species.,
    dateTime_GMT, dateTime_Local,
    Count = Count..N., Photographed = Observations.Photographed..N.,
    PMC = Observations...Camera.Summary...Camera.Used.PMC, JHC = Observations...Camera.Summary...Camera.Used.JHC,
    startFrame, endFrame,
    Latitude = X_Record.your.current.location_latitude,
    Longitude = X_Record.your.current.location_longitude,
    Comments = Observations.Observation.Comments.Comments,
    mindex = X_index
  ) %>%
  left_join(ff, by = "mindex") #%>%
#left_join(fs, by = "mindex")

dfn <- dfn %>%
  mutate(
    startFrame = sFrame, endFrame = eFrame
  ) %>%
  dplyr::select(
    -mindex, -sFrame, -eFrame
  )

dfo <- dfn
#dfo <- rbind(df, dfn)
write.csv(dfo, "F:/2024_MML_Er_Observations.csv")
