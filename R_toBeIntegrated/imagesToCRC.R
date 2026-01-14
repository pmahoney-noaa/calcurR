pkgs <- c(
  "tidyverse",
  "RSQLite",
  "lubridate",
  "sf",
  "glue",
  "exifr",
  "purrr",
  "XML",
  "hms"
)
sapply(pkgs, require, character = T)

## Other functions
source("./R_toBeIntegrated/srtToDf.R")

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
      tmin <- data.frame(
        DateTime_Local = x,
        Longitude = NA,
        Latitude = NA,
        diffT = NA
      )
    } else {
      tmin <- tmin %>%
        mutate(diffT = abs(as.numeric(DateTime_Local - x))) %>%
        filter(diffT == min(diffT)) %>%
        slice(1) %>%
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
conn <- dbConnect(RSQLite::SQLite(), dbLoc, flags = SQLITE_RO)
dbListTables(conn)

dbGetQuery(conn, "SELECT * FROM AlbumRoots") # Root directory
dbGetQuery(conn, "SELECT * FROM Albums") # Album Info
dbGetQuery(conn, "SELECT * FROM ImageInformation") # Rating, format, times, Height, width, orientation
dbGetQuery(conn, "SELECT * FROM ImageMetadata") # Camera make, model, lens, settings
dbGetQuery(conn, "SELECT * FROM VideoMetadata") # Camera make, model, lens, settings
dbGetQuery(conn, "SELECT * FROM ImagePositions") # Lat, long, alt
dbGetQuery(conn, "SELECT * FROM ImageTags") # Image tagid (numeric), long format
dbGetQuery(conn, "SELECT * FROM Images") # Album, name, modDate, size
dbGetQuery(conn, "SELECT * FROM Tags") # tag id, pid, name

# Build query to pull for ID
#spp <- "Gray Whale"
spp <- "Humpback Whale"

# Parent ID for gray whales
pid <- dbGetQuery(
  conn,
  "SELECT id, name FROM Tags
                         WHERE name = ?",
  spp
) %>%
  pull(id)

# # Tag ID for pulling images "For ID"
# tid <- dbGetQuery(conn, "SELECT id, pid, name FROM Tags
#                          WHERE name = 'For ID' AND pid = ?", pid) %>% pull(id)

# Tag ID for pulling images e.g.: "Oblique" and "Fluke"
tid <- dbGetQuery(
  conn,
  #  "SELECT id, pid, name FROM Tags
  #                         WHERE pid = ? AND name != 'Aerial'",
  "SELECT id, pid, name FROM Tags
                         WHERE pid = ? AND name == 'Fluke'",
  pid
) %>%
  pull(id)

# Pull image ids that intended for matching
iid <- dbGetQuery(
  conn,
  "SELECT imageid FROM ImageTags
                         WHERE tagid = ?",
  list(tid)
) %>%
  pull(imageid)

##
### Full Query
##

## Just images
#  "SELECT Images.id, Images.name,
#                          ImageInformation.creationDate, ImageInformation.format,
#                          ImageMetadata.model, ImageMetadata.lens, ImageMetadata.aperture, ImageMetadata.focalLength35, ImageMetadata.exposureTime, ImageMetadata.sensitivity,
#                          ImagePositions.latitudeNumber, ImagePositions.longitudeNumber, ImagePositions.altitude,
#                          Images.album, AlbumRoots.specificPath, Albums.relativePath
#                  FROM Images
#                  JOIN ImageInformation ON Images.id = ImageInformation.imageid
#                  JOIN ImageMetadata ON Images.id = ImageMetadata.imageid
#                  LEFT OUTER JOIN ImagePositions ON Images.id = ImagePositions.imageid
#                  JOIN Albums ON Images.album = Albums.id
#                  JOIN AlbumRoots ON Albums.albumRoot = AlbumRoots.id"

imgs <- dbGetQuery(
  conn,
  "SELECT Images.id, Images.name,
                          ImageInformation.creationDate, ImageInformation.format,
                          ImageMetadata.model, ImageMetadata.lens, ImageMetadata.aperture, ImageMetadata.focalLength35, ImageMetadata.exposureTime, ImageMetadata.sensitivity,
                          VideoMetadata.aspectRatio, VideoMetadata.duration, VideoMetadata.frameRate, VideoMetadata.videoCodec,
                          ImagePositions.latitudeNumber, ImagePositions.longitudeNumber, ImagePositions.altitude,
                          Images.album, AlbumRoots.specificPath, Albums.relativePath, Images.fileSize
                  FROM Images
                  LEFT OUTER JOIN ImageInformation ON Images.id = ImageInformation.imageid
                  LEFT OUTER JOIN ImageMetadata ON Images.id = ImageMetadata.imageid
                  LEFT OUTER JOIN VideoMetadata ON Images.id = VideoMetadata.imageid
                  LEFT OUTER JOIN ImagePositions ON Images.id = ImagePositions.imageid
                  LEFT OUTER JOIN Albums ON Images.album = Albums.id
                  LEFT OUTER JOIN AlbumRoots ON Albums.albumRoot = AlbumRoots.id"
)
imgTags <- dbGetQuery(
  conn,
  "SELECT *
                      FROM ImageTags
                      JOIN Tags ON ImageTags.tagid = Tags.id"
) %>%
  filter(!grepl("None", name)) %>%
  #slice(1:500) %>%
  group_by(imageid) %>%
  summarize(Tags = paste0(name, collapse = "/"), )


# Subset images for submission
startDate <- lubridate::ymd_hms("2024-01-01 00:00:00")
endDate <- lubridate::ymd_hms("2026-01-01 00:00:00")
io <- imgs %>%
  mutate(
    dateTime = lubridate::ymd_hms(creationDate, tz = "America/Los_Angeles")
  ) %>%
  left_join(imgTags, by = join_by(id == imageid)) %>%
  filter(
    id %in% iid & dateTime >= startDate & dateTime <= endDate
  ) %>%
  filter(!is.na(specificPath))

##
### Remove thermal and standard resolution videos
##
io <- io %>% filter(!(grepl("*_T.*", name) | grepl("*_S.*", name)))

##
### Video summaries
##
# Video summary
(vidSumm <- io %>%
  filter(!is.na(duration)) %>%
  mutate(
    month = month(ymd_hms(creationDate)),
    year = year(ymd_hms(creationDate))
  ) %>%
  group_by(year, month) %>%
  dplyr::summarize(
    totalVids = n(),
    totalDuration_min = (sum(as.numeric(duration)) / 1000) / 60,
    totalSize_gb = sum(as.numeric(fileSize) / 1E9)
  ))

# Image summary
(datSumm <- io %>%
  mutate(
    month = month(ymd_hms(creationDate)),
    year = year(ymd_hms(creationDate))
  ) %>%
  group_by(year, month) %>%
  dplyr::summarize(
    totalImages = sum(is.na(duration)),
    totalVideos = sum(!is.na(duration)),
    totalImagery = n(),
    totalSize_gb = sum(as.numeric(fileSize) / 1E9)
  ))


##
### Location pairing
##

dates <- getDates(io, "dateTime")

# DJI Videos
i_vids <- which(io$format %in% c("MOV", "MP4"))
for (i in i_vids) {
  # test <- io %>% filter(format == "MP4") %>% slice(1)
  # videoName <- test$name
  # dir = paste0("D:", test$specificPath, test$relativePath)
  # out <- srtToCsv(videoName, dir)
  print(paste0("Working on file: ", io$name[i]))

  yr <- year(io$dateTime[i])

  if (yr == 2024) {
    drv <- "D:"
  } else if (yr == 2025) {
    drv <- "E:"
  }

  srt <- srtToDf(
    io$name[i],
    paste0(drv, io$specificPath[i], io$relativePath[i])
  )

  io$longitudeNumber[i] <- srt$longitude[1]
  io$latitudeNumber[i] <- srt$latitude[1]
  io$altitude[i] <- srt$rel_alt[1]
}


# Capt's Logger
#files <- list.files("D:/2024_PNW_data/CaptainsLogger", "CaptainsLog")
files <- list.files("C:/Users/pmaho/Desktop/CaptainsLogger", "CaptainsLog")

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

sdf <- files %>%
  map_df(
    ~ read_csv(paste("C:/Users/pmaho/Desktop/CaptainsLogger", ., sep = "/"))
  ) %>%
  mutate(
    DateTime_GMT = lubridate::mdy_hms(DateTime_GMT, tz = "GMT"),
    DateTime_Local = lubridate::with_tz(
      DateTime_GMT,
      tzone = "Etc/GMT+7"
    )
  ) %>%
  filter(!is.na(DateTime_Local))

locsCL <- matchTimesForLocation(io, sdf, tbuffer = 60)


# Garmin
#gpxFiles <- list.files("D:/2024_PNW_data/Garmin/", "GPX", recursive = T)
gpxFiles <- list.files("C:/Users/pmaho/Desktop/Garmin/", "GPX", recursive = T)
gpx <- gpxFiles %>%
  map_df(
    ~ sf::st_read(
      paste("C:/Users/pmaho/Desktop/Garmin", ., sep = "/"),
      layer = "track_points"
    )
  )

gpxo <- gpx %>%
  filter(!is.na(time)) %>%
  mutate(
    Longitude = st_coordinates(.)[, 1],
    Latitude = st_coordinates(.)[, 2],
    source = "Garmin Chartplotter",
    dateTime_local = lubridate::ymd_hms(time, tz = "Etc/GMT+7"),
    dateTime_GMT = lubridate::with_tz(dateTime_local, tz = "GMT")
  ) %>%
  dplyr::select(DateTime_Local = dateTime_local, Longitude, Latitude) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  arrange(DateTime_Local) %>%
  filter(!duplicated(.))

locsGPX <- matchTimesForLocation(io, gpxo, tbuffer = 60)

# Merging location information
locs <- map_df(1:nrow(locsCL), function(x) {
  #locsCL$DateTime_Local
  evalBool <- locsCL %>%
    slice(x) %>%
    pull(Latitude) %>%
    is.na()

  if (evalBool) {
    irec <- locsGPX %>%
      slice(x)
  } else {
    irec <- locsCL %>%
      slice(x)
  }

  return(irec)
})

io_backup <- io
io <- io %>%
  mutate(
    latitudeNumber = ifelse(
      is.na(latitudeNumber),
      locs$Latitude,
      latitudeNumber
    ),
    longitudeNumber = ifelse(
      is.na(longitudeNumber),
      locs$Longitude,
      longitudeNumber
    )
  )

# Output
destDir <- "E:/"
#write.csv(io, file = paste0(destDir, "2024_2025_Mn_MML_io.csv"), row.names = F)
#io <- read.csv(paste0(destDir, "2024_2025_Mn_MML_io.csv"))

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

# 2024 Mn
sourceDrive <- "D:"
destDir <- "F:/2024_Mn_MML/"
dir.create(destDir, recursive = T)

dirs <- io %>%
  filter(year(dateTime) == 2024) %>%
  dplyr::select(relativePath) %>%
  pull(relativePath) %>%
  unique()

for (d in dirs) {
  dir.create(paste0(destDir, d, "/"), recursive = T)
}

dest <- io %>%
  filter(year(dateTime) == 2024) %>%
  dplyr::select(
    id,
    name,
    dateTime,
    specificPath,
    relativePath,
    longitude = longitudeNumber,
    latitude = latitudeNumber
  ) %>%
  mutate(
    date = as.character(lubridate::date(dateTime)),
    localName = paste(
      paste0(sourceDrive, specificPath, relativePath),
      name,
      sep = "/"
    ),
    destName = paste0(destDir, relativePath, "/", name)
  ) %>%
  dplyr::select(-specificPath, -relativePath)

#file.copy(dest$localName, dest$destName, copy.date = TRUE)
future::plan("multisession", workers = 4)
furrr::future_pmap(
  dest %>%
    mutate(recursive = T, copy.date = T) %>%
    dplyr::select(from = localName, to = destName, recursive, copy.date),
  file.copy,
  .progress = T
)

# 2025 Mn
sourceDrive <- "E:"
destDir <- "F:/2025_Mn_MML/"
dir.create(destDir, recursive = T)

dirs <- io %>%
  filter(year(dateTime) == 2025) %>%
  dplyr::select(relativePath) %>%
  pull(relativePath) %>%
  unique()

for (d in dirs) {
  dir.create(paste0(destDir, d, "/"), recursive = T)
}

dest <- io %>%
  filter(year(dateTime) == 2025) %>%
  dplyr::select(
    id,
    name,
    dateTime,
    specificPath,
    relativePath,
    longitude = longitudeNumber,
    latitude = latitudeNumber
  ) %>%
  mutate(
    date = as.character(lubridate::date(dateTime)),
    localName = paste(
      paste0(sourceDrive, specificPath, relativePath),
      name,
      sep = "/"
    ),
    destName = paste0(destDir, relativePath, "/", name)
  ) %>%
  dplyr::select(-specificPath, -relativePath)

#file.copy(dest$localName, dest$destName, copy.date = TRUE)
future::plan("multisession", workers = 4)
furrr::future_pmap(
  dest %>%
    mutate(recursive = T, copy.date = T) %>%
    dplyr::select(from = localName, to = destName, recursive, copy.date),
  file.copy,
  .progress = T
)


##
#### Metadata file for export
##

dest <- io %>%
  #filter(year(dateTime) == 2025) %>%
  dplyr::select(
    id,
    name,
    dateTime,
    specificPath,
    relativePath,
    longitude = longitudeNumber,
    latitude = latitudeNumber
  ) %>%
  mutate(
    date = as.character(lubridate::date(dateTime)),
    localName = paste(
      paste0(sourceDrive, specificPath, relativePath),
      name,
      sep = "/"
    ),
    destName = paste0(destDir, relativePath, "/", name)
  ) %>%
  dplyr::select(-specificPath, -relativePath)

odf <- dest %>%
  rowwise() %>%
  mutate(
    date = date(dateTime),
    port = NA,
    timeLocal = as_hms(ymd_hms(dateTime, tz = "Etc/GMT+7")),
    species = "Humpback Whale",
    dailySeqID = NA,
    cameraRoll = NA,
    imageFileName = NA
  ) %>%
  dplyr::select(
    date,
    port,
    timeLocal,
    species,
    cameraRoll,
    localName,
    name
  )

odf$port <- unlist(lapply(str_split(odf$localName, "/"), function(x) {
  str_split(x[4], "_")[[1]][4]
}))
odf$cameraRoll <- unlist(lapply(str_split(odf$localName, "/"), function(x) {
  x[5]
}))
odf$videoFileName <- ifelse(
  grepl(".JPG$", odf$name, ignore.case = T),
  NA,
  odf$name
)
odf$imageFileName <- ifelse(
  grepl(".JPG$", odf$name, ignore.case = T),
  odf$name,
  NA
)

odf <- odf %>%
  select(
    date,
    port,
    timeLocal,
    species,
    cameraRoll,
    videoFileName,
    imageFileName
  )
write.csv(odf, "E:/2024_2025_Mn_Image_Metadata.csv", row.names = F)


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
### Er Image database connection
##

#dbLoc <- "../ImageDatabase/digikam4.db"
dbLoc <- "c:/Users/pmaho/Documents/ImageDatabase/digikam4.db"
conn <- dbConnect(RSQLite::SQLite(), dbLoc, flags = SQLITE_RO)
dbListTables(conn)

# Build query to pull for ID
spp <- "Gray Whale"

# Parent ID for gray whales
pid <- dbGetQuery(
  conn,
  "SELECT id, name FROM Tags
                         WHERE name = ?",
  spp
) %>%
  pull(id)

# # Tag ID for pulling images "For ID"
# tid <- dbGetQuery(conn, "SELECT id, pid, name FROM Tags
#                          WHERE name = 'For ID' AND pid = ?", pid) %>% pull(id)

# Tag ID for pulling images e.g.: "Oblique" and "Fluke"
tid <- dbGetQuery(
  conn,
  #  "SELECT id, pid, name FROM Tags
  #                         WHERE pid = ? AND name != 'Aerial'",
  "SELECT id, pid, name FROM Tags
                         WHERE pid = ? AND name == 'Oblique'",
  pid
) %>%
  pull(id)

# Pull image ids that intended for matching
iid <- dbGetQuery(
  conn,
  "SELECT imageid FROM ImageTags
                         WHERE tagid = ?",
  list(tid)
) %>%
  pull(imageid)

##
### Full Query
##

imgs <- dbGetQuery(
  conn,
  "SELECT Images.id, Images.name,
                          ImageInformation.creationDate, ImageInformation.format,
                          ImageMetadata.model, ImageMetadata.lens, ImageMetadata.aperture, ImageMetadata.focalLength35, ImageMetadata.exposureTime, ImageMetadata.sensitivity,
                          VideoMetadata.aspectRatio, VideoMetadata.duration, VideoMetadata.frameRate, VideoMetadata.videoCodec,
                          ImagePositions.latitudeNumber, ImagePositions.longitudeNumber, ImagePositions.altitude,
                          Images.album, AlbumRoots.specificPath, Albums.relativePath, Images.fileSize
                  FROM Images
                  LEFT OUTER JOIN ImageInformation ON Images.id = ImageInformation.imageid
                  LEFT OUTER JOIN ImageMetadata ON Images.id = ImageMetadata.imageid
                  LEFT OUTER JOIN VideoMetadata ON Images.id = VideoMetadata.imageid
                  LEFT OUTER JOIN ImagePositions ON Images.id = ImagePositions.imageid
                  LEFT OUTER JOIN Albums ON Images.album = Albums.id
                  LEFT OUTER JOIN AlbumRoots ON Albums.albumRoot = AlbumRoots.id"
)
imgTags <- dbGetQuery(
  conn,
  "SELECT *
                      FROM ImageTags
                      JOIN Tags ON ImageTags.tagid = Tags.id"
) %>%
  filter(!grepl("None", name)) %>%
  #slice(1:500) %>%
  group_by(imageid) %>%
  summarize(Tags = paste0(name, collapse = "/"), )


# Subset images for submission
startDate <- lubridate::ymd_hms("2025-01-01 00:00:00")
endDate <- lubridate::ymd_hms("2026-01-01 00:00:00")
io <- imgs %>%
  mutate(
    dateTime = lubridate::ymd_hms(creationDate, tz = "America/Los_Angeles")
  ) %>%
  left_join(imgTags, by = join_by(id == imageid)) %>%
  filter(
    id %in% iid & dateTime >= startDate & dateTime <= endDate
  ) %>%
  filter(!is.na(specificPath))

##
### Remove thermal and standard resolution videos
##
io <- io %>% filter(!(grepl("*_T.*", name) | grepl("*_S.*", name)))

##
### Video summaries
##
# Video summary
(vidSumm <- io %>%
  filter(!is.na(duration)) %>%
  mutate(
    month = month(ymd_hms(creationDate)),
    year = year(ymd_hms(creationDate))
  ) %>%
  group_by(year, month) %>%
  dplyr::summarize(
    totalVids = n(),
    totalDuration_min = (sum(as.numeric(duration)) / 1000) / 60,
    totalSize_gb = sum(as.numeric(fileSize) / 1E9)
  ))

# Image summary
(datSumm <- io %>%
  mutate(
    month = month(ymd_hms(creationDate)),
    year = year(ymd_hms(creationDate))
  ) %>%
  group_by(year, month) %>%
  dplyr::summarize(
    totalImages = sum(is.na(duration)),
    totalVideos = sum(!is.na(duration)),
    totalImagery = n(),
    totalSize_gb = sum(as.numeric(fileSize) / 1E9)
  ))


##
### Location pairing
##

dates <- getDates(io, "dateTime")

# DJI Videos
i_vids <- which(io$format %in% c("MOV", "MP4"))
drv <- "E:"
for (i in i_vids) {
  # test <- io %>% filter(format == "MP4") %>% slice(1)
  # videoName <- test$name
  # dir = paste0("D:", test$specificPath, test$relativePath)
  # out <- srtToCsv(videoName, dir)
  print(paste0("Working on file: ", io$name[i]))

  yr <- year(io$dateTime[i])

  srt <- srtToDf(
    io$name[i],
    paste0(drv, io$specificPath[i], io$relativePath[i])
  )

  io$longitudeNumber[i] <- srt$longitude[1]
  io$latitudeNumber[i] <- srt$latitude[1]
  io$altitude[i] <- srt$rel_alt[1]
}


# Capt's Logger
#files <- list.files("D:/2024_PNW_data/CaptainsLogger", "CaptainsLog")
files <- list.files("C:/Users/pmaho/Desktop/CaptainsLogger", "CaptainsLog")

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

sdf <- files %>%
  map_df(
    ~ read_csv(paste("C:/Users/pmaho/Desktop/CaptainsLogger", ., sep = "/"))
  ) %>%
  mutate(
    DateTime_GMT = lubridate::mdy_hms(DateTime_GMT, tz = "GMT"),
    DateTime_Local = lubridate::with_tz(
      DateTime_GMT,
      tzone = "Etc/GMT+7"
    )
  ) %>%
  filter(!is.na(DateTime_Local))

locsCL <- matchTimesForLocation(io, sdf, tbuffer = 60)


# Garmin
#gpxFiles <- list.files("D:/2024_PNW_data/Garmin/", "GPX", recursive = T)
gpxFiles <- list.files("C:/Users/pmaho/Desktop/Garmin/", "GPX", recursive = T)
gpx <- gpxFiles %>%
  map_df(
    ~ sf::st_read(
      paste("C:/Users/pmaho/Desktop/Garmin", ., sep = "/"),
      layer = "track_points"
    )
  )

gpxo <- gpx %>%
  filter(!is.na(time)) %>%
  mutate(
    Longitude = st_coordinates(.)[, 1],
    Latitude = st_coordinates(.)[, 2],
    source = "Garmin Chartplotter",
    dateTime_local = lubridate::ymd_hms(time, tz = "Etc/GMT+7"),
    dateTime_GMT = lubridate::with_tz(dateTime_local, tz = "GMT")
  ) %>%
  dplyr::select(DateTime_Local = dateTime_local, Longitude, Latitude) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  arrange(DateTime_Local) %>%
  filter(!duplicated(.))

locsGPX <- matchTimesForLocation(io, gpxo, tbuffer = 60)

# Merging location information
locs <- map_df(1:nrow(locsCL), function(x) {
  #locsCL$DateTime_Local
  evalBool <- locsCL %>%
    slice(x) %>%
    pull(Latitude) %>%
    is.na()

  if (evalBool) {
    irec <- locsGPX %>%
      slice(x)
  } else {
    irec <- locsCL %>%
      slice(x)
  }

  return(irec)
})

io_backup <- io
io <- io %>%
  mutate(
    latitudeNumber = ifelse(
      is.na(latitudeNumber),
      locs$Latitude,
      latitudeNumber
    ),
    longitudeNumber = ifelse(
      is.na(longitudeNumber),
      locs$Longitude,
      longitudeNumber
    )
  )

# Output
destDir <- "F:/"
write.csv(io, file = paste0(destDir, "2025_Er_MML_io.csv"), row.names = F)
#io <- read.csv(paste0(destDir, "2025_Er_MML_io.csv"))

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

# 2025 Er
sourceDrive <- "E:"
destDir <- "F:/2025_Er_MML/"
dir.create(destDir, recursive = T)

dirs <- io %>%
  #filter(year(dateTime) == 2025) %>%
  dplyr::select(relativePath) %>%
  pull(relativePath) %>%
  unique()

for (d in dirs) {
  dir.create(paste0(destDir, d, "/"), recursive = T)
}

dest <- io %>%
  #filter(year(dateTime) == 2025) %>%
  dplyr::select(
    id,
    name,
    dateTime,
    specificPath,
    relativePath,
    longitude = longitudeNumber,
    latitude = latitudeNumber
  ) %>%
  mutate(
    date = as.character(lubridate::date(dateTime)),
    localName = paste(
      paste0(sourceDrive, specificPath, relativePath),
      name,
      sep = "/"
    ),
    destName = paste0(destDir, relativePath, "/", name)
  ) %>%
  dplyr::select(-specificPath, -relativePath)

#file.copy(dest$localName, dest$destName, copy.date = TRUE)
future::plan("multisession", workers = 4)
furrr::future_pmap(
  dest %>%
    mutate(recursive = T, copy.date = T) %>%
    dplyr::select(from = localName, to = destName, recursive, copy.date),
  file.copy,
  .progress = T
)


##
#### Metadata file for export
##

odf <- dest %>%
  rowwise() %>%
  mutate(
    date = date(dateTime),
    port = NA,
    timeLocal = as_hms(dateTime),
    species = "Gray Whale",
    dailySeqID = NA,
    cameraRoll = NA,
    imageFileName = NA
  ) %>%
  dplyr::select(
    date,
    port,
    timeLocal,
    species,
    cameraRoll,
    localName,
    name
  )

odf$port <- unlist(lapply(str_split(odf$localName, "/"), function(x) {
  str_split(x[4], "_")[[1]][4]
}))
odf$cameraRoll <- unlist(lapply(str_split(odf$localName, "/"), function(x) {
  x[5]
}))
odf$videoFileName <- ifelse(
  grepl(".JPG$", odf$name, ignore.case = T),
  NA,
  odf$name
)
odf$imageFileName <- ifelse(
  grepl(".JPG$", odf$name, ignore.case = T),
  odf$name,
  NA
)

odf <- odf %>%
  select(
    date,
    port,
    timeLocal,
    species,
    cameraRoll,
    videoFileName,
    imageFileName
  )
write.csv(odf, "E:/2025_Er_Image_Metadata.csv", row.names = F)


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
    dateTime_GMT = lubridate::ymd_hms(
      Observations.Observation.time,
      tz = "GMT"
    ),
    dateTime_Local = lubridate::with_tz(
      dateTime_GMT,
      tzone = "America/Los_Angeles"
    ),
    year = lubridate::year(dateTime_GMT),
    startFrame = NA,
    JHC = NA,
    PMC = NA,
  ) %>%
  filter(
    Observations.Species. == "Gray Whale" &
      lubridate::date(dateTime_GMT) %in% dates
  ) %>%
  dplyr::select(
    Species = Observations.Species.,
    dateTime_GMT,
    dateTime_Local,
    Count = Observations.Count..N.,
    Photographed = Observations.Photographed..N.,
    PMC,
    JHC,
    startFrame,
    endFrame = Observations.Photo.frames,
    Latitude = Observations._Record.your.current.location_latitude,
    Longitude = Observations._Record.your.current.location_longitude,
    Comments = Observations.Comments
  )

# New form implemented in 2023
ff <- read.csv("F:/2024_PNW_data/MMVS_PhotoPt1_2024-10-15-03-19-23.csv") %>% #read.csv("D:/MMVS_PhotoFrames_2023-11-01-17-38-35.csv") %>%
  rename(
    mindex = X_parent_index,
    sFrame = Observations...Camera.Summary...photo_section_table.Start.Frame,
    eFrame = Observations...Camera.Summary...photo_section_table.End.Frame
  ) %>%
  dplyr::select(mindex, sFrame, eFrame)
# fs <- read.csv("D:/MMVS_PhotoSections_2023-11-01-17-38-35.csv") %>%
#   rename(
#     mindex = X_parent_index,
#     Left = Sections.photographed.Left,
#     Right = Sections.photographed.Right,
#     Fluke = Sections.photographed.Fluke,
#     numberSections = Number.of.Individuals) %>%
#   dplyr::select(mindex, Left, Right, Fluke, numberSections)

dfn <- read.csv(
  "F:/2024_PNW_data/MMVS_Observations_2024-10-15-03-19-23.csv"
) %>% #read.csv("D:/MMVS_Observations_2023-11-01-17-38-35.csv") %>%
  mutate(
    dateTime_GMT = lubridate::ymd_hms(Observation.time, tz = "GMT"),
    dateTime_Local = lubridate::with_tz(
      dateTime_GMT,
      tzone = "America/Los_Angeles"
    ),
    year = lubridate::year(dateTime_GMT),
    startFrame = NA,
    endFrame = NA
  ) %>%
  filter(
    #Species. == "Gray Whale" & lubridate::date(dateTime_GMT) %in% dates
    Species. == "Humpback Whale" & lubridate::date(dateTime_GMT) %in% dates
  ) %>%
  dplyr::select(
    Species = Species.,
    dateTime_GMT,
    dateTime_Local,
    Count = Count..N.,
    Photographed = Observations.Photographed..N.,
    PMC = Observations...Camera.Summary...Camera.Used.PMC,
    JHC = Observations...Camera.Summary...Camera.Used.JHC,
    startFrame,
    endFrame,
    Latitude = X_Record.your.current.location_latitude,
    Longitude = X_Record.your.current.location_longitude,
    Comments = Observations.Observation.Comments.Comments,
    mindex = X_index
  ) #%>%
#left_join(ff, by = "mindex") #%>%
#left_join(fs, by = "mindex")

dfn <- dfn %>%
  mutate(
    startFrame = sFrame,
    endFrame = eFrame
  ) %>%
  dplyr::select(
    -mindex,
    -sFrame,
    -eFrame
  )

dfo <- dfn
#dfo <- rbind(df, dfn)
#write.csv(dfo, "F:/2024_MML_Er_Observations.csv")
write.csv(dfo, "F:/2024_MML_Mn_Observations.csv")
