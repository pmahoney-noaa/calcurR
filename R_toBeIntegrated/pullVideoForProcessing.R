pkgs <- c(
  "tidyverse",
  "RSQLite",
  "lubridate",
  "sf",
  "glue",
  "exifr",
  "purrr",
  "furrr",
  "XML",
  "hms"
)
sapply(pkgs, require, character = T)


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

# Tag ID for pulling images "Aerial"
tid <- dbGetQuery(
  conn,
  "SELECT id, pid, name FROM Tags
                         WHERE pid = ? AND name = 'Aerial'",
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
                      LEFT OUTER JOIN Tags ON ImageTags.tagid = Tags.id"
) %>%
  filter(!grepl("None", name)) %>%
  #slice(1:500) %>%
  group_by(imageid) %>%
  summarize(Tags = paste0(name, collapse = "/"), )


# Subset images for submission
startDate <- lubridate::ymd_hms("2024-06-01 00:00:00")
endDate <- lubridate::ymd_hms("2024-10-01 00:00:00")
io <- imgs %>%
  mutate(
    dateTime = lubridate::ymd_hms(creationDate, tz = "America/Los_Angeles")
  ) %>%
  left_join(imgTags, by = join_by(id == imageid)) %>%
  filter(
    #id %in% iid & # Filter out specific group/tag
    dateTime >= startDate & dateTime <= endDate
  ) %>%
  filter(!is.na(specificPath))

# Video summary
(vidSumm <- io %>%
  filter(!is.na(duration)) %>%
  mutate(
    month = month(ymd_hms(creationDate)),
    year = year(ymd_hms(creationDate))
  ) %>%
  group_by(month) %>%
  dplyr::summarize(
    totalVids = n(),
    totalDuration_min = (sum(as.numeric(duration)) / 1000) / 60,
    totalSize_gb = sum(as.numeric(fileSize) / 1E9)
  ))

# Volume summary
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
### Copy into daily folders and add location EXIF
##

destDir <- "D:/2024_Er_Aerials"
dir.create(destDir, recursive = T)

dirs <- io %>%
  dplyr::select(relativePath) %>%
  pull(relativePath) %>%
  unique()

for (d in dirs) {
  dir.create(paste0(destDir, d, "/"), recursive = T)
}

dest <- io %>%
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
      paste0("F:", specificPath, relativePath),
      name,
      sep = "/"
    ),
    destName = paste0(destDir, relativePath, "/", name)
  ) %>%
  dplyr::select(-specificPath, -relativePath)

#file.copy(dest$localName, dest$destName, copy.date = TRUE)

plan(multisession, workers = 4)
furrr::future_map(
  1:nrow(dest),
  ~ file.copy(dest$localName[.x], dest$destName[.x], copy.date = TRUE),
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
    uasPlatform = NA,
    imageFileName = NA
  ) %>%
  dplyr::select(
    date,
    port,
    timeLocal,
    species,
    uasPlatform,
    localName,
    name
  )

odf$port <- unlist(lapply(str_split(odf$localName, "/"), function(x) {
  str_split(x[4], "_")[[1]][4]
}))
odf$uasPlatform <- unlist(lapply(str_split(odf$localName, "/"), function(x) {
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
    uasPlatform,
    videoFileName,
    imageFileName
  )
write.csv(odf, "2024_Er_ImageProcessing_AugSep.csv", row.names = F)
