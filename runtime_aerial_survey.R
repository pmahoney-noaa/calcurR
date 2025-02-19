pkgs <- c("tidyverse", "future", "furrr", "lubridate", "sf", "glue",
          "exifr", 'geosphere', "mapboxer", "calcurR")
sapply(pkgs, require, character = T)

####
#### Add site information to the exif data
####
# Assign number of cores to be used in parallel jobs
# Includes reading exif, adding exif data, and copying image files
future::plan("multisession", workers = 6)

# Set working directory
setwd("E:/2024_HarborSeal_Aerial_Survey/")

# Metadata
md <- read.csv('./metaData/June14_VSFB_ES_SouthBay_photolog_v2.csv')

if ("new_frame_count" %in% names(md)) {
  md <- md %>% mutate(frame_count = new_frame_count)
}

# Visual check of track log
# md %>%
#   as_mapbox_source(lng = "Longitude", lat = "Latitude") %>%
#   # Setup a map with the default source above
#   mapboxer(
#     center = c(md$Longitude[1], md$Latitude[1]),
#     zoom = 10
#   ) %>%
#   # Add a navigation control
#   add_navigation_control() %>%
#   # Add a layer styling the data of the default source
#   add_circle_layer(
#     circle_color = "white",
#     circle_radius = 3,
#   )


# Images
idir <- "./imagery/6.14 Vandenberg-ElkhornSlough-SouthBay/"
imgs <- list.files(idir, ".JPG$")


#
## Compile image exif into data.frame
#
iexif <- pull_exif(paste0(idir, imgs))
# View(iexif)


#
## Merge flight log with exif
#
mdo <- merge_log_exif(md, iexif, by_seq = T)

#
## Image to metadata time pairings
#
merge_tim_delta = 10 # seconds; flag if metadata / image time differences >
seq_tim_delta = 2   # minutes; image sequential difference for considering a break
seq_dist_delta = 100  # meters; image sequential difference for considering a break

dfo <- check_aerial_merge(mdo, merge_tim_delta, seq_tim_delta, seq_dist_delta)

# For view a subset in cases where discrepancies exist
# test <- dfo %>% dplyr::select(utc_time, UTC.time, frame_count,
#                               DateTimeS, latitude, longitude, capture_date,
#                               file_date, merge_diff_sec, seq_diff_sec, seq_dist_m,
#                               seq_bear_deg, SourceFile)

#
## Write to exif
#
# write_exif(dfo)

#
## Image sets by break
#

# ------ Assign space OR time? Run next block if so...
dfo <- dfo %>%
  mutate(
    survey_break = if_else((survey_break_space == 1 | survey_break_time == 1), 1, 0)
  )
# ------

nbreaks <- sum(dfo$survey_break, na.rm = T)
breakIds <- 1:(nbreaks + 1)
breakLocs <- c(which(dfo$survey_break == 1), nrow(dfo) + 1)
for(br in 1:length(breakLocs)) {
  if (br == 1) {
    breakCount = breakLocs[br] - 1
  } else {
    breakCount[br] = breakLocs[br] - breakLocs[br-1]
  }
}

df <- dfo <- dfo %>%
  mutate(
    breakId = rep(breakIds, c(breakCount))
  )

# For view a subset in cases where discrepancies exist
# test <- dfo %>% dplyr::select(capture_date, file_date, frame_count,
#                               latitude, longitude,
#                               merge_diff_sec, seq_diff_sec, seq_dist_m,
#                               seq_bear_deg, SourceFile,
#                               survey_break, survey_break_time, survey_break_space,
#                               breakId)

#
## Image sets by region (will duplicate pts in areas of)
#

# ------- Run only if you want to use SHP designations
shp <- st_read("D:/2024_HarborSeal_Aerial_Survey/GIS/allBoundaries.shp")
spdf <- dfo %>%
  dplyr::select(-Id) %>%
  st_as_sf(coords = c("longitude", "latitude"), remove = F, crs = st_crs(4326))
df <- spdf %>%
  st_join(shp) %>%
  as.data.frame() %>% dplyr::select(-geometry) %>%
  mutate(
    section = paste(island, division, sep = "_")
  )
# -------

#
## Save metadata file for image set
#
write.csv(df, './metaData/June14_VSFB_ES_SouthBay_metadata.csv', row.names = F)
# df <- read.csv("./metaData/June14_VSFB_ES_SouthBay_metadata.csv")

#
## Visual check of image log
#

# Spatial/temporal breaks
dfo %>%
  mutate(
    color = factor(breakId, labels = viridis::viridis(length(unique(breakId)), option = "C")),
    color = gsub("FF", "", color)
  ) %>%
  as_mapbox_source(lng = "longitude", lat = "latitude") %>%
  # Setup a map with the default source above
  mapboxer(
    center = c(md$Longitude[1], md$Latitude[1]),
    zoom = 10
  ) %>%
  # Add a navigation control
  add_navigation_control() %>%
  # Add a layer styling the data of the default source
  add_circle_layer(
    circle_color = c("get", "color"),
    circle_radius = 3,
    popup = "Break ID: {{breakId}} <br> {{SourceFile}}"
  )

# Breaks based on SHP file
df %>%
  mutate(
    color = factor(section, labels = viridis::viridis(length(unique(section)), option = "G")),
    color = gsub("FF", "", color)
  ) %>%
  as_mapbox_source(lng = "longitude", lat = "latitude") %>%
  # Setup a map with the default source above
  mapboxer(
    center = c(md$Longitude[1], md$Latitude[1]),
    zoom = 10
  ) %>%
  # Add a navigation control
  add_navigation_control() %>%
  # Add a layer styling the data of the default source
  add_circle_layer(
    circle_color = c("get", "color"),
    circle_radius = 3,
    popup = "Break ID: {{section}} <br> {{SourceFile}}"
  )




#
## Move files into groups based on batches/bursts
#

## Option 1: By break
idir2 <- gsub("^./", "", idir)
destNames <- paste0("./toMosaic/", idir2, "Break_", breakIds)
lapply(destNames, function (x) dir.create(x, recursive = T))

dfm <- dfo %>%
  mutate(
    DestFile = paste0("./toMosaic/", idir2, "Break_", breakId, "/")
  )

furrr::future_pmap(dfm %>%
                     mutate(recursive = T, copy.date = T) %>%
                     dplyr::select(from = SourceFile, to = DestFile, recursive, copy.date),
                   file.copy,
                   .progress = T)

## Option 2: By section
# idir2 <- gsub("^./", "", idir)
# destNames <- paste0("./toMosaic/", idir2, unique(df$section))
# lapply(destNames, function (x) dir.create(x, recursive = T))
#
# dfm <- df %>%
#   mutate(
#     DestFile = paste0("./toMosaic/", idir2, section, "/")
#   )
#
# furrr::future_pmap(dfm %>%
#                      mutate(recursive = T, copy.date = T) %>%
#                      dplyr::select(from = SourceFile, to = DestFile, recursive, copy.date),
#                    file.copy)




