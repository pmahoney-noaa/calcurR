pkgs <- c(
  "tidyverse",
  "stringr"
)
sapply(pkgs, require, character = T)

##
### Function for retrieving DJI video exif data from SRT file
##
srtToDf <- function(
  videoName,
  dir
) {
  video_path <- paste(dir, videoName, sep = "/")

  if (grepl("\\.MP4", video_path)) {
    srt_path <- gsub("\\.MP4", "\\.SRT", video_path)
  } else if (grepl("\\.MOV", video_path)) {
    srt_path <- gsub("\\.MOV", "\\.SRT", video_path)
  } else {
    stop(
      paste0("Video path does not contain a video file: ", video_path)
    )
  }

  srt_content <- readLines(srt_path)

  # Initialize empty vectors to store parsed data
  timestamp <- NA_character_
  frame <- NA_integer_
  diffTime <- NA_character_
  latitude <- NA_real_
  longitude <- NA_real_
  rel_alt = NA_real_
  abs_alt = NA_real_
  focal_len = NA_real_
  dzoom_ratio = NA_real_
  gb_yaw = NA_real_
  gb_pitch = NA_real_
  gb_roll = NA_real_

  for (i in seq_along((srt_content))) {
    line <- srt_content[i]

    # Frame Count
    if (str_detect(line, "FrameCnt: ")) {
      frame <- as.numeric(sub(
        ",.*",
        "",
        str_split(line, "FrameCnt: ")[[1]][[2]]
      ))
    }

    # Timestamp
    if (str_detect(line, "-->")) {
      timestamp <- str_split(line, " --> ")[[1]][[1]]
    }

    # Time interval
    if (str_detect(line, "DiffTime: ")) {
      diffTime <- sub(",.*", "", str_split(line, "DiffTime: ")[[1]][[2]])
    }

    # Latitude
    if (str_detect(line, "latitude: ")) {
      latitude <- as.numeric(sub(
        "].*",
        "",
        str_split(line, "latitude: ")[[1]][[2]]
      ))
    }

    # Longitude
    if (str_detect(line, "longitude: ")) {
      longitude <- as.numeric(sub(
        "].*",
        "",
        str_split(line, "longitude: ")[[1]][[2]]
      ))
    }

    # Altitude
    if (str_detect(line, "rel_alt: ")) {
      rel_alt <- as.numeric(sub(
        " .*",
        "",
        str_split(line, "rel_alt: ")[[1]][[2]]
      ))

      abs_alt <- as.numeric(sub(
        "].*",
        "",
        str_split(line, "abs_alt: ")[[1]][[2]]
      ))
    }

    # Focal length
    if (str_detect(line, "focal_len: ")) {
      focal_len <- as.numeric(sub(
        "].*",
        "",
        str_split(line, "focal_len: ")[[1]][[2]]
      ))
    }

    # Digital zoom
    if (str_detect(line, "dzoom_ratio: ")) {
      dzoom_ratio <- as.numeric(sub(
        "].*",
        "",
        str_split(line, "dzoom_ratio: ")[[1]][[2]]
      ))
    }

    # Gimbal yaw
    if (str_detect(line, "gb_yaw: ")) {
      gb_yaw <- as.numeric(sub(
        " .*",
        "",
        str_split(line, "gb_yaw: ")[[1]][[2]]
      ))
    }

    # Gimbal pitch
    if (str_detect(line, "gb_pitch: ")) {
      gb_pitch <- as.numeric(sub(
        " .*",
        "",
        str_split(line, "gb_pitch: ")[[1]][[2]]
      ))
    }

    # Gimbal roll
    if (str_detect(line, "gb_roll: ")) {
      gb_roll <- as.numeric(sub(
        "].*",
        "",
        str_split(line, "gb_roll: ")[[1]][[2]]
      ))
    }

    # Check for blanks
    if (grepl('^\\s*$', line)) {
      if (frame == 1) {
        out <- data.frame(
          frame = frame,
          timestamp = timestamp,
          difftime = diffTime,
          longitude = longitude,
          latitude = latitude,
          rel_alt = rel_alt,
          abs_alt = abs_alt,
          focal_len = focal_len,
          dzoom_ratio = dzoom_ratio,
          gb_yaw = gb_yaw,
          gb_pitch = gb_pitch,
          gb_roll = gb_roll
        )
      } else {
        out <- rbind(
          out,
          data.frame(
            frame = frame,
            timestamp = timestamp,
            difftime = diffTime,
            longitude = longitude,
            latitude = latitude,
            rel_alt = rel_alt,
            abs_alt = abs_alt,
            focal_len = focal_len,
            dzoom_ratio = dzoom_ratio,
            gb_yaw = gb_yaw,
            gb_pitch = gb_pitch,
            gb_roll = gb_roll
          )
        )
      }

      # Initialize empty vectors to store parsed data
      timestamp <- NA_character_
      frame <- NA_integer_
      diffTime <- NA_character_
      latitude <- NA_real_
      longitude <- NA_real_
      rel_alt = NA_real_
      abs_alt = NA_real_
      focal_len = NA_real_
      dzoom_ratio = NA_real_
      gb_yaw = NA_real_
      gb_pitch = NA_real_
      gb_roll = NA_real_
    }
  }

  return(out)
}

# Example file
# test <- io %>% filter(format == "MP4") %>% slice(1)
# videoName <- test$name
# dir = paste0("D:", test$specificPath, test$relativePath)
# out <- srtToCsv(videoName, dir)
