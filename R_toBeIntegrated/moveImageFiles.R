pkgs <- c("tidyverse")
sapply(pkgs, require, character = T)

dir <- "D:/2024_Er_Aerials/"

fi <- list.files(dir, "Timeline", recursive = T)

# Identify and rename Davinci output
df <- data.frame(path = fi, path2 = fi) %>%
  separate(path2, sep = "/", into = c("album", "platform", "video", "orig_image")) %>%
  mutate(
    new_image = str_replace(orig_image, "Timeline 1_01_", paste0(video, "_")),
    out_file = paste0(dir, album, "/", platform, "/", video, "/Stills/", new_image)
  )

# Create output directory for stills
dirs <- unique(paste(df$album, df$platform, df$video, "Stills", sep = "/"))
for (i in dirs) {
  dir.create(paste0(dir, i), recursive = T)
}

# Copy files over
future::plan("multisession", workers = 4)
furrr::future_pmap(df %>%
                     mutate(recursive = T, copy.date = T, from = paste0(dir, path)) %>%
                     dplyr::select(from = from, to = out_file, recursive, copy.date),
                   file.copy,
                   .progress = T)

# Output metadata
odf <- df %>%
  separate(album, "_", into = c("year", "month", "day", "port")) %>%
  mutate(
    date = paste(month, day, year, sep = "/"),
    timeStamp = str_replace(orig_image, "Timeline 1_01_", ""),
    timeStamp = str_replace(timeStamp, ".jpg", ""),
    timeStamp = str_replace(timeStamp, " copy", ""),
    timeStamp = str_replace_all(timeStamp, "_", ":"),
    videoFile = paste0(video, ".MP4")
  ) %>%
  select(videoFile, date, port, uasPlatform = platform, timeStamp, outputImageFileName = new_image)
write.csv(odf, paste0(dir, "temp_still_filenames.csv"), row.names = F)
