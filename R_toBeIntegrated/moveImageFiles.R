pkgs <- c("tidyverse")
sapply(pkgs, require, character = T)

dir <- "D:/2024_Er_Aerials/Aug-Sept/"

fi <- list.files(dir, "Timeline", recursive = T)

# Identify and rename Davinci output
df <- data.frame(path = fi, path2 = fi) %>%
  separate(path2, sep = "/", into = c("album", "platform", "video", "orig_image"), extra = "merge") %>%
  mutate(
    new_image = str_replace(orig_image, "Timeline 1_01_", paste0(video, "_")),
    out_file = paste0(dir, album, "/", platform, "/", video, "/Stills/", new_image)
  ) %>%
  separate(new_image, sep = "/", into = c("subfolder", "new_image"), fill = "left") %>%
  mutate(
    subfolder = if_else(is.na(subfolder), "", subfolder)
  )

# Create output directory for stills
dirs <- unique(paste(df$album, df$platform, df$video, "Stills", df$subfolder, sep = "/"))
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
  separate(orig_image, sep = "/", into = c("tsubfolder", "orig_image"), fill = "left") %>%
  mutate(
    date = paste(month, day, year, sep = "/"),
    timeStamp = str_replace(orig_image, "Timeline 1_01_", ""),
    timeStamp = str_replace(timeStamp, ".jpg", ""),
    timeStamp = str_replace(timeStamp, " copy", ""),
    timeStamp = str_replace_all(timeStamp, "_", ":"),
    videoFile = paste0(video, ".MP4")
  ) %>%
  unite(
    "outputImageFileName", c(tsubfolder, new_image), sep = "/", remove = F, na.rm = T
  ) %>%
  select(videoFile, date, port, uasPlatform = platform, timeStamp, outputImageFileName)
write.csv(odf, paste0(dir, "2024_Er_AugSept_NewStill_Filenames.csv"), row.names = F)
