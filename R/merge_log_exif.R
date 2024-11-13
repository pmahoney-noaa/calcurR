#' Title
#'
#' @param md
#' @param exif
#'
#' @return
#' @export
#'
#' @examples

merge_log_exif <- function(md, exif) {

  if (all(c("Latitude", "Longitude") %in% names(md))) {
    mdo <- md %>%
      mutate(
        latitude = Latitude,
        longitude = Longitude
      )
  } else {
    mdo <- md %>%
      mutate(
        lat_dir = if_else(lat_ns == "N", 1, -1),
        lon_dir = if_else(lon_ew == "E", 1, -1),
        latitude = lat_dir * (as.numeric(substr(lat, 1, 2)) + as.numeric(substr(lat, 3, nchar(lat))) / 60),
        longitude = lon_dir * (as.numeric(substr(lon, 1, 3)) + as.numeric(substr(lon, 4, nchar(lon))) / 60),
      )
  }

  mdo <- mdo %>%
    dplyr::inner_join(exif, by = "frame_count") %>%
    mutate(
      # Ideally, this would be the variable utc_time, but problematic for some files
      capture_date = mdy_hms(paste(date.y, UTC.time)), #ymd_hms(utc_time),

      # Difference (in secs) between survey capture_date (and time) and file_date (and time)
      merge_diff_sec = as.numeric(abs(capture_date - file_date)),

      lat1 = c(NA, latitude[-length(latitude)]), long1 = c(NA, longitude[-length(longitude)]),
      lat2 = latitude, long2 = longitude,
      seq_diff_sec = c(NA, as.numeric(diff(capture_date))),
      seq_dist_m = geosphere::distHaversine(cbind(long1, lat1), cbind(long2, lat2))
    )

  seq_bear_deg <- mdo %>%
    slice(-1) %>%
    mutate(
      seq_bear_deg = geosphere::bearingRhumb(cbind(long1, lat1), cbind(long2, lat2))
    ) %>%
    pull(seq_bear_deg)

  mdo <- mdo %>%
    mutate(seq_bear_deg = c(NA, seq_bear_deg)) %>%
    dplyr::select(-lat1, -lat2, -long1, -long2)

  return(mdo)
}
