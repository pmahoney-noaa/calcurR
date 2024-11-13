#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples

write_exif <- function(df) {
  furrr::future_pmap(df %>% dplyr::select(longitude, latitude, SourceFile), exifMod)
}

exifMod <- function(longitude, latitude, SourceFile) {
  lon_ew = ifelse(longitude < 0, "W", "E")
  lat_ns = ifelse(latitude < 0, "S", "N")

  exifr::exiftool_call(
    paste(paste0("-GPSLongitude=", double_quote(longitude)),
          paste0("-GPSLongitudeRef=", double_quote(lon_ew)),
          paste0("-GPSLatitude=", double_quote(latitude)),
          paste0("-GPSLatitudeRef=", double_quote(lat_ns))),
    SourceFile
  )
}
