#' Title
#'
#' @param images
#'
#' @return
#' @export
#'
#' @examples

pull_exif <- function(images) {
  furrr::future_map_dfr(images, exifr::read_exif) %>%
    extract(FileName, "frame_count") %>%
    mutate(
      frame_count = as.numeric(gsub("DSC", "", frame_count)),
      file_date = ymd_hms(paste0(CreateDate, OffsetTime))
    ) %>%
    dplyr::select(
      frame_count, ShutterCount, SourceFile, FileModifyDate, FileType, FileTypeExtension,
      Make, Model, FocalLength, Aperture, LensInfo, LensModel, Megapixels, file_date
    )
}
