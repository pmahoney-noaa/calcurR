#' Title
#'
#' @param shp
#' @param max_length
#' @param split_width
#' @param epsg
#'
#' @return
#' @export
#'
#' @examples

split_survey_polygons <- function(shp, max_length = 1000, split_width = 3000, epsg = "EPSG:32611") {
  #furrr::future_map_dfr(1:nrow(shp), \(i) {
  out <- purrr::map_dfr(1:nrow(shp), \(i) {
    # Geo transform
    shpi <- st_transform(shp[i,], epsg)

    # Best guess central line path along polygon
    cl <- centerline::cnt_path_guess(nngeo::st_remove_holes(shpi), keep = 1)

    if (as.numeric(st_length(cl)) > max_length) {
      # Sample along central line at regulare intervals for dividing polygon for determining angle
      pointsSampleCL <- st_line_sample(cl, n = st_length(cl) / max_length, type = "regular") %>%
        st_cast(to = "POINT")

      # Central line points for determining angle
      pointsCL <- cl %>%
        sf::st_coordinates() %>%
        as.data.frame() %>%
        sf::st_as_sf(coords = c("X", "Y"), crs = epsg)


      # Make lines for clipping polygon
      lineClip <- purrr::map_dfr(1:length(pointsSampleCL), \(x) {
        p1 <- pointsSampleCL[x]
        p2 <- pointsCL %>%
          slice(st_nearest_feature(pointsSampleCL[x], pointsCL))

        # Convert to basic coords
        p1c <- sf::st_coordinates(p1); p2c <- sf::st_coordinates(p2)

        # Estimate angle of line
        alpha <- 90 - 180 * atan((p1c[2] - p2c[2]) / (p1c[1] - p2c[1])) / pi

        # create two new point sets for a perpendicular line
        x1 <- (split_width / 2) * sin((90 + alpha)*(pi/180)); y1 <- (split_width / 2) * cos((90 + alpha)*(pi/180))
        x2 <- (split_width / 2) * sin((-90 + alpha)*(pi/180)); y2 <- (split_width / 2) * cos((-90 + alpha)*(pi/180))
        np1 <- p1 + c(x1, y1); np2 <- p1 + c(x2, y2)
        sf::st_crs(np1) <- sf::st_crs(np2) <- sf::st_crs(p1)

        # Create line
        #op <- sf::st_cast(sf::st_union(np1, np2), "LINESTRING")
        op <- sf::st_as_sf(sf::st_cast(sf::st_union(np1, np2), "LINESTRING"))

        return(op)
      })

      # Split polygon
      parsedPoly <- shpi %>%
        lwgeom::st_split(st_combine(lineClip)) %>%
        st_collection_extract("POLYGON") %>%
        mutate(subsection = 1:n()) %>%
        st_cast("MULTIPOLYGON")

      # ggplot() +
      #   geom_sf(data = shpi, fill = NA) +
      #   geom_sf(data = cl) +
      #   geom_sf(data = pts, size = 2, color = "red") +
      #   geom_sf(data = pointsCL) +
      #   # geom_sf(data = p1, color = "blue", size = 2) +
      #   # geom_sf(data = p2, color = "green", size = 1) +
      #   # geom_sf(data = np1, color = "gray", size = 1) +
      #   # geom_sf(data = np2, color = "purple", size = 1) +
      #   geom_sf(data = lineClip, color = "purple") +
      #   #geom_sf(data = st_combine(lineClip)) +
      #   geom_sf(data = parsedPoly, aes(fill = subset)) +
      #   NULL

    } else {
      parsedPoly <- shpi %>%
        mutate(
          subsection = 1
        )
    }

    return(st_transform(parsedPoly, "EPSG:4326"))
  }, .progress = TRUE)

  return(out)
}
