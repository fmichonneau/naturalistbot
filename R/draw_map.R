##' @title Convert GPS coordinates from iNat into latitude and longitude
##' @param location the GPS coordinate string from iNat (in the form "lat,lon")
##' @importFrom rlang is_scalar_character
##' @export
parse_coords <- function(location) {
  stopifnot(rlang::is_scalar_character(location))
  lat_lon <- strsplit(location, ",")[[1]]
  list(
    lat = as.double(lat_lon[1]),
    lon = as.double(lat_lon[2])
  )
}

##' @title Calculate the bounding box for the observation
##' @param location the output from `parse_coords`
##' @param f how much space to include around the GPS coordinate
##' @return a vector of length 4 that includes the coordinates of the corners of
##'   the bounding box
##' @export
calc_bbox <- function(location, f = .25) {
  lons <- c(location$lon - f, location$lon + f)
  lats <- c(location$lat - f, location$lat + f)
  c(
    left = min(lons),
    bottom = min(lats),
    right = max(lons),
    top = max(lats)
  )
}

##' @title Draw the observation map
##' @param obs A one-row data frame with the iNaturalist information
##' @return The path of the file that includes the map as a png file
##' @importFrom ggmap ggmap
##' @importFrom ggplot2 ggsave
##' @importFrom scales alpha
##' @export
draw_obs_map <- function(obs) {
  location <- obs[["location_coordinates"]]
  outfile <- tempfile(fileext = ".png")
  location <- parse_coords(location)
  bbox <- calc_bbox(location)
  map <- ggmap::get_stadiamap(
    bbox = bbox,
    maptype = "stamen_terrain",
    zoom = 11
  )
  ggmap::ggmap(map, extent = "device") +
    ggplot2::geom_point(
      data = data.frame(lon = location$lon, lat = location$lat),
      shape = 21,
      color = scales::alpha("red", .6),
      fill = NA,
      size = 4,
      stroke = 2
    )

  ggplot2::ggsave(filename = outfile, width = 670, height = 1000, units = "px")
  outfile
}
