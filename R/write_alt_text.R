##' @title Write the alt text accompanying the photo observation
##' @param obs A one-row data frame with the iNaturalist information
##' @importFrom glue glue_data
##' @export
write_photo_alt_text <- function(obs) {
  common <- " "
  if (!is.na(obs[["taxon_common"]])) {
    common <- obs[["taxon_common"]]
    common <- glue::glue(" ({common}) ")
  }

  glue::glue_data(
    obs,
    "Photo of {taxon_name}", common, "by {user_login}. Attribution: {photo_attribution}."
  )
}

##' @title Write the alt text for the map accompanying the observation
##' @param obs A one-row data frame with the iNaturalist information
##' @importFrom glue glue_data
##' @export
write_map_alt_text <- function(obs) {
  coords <- parse_coords(obs[["location_coordinates"]])
  coords <- paste0(
    "(latitude: ", round(coords$lat, 2),
    ", longitude: ", round(coords$lon, 2), ")."
  )
  glue::glue_data(
    obs,
    "Map of the location of the observation: {location_place_guess}, {coords}"
  )
}
