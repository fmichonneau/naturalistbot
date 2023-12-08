##' @title Write the alt text accompanying the photo observation
##' @param obs A one-row data frame with the iNaturalist information
##' @importFrom glue glue_data
##' @export
write_photo_alt_text <- function(obs) {
  glue::glue_data(
    obs,
    "Photo of {taxon_name} ({taxon_common}) by {user_login}. Attribution: {photo_attribution}"
  )
}

##' @title Write the alt text for the map accompanying the observation
##' @param obs A one-row data frame with the iNaturalist information
##' @importFrom glue glue_data
##' @export
write_map_alt_text <- function(obs) {
  glue::glue_data(
    obs,
    "Map of the location of the observation: {location_place_guess}"
  )
}
