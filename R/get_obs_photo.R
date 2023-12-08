##' @title Get the photo associated associated with the observation
##' @param obs A one-row data frame with the iNaturalist information
##' @return The path of the image downloaded from iNaturalist
##' @importFrom glue glue
##' @importFrom httr2 request req_perform
##' @export
get_obs_photo <- function(obs) {
  photo_id <- obs[["photo_id"]]
  extension <- obs[["extension"]]

  outfile <- tempfile(fileext = paste0(".", extension))
  url <- glue::glue(
    "https://inaturalist-open-data.s3.amazonaws.com/photos/{photo_id}/large.{extension}",
    photo_id = photo_id,
    extension = extension
  )
  req <- httr2::request(url) |>
    httr2::req_perform(path = outfile)
  outfile
}
