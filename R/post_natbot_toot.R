##' @title Write the Toots with the iNaturalist information
##' @param obs A one-row data frame with the iNaturalist information
##' @param scheduled_at Specify date/time for posting the toot
##' @export
post_natbot_toot <- function(obs, scheduled_at = NULL) {
  stopifnot(
    inherits(obs, "tbl_df"),
    identical(nrow(obs), 1L)
  )
  rtoot::post_toot(
    status = write_toot_text(obs),
    media = c(
      get_obs_photo(obs),
      draw_obs_map(obs)
    ),
    alt_text = c(
      write_photo_alt_text(obs),
      write_map_alt_text(obs)
    ),
    scheduled_at = scheduled_at,
    visibility = "public",
    language = "en"
  )
}
