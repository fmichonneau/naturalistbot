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

##' @title Schedule the toots
##' @export
##' @importFrom purrr iwalk
schedule_natbot_toots <- function() {
  ## schedule toots a day in advance so if we want the observations from last
  ## year, need to substract 366 days
  raw_obs <- get_inat_obs_raw(observed_on = as.character(Sys.Date() - 366L))

  ## get the data frame
  obs <- get_inat_obs(raw_obs)

  ## schedule post times
  post_times <- as.POSIXlt(
    paste(Sys.Date() + 1L,
      c("00:00:01", "06:00:01", "12:00:01", "18:00:01")
    )
  )
  post_times <- strftime(post_times, "%Y-%m-%dT%H:%M:%S%z")

  ## draw random observations
  i_obs <- sample(seq_len(nrow(obs)), length(post_times))

  purrr::iwalk(
    i_obs,
    \(i, idx) post_natbot_toot(obs[i, ], scheduled_at = post_times[idx])
  )
}
