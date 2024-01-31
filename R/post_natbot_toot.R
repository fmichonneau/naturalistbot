##' @title Write the Toots with the iNaturalist information
##' @param obs A one-row data frame with the iNaturalist information
##' @param scheduled_at Specify date/time for posting the toot
##' @param token the name of the environment variable that holds the Mastodon
##'   token
##' @export
post_natbot_toot <- function(obs, scheduled_at = NULL, token = "RTOOT_TOKEN") {
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
    language = "en",
    token = rtoot:::get_token_from_envvar(token)
  )
}

get_post_times <- function(date = Sys.Date() + 1L) {
  post_times <- as.POSIXlt(
    paste(
      date,
      c("00:00:01", "06:00:01", "12:00:01", "18:00:01")
    )
  )
  post_times
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

  ## write observations
  write_obs_data(obs)

  ## schedule post times
  post_times <- get_post_times()
  post_times <- strftime(post_times, "%Y-%m-%dT%H:%M:%S%z")

  ## draw observations
  n_obs <- min(length(post_times), nrow(obs))
  i_obs <- tryCatch(
    pick_obs(obs, n_obs = n_obs),
    error = function(e) {
      warning(
        "clustering failed with message: ", e$message, call. = FALSE
      )
      message("using random selection instead")
      sample(seq_len(nrow(obs)), n_obs)
    }
  )

  purrr::iwalk(
    i_obs,
    \(i, idx) post_natbot_toot(obs[i, ], scheduled_at = post_times[idx])
  )
}
