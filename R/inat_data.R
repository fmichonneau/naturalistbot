##' Get raw data from the iNaturalist API
##'
##' This only returns observations that:
##' - have photo(s) licensed cc-by or cc0
##' - can be mapped
##' - that are "research" quality grade
##'
##' @param n_res number of results to return
##' @param observed_on date of when the observation was made (by default 1 year
##'   before today's date)
##' @return a list representing the JSON coming back from the API
##' @importFrom httr2 request req_url_path_append req_url_query req_perform
##'   resp_body_json
##' @export
get_inat_obs_raw <- function(n_res = 199, observed_on = as.character(Sys.Date() - 365)) {
  req <- httr2::request("https://api.inaturalist.org/v1/") |>
    httr2::req_url_path_append("observations") |>
    httr2::req_url_query(
      "observed_on" = observed_on,
      "photos" = TRUE,
      "photo_license" = "cc-by,cc0",
      "mappable" = TRUE,
      "quality_grade" = "research",
      per_page = n_res
    ) |>
    httr2::req_perform()

  resp <- req |>
    httr2::resp_body_json()

  resp
}

##' Transform the list coming back from the API into a tibble
##'
##' The tibble contains the information needed to compose the toot
##'
##' @param obs  A data frame with the iNaturalist information
##' @return a tibble
##' @importFrom purrr map_dfr
##' @importFrom tools file_ext
##' @importFrom dplyr filter bind_cols .data
##' @importFrom tibble tibble
##' @export
get_inat_obs <- function(obs) {
  purrr::map_dfr(obs[[4]], \(x) {
    photos <- purrr::map_dfr(x[["observation_photos"]], \(p) {
      list(
        position = p[["position"]],
        photo_id = p[["photo_id"]],
        photo_license = p[["photo"]][["license_code"]],
        photo_attribution = p[["photo"]][["attribution"]],
        url = p[["photo"]][["url"]],
        extension = tools::file_ext(p[["photo"]][["url"]])
      )
    }) |>
      dplyr::filter(.data[["position"]] == min(.data[["position"]]))

    dplyr::bind_cols(
      tibble::tibble(
        uuid = x[["uuid"]],
        id = x[["id"]],
        observed_on = x[["observed_on_details"]][["date"]],
        taxon_name = x[["taxon"]][["name"]],
        taxon_common = x[["taxon"]][["preferred_common_name"]],
        taxon_rank = x[["taxon"]][["rank"]],
        taxon_observations_count = x[["taxon"]][["observations_count"]],
        taxon_wikipedia_url = x[["taxon"]][["wikipedia_url"]],
        taxon_iconic_name = x[["taxon"]][["iconic_taxon_name"]],
        faves_count = x[["faves_count"]],
        uri = x[["uri"]],
        location_coordinates = x[["location"]],
        location_place_guess = x[["place_guess"]],
        spam = x[["spam"]],
        user_login = x[["user"]][["login"]],
        user_spam = x[["user"]][["spam"]],
        user_suspended = x[["user"]][["suspended"]]
      ),
      photos
    )
  }) |>
  dplyr::filter(!.data[["spam"]], !.data[["user_spam"]], !.data[["user_suspended"]])
}

##' Write iNaturalist Observation data to CSV file
##'
##' @param obs The data frame returned by `get_inat_obs`
##' @param path The path where the CSV file should be written
##' @return the full path of the CSV file
##' @importFrom readr write_csv
write_obs_data <- function(obs, path = ".") {
  filename <- paste0(
    format(Sys.time(), "%Y%m%d-%H%M%S-"),
    "inat-data.csv"
  )
  fullpath <- file.path(path, filename)
  readr::write_csv(obs, file = fullpath)

  if (!file.exists(fullpath)) {
    stop("Error when creating the observation file", call. = FALSE)
  }

  fullpath
}
