## Attempt at using hierarchical clustering to pick observations from different
## users/different parts of the world.
##' @title Pick observations to post
##' @param obs data frame returned by `get_inat_obs`
##' @param n_obs number of indices to return
##' @return a vector of indices corresponding to the number of observations to
##'   return
##' @export
##' @importFrom dplyr select mutate across where everything
##' @importFrom tidyr separate_wider_delim
##' @importFrom tidyclust hier_clust fit
##' @importFrom stats predict
pick_obs <- function(obs, n_obs) {

  clean_obs <- obs |>
    dplyr::select(
      .data$taxon_iconic_name,
      .data$location_coordinates,
      .data$user_login
    ) |>
    tidyr::separate_wider_delim(
      .data$location_coordinates,
      delim = ",",
      names = c("lat", "long")
    ) |>
    dplyr::mutate(across(c("lat", "long"), as.numeric)) |>
    dplyr::mutate(across(where(is.character), as.factor))

  std_obs <- clean_obs |>
    dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.numeric)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), scale))

  hc_spec <- tidyclust::hier_clust(
    num_clusters = n_obs,
    linkage_method = "average"
  )

  hc_fit <- hc_spec |>
    tidyclust::fit(~ ., data = std_obs)

  hc_preds <- hc_fit |>
    stats::predict(std_obs)

  hc_preds |>
    dplyr::mutate(i = dplyr::row_number()) |>
    dplyr::group_by(.data$.pred_cluster) |>
    dplyr::sample_n(1L) |>
    dplyr::pull(.data$i)
}
