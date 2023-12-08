##' @title Get the emoji that represents the taxa in the observation
##' @param obs  A one-row data frame with the iNaturalist information
##' @export
get_obs_emoji <- function(obs) {

  iconic_taxon <- obs[["taxon_iconic_name"]]

  if (is.na(iconic_taxon) || !nzchar(iconic_taxon)) {
    return("🦋")
  }

  emojis <- c(
    "Actinopterygii" = "🐟",
    "Animalia" = "🐙",
    "Amphibia" = "🐸",
    "Arachnida" = "🕷️",
    "Aves" = "🐦",
    "Chromista" = "🦠",
    "Fungi" = "🍄",
    "Insecta" = "🐛",
    "Mammalia" = "🐨",
    "Mollusca" = "🐚",
    "Plantae" = "🌿",
    "Protozoa" = "🦠",
    "Reptilia" = "🦎",
    "unknown" = "❓"
  )

  emojis[iconic_taxon]

}

##' @title Get the hashtags for the iNaturalist observation
##' @param obs A one-row data frame with the iNaturalist information
##' @export
##' @importFrom glue glue
get_obs_hashtags <- function(obs) {
  iconic_taxon <- obs[["taxon_iconic_name"]]

  common_tags <- c("#iNaturalist #Nature #WildlifePhotography")

  taxon_tags <- c(
    "Actinopterygii" = "#Fish",
    "Animalia" = "#Animals",
    "Amphibia" = "#Herpetology",
    "Arachnida" = "#Spiders",
    "Aves" = "#Birds",
    "Chromista" = "#Chromista",
    "Fungi" = "#Fungi",
    "Insecta" = "#Entomology #Insects",
    "Mammalia" = "#Mammals",
    "Mollusca" = "#Mollusks",
    "Plantae" = "#Plants #Bloomscrolling",
    "Protozoa" = "#Protozoa",
    "Reptilia" = "#Herpetology #Reptiles",
    "unknown" = ""
  )

  glue::glue(
    common_tags,
    taxon_tags[iconic_taxon],
    .sep = " "
  )

}


##' @title Compose the content of the toot based on the information associated with the observation
##' @param obs A one-row data frame with the iNaturalist information
##' @return A string with the text for the toot
##' @importFrom glue glue_data glue
##' @export
write_toot_text <- function(obs) {
  res <- glue::glue_data(
    obs,
    "
👤 By: {user_login}

📌 Location: {location_place_guess}

🔗 iNaturalist link: {uri}

📷 Photo credits: {photo_attribution}

"
)

  first_line <- glue::glue_data(
    obs,
    " Observed on {observed_on}: {taxon_name}"
  )

  ## add relevant emoji for taxon in front of text
  first_line <- glue::glue(get_obs_emoji(obs), first_line)

  ## add common name if it exists
  if (!is.na(obs[["taxon_common"]])) {
    first_line <- glue::glue_data(
      obs, first_line, " ({taxon_common})")
  }

  ## add first line to rest of the toot
  res <- glue::glue(first_line, "\n", res)

  ## add link to wikipedia page if it exists
  if (!is.na(obs$taxon_wikipedia_url)) {
    res <- glue::glue(
      res,
      "\n\n🔎 Learn more about the taxon: {wikipedia_url}",
      wikipedia_url = obs$taxon_wikipedia_url
    )
  }

  ## add hashtags
  res <- glue::glue(
    res,
    "\n\n",
    get_obs_hashtags(obs)
  )

  res
}
