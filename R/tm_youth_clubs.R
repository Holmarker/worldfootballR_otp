#' Scrape Youth Clubs box from Transfermarkt player pages
#'
#' Extracts the "Youth clubs" section from Transfermarkt player profile pages.
#' This box is found inside the `.tm-player-additional-data` containers and is
#' identified specifically by the "Youth clubs" headline.
#'
#' @param player_urls A character vector of Transfermarkt player profile URLs.
#' @param progress Logical; if TRUE (default), shows a progress bar while scraping.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{player_id}{Numeric player ID extracted from the URL}
#'   \item{youth_clubs}{Character string containing youth club history, or `NA` if unavailable}
#' }
#'
#' @examples
#' \dontrun{
#' tm_youth_clubs("https://www.transfermarkt.com/lionel-messi/profil/spieler/28003")
#'
#' tm_youth_clubs(c(
#'   "https://www.transfermarkt.com/lionel-messi/profil/spieler/28003",
#'   "https://www.transfermarkt.com/kylian-mbappe/profil/spieler/342229"
#' ))
#' }
#'
#' @export
tm_youth_clubs <- function(player_urls, progress = TRUE) {
  
  pb <- NULL
  if (progress) {
    pb <- progress::progress_bar$new(total = length(player_urls))
  }
  
  scrape_one <- function(url) {
    if (!is.null(pb)) pb$tick()
    
    player_id <- stringr::str_extract(url, "(?<=/spieler/)[0-9]+")
    
    page <- try(xml2::read_html(url), silent = TRUE)
    if (inherits(page, "try-error"))
      return(tibble::tibble(player_id, youth_clubs = NA_character_))
    
    boxes <- page %>% rvest::html_nodes("div.box.tm-player-additional-data")
    if (length(boxes) == 0)
      return(tibble::tibble(player_id, youth_clubs = NA_character_))
    
    headlines <- boxes %>%
      rvest::html_node(".content-box-headline") %>%
      rvest::html_text(trim = TRUE)
    
    idx <- which(stringr::str_detect(stringr::str_to_lower(headlines), "youth"))
    if (length(idx) == 0)
      return(tibble::tibble(player_id, youth_clubs = NA_character_))
    
    content <- boxes[[idx]] %>%
      rvest::html_node("div.content") %>%
      rvest::html_text(trim = TRUE)
    
    content <- stringr::str_squish(content)
    
    tibble::tibble(player_id, youth_clubs = content)
  }
  
  purrr::map_dfr(player_urls, scrape_one)
}
