#' Scrape Further Information box from Transfermarkt player pages
#'
#' @param player_urls Vector of Transfermarkt player profile URLs
#' @param progress Show progress bar (default = TRUE)
#'
#' @return A tibble with player_id and further_info
#' @export
tm_further_info <- function(player_urls, progress = TRUE) {
  
  pb <- NULL
  if (progress) {
    pb <- progress::progress_bar$new(total = length(player_urls))
  }
  
  scrape_one <- function(url) {
    if (!is.null(pb)) pb$tick()
    
    player_id <- stringr::str_extract(url, "(?<=/spieler/)[0-9]+")
    
    page <- try(xml2::read_html(url), silent = TRUE)
    if (inherits(page, "try-error"))
      return(tibble::tibble(player_id, further_info = NA_character_))
    
    # Get all additional-data boxes
    boxes <- page %>% rvest::html_nodes("div.box.tm-player-additional-data")
    
    if (length(boxes) == 0)
      return(tibble::tibble(player_id, further_info = NA_character_))
    
    # Extract headlines for each box
    headlines <- boxes %>%
      rvest::html_node(".content-box-headline") %>%
      rvest::html_text(trim = TRUE)
    
    # Find the index where headline == "Further information"
    idx <- which(stringr::str_detect(stringr::str_to_lower(headlines), "further information"))
    
    if (length(idx) == 0)
      return(tibble::tibble(player_id, further_info = NA_character_))
    
    # Extract the correct content div
    content <- boxes[[idx]] %>%
      rvest::html_node("div.content") %>%
      rvest::html_text(trim = TRUE)
    
    content <- stringr::str_squish(content)
    
    tibble::tibble(player_id, further_info = content)
  }
  
  purrr::map_dfr(player_urls, scrape_one)
}


