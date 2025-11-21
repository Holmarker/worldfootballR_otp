#' Get national team player URLs from Transfermarkt
#'
#' @description Scrapes the national team squad page and returns player URLs.
#'
#' @param team_url A Transfermarkt national team URL (e.g., "https://www.transfermarkt.com/denmark/startseite/team/3435")
#'
#' @return A tibble with columns player_name and player_url.
#'
#' @examples
#' \donttest{
#' tm_national_team_player_urls("https://www.transfermarkt.com/denmark/startseite/team/3435")
#' }
#'
#' @export
tm_national_team_player_urls <- function(team_url) {
  main_url <- "https://www.transfermarkt.com"
  
  page <- tryCatch(
    xml2::read_html(team_url),
    error = function(e) return(character(0))
  )
  
  if (length(page) == 0) {
    return(character(0))
  }
  
  urls <- page %>%
    html_nodes("table.items td.hauptlink a") %>%
    html_attr("href") %>%
    unique()
  
  # Keep only player profile links (drop "market value history" links)
  urls <- urls[grepl("/profil/spieler/", urls)]
  
  paste0(main_url, urls)
}

