

#' URL naar Google streetview
#' @param d Een rij uit het BAG (sf-dataframe)
#' @export
google_streetview_url <- function(d,
                                  arrangement = c("11","12"),
                                  rotation_angle = 0,
                                  tilt_angle = 0,
                                  zoom_level = 0,
                                  pitch = 5){

  coor <- as.vector(sf::st_coordinates(d))
  latitude <- coor[2]
  longitude <- coor[1]

  if(zoom_level > 2)stop("zoom_level must be 0,1,2")
  if(abs(pitch) > 90)stop("pitch must be between -90 and 90")

  arrangement <- match.arg(arrangement)

  tryCatch(
    as.character(glue::glue("http://maps.google.com/maps?q=&",
             "layer=c&",
             "cbll={latitude},{longitude}&",
             "cbp={arrangement},{rotation_angle},{tilt_angle},{zoom_level},{pitch}")) %>%
    as.character(),
    error = function(e)"Link niet beschikbaar"
  )
}


#' URL naar de BAG viewer (Kadaster)
#' @param id pandid, adresseerbaarobject, of nummeraanduiding uit het BAG (chr)
#' @export
bagviewer_url <- function(id){
  tryCatch(
    as.character(glue::glue("https://bagviewer.kadaster.nl/lvbag/bag-viewer/index.html#?searchQuery={id}")),
    error = function(e)"Link niet beschikbaar"
  )
}


#' URL naar Google search
#' @param d Een rij uit het BAG (sf-dataframe)
#' @export
google_search_url <- function(d){
  if(!"bag_adres_full" %in% names(d)){
    d$bag_adres_full <- paste(d$openbareruimtenaam, d$huisnummer, d$woonplaatsnaam)
  }
  tryCatch(as.character(glue::glue('https://www.google.com/search?q="{d$bag_adres_full}"')),
           error = function(e)"Link niet beschikbaar")
}


#' Tabel met links naar externe bronnen
#' @param d Een rij uit het BAG (sf-dataframe)
#' @details Maakt een \code{div} (class = "box_externe_bronnen") met daaring links naar
#' Google Streetview, de Kadaster BAG Viewer, en een Google search naar het adres.
#' De gegeven rij uit het BAG moet de sf-dataframe versie zijn, met kolommen 'bag_adres_full' e.a.,
#' toegevoegd met \code{\link{add_bag_adres_kolommen}}
#' (of een recente download met \code{\link{download_gemeente_opendata}).
#' @export
externe_bronnen_lokatie <- function(d){

  withTags(
    div(class = "box_externe_bronnen",
      a(href = utils::URLencode(google_streetview_url(d)),
        HTML("Google Streetview <i class='fa fa-external-link'></i>"),
        target="_blank"),
      br(),
      # a(href=ruimtelijkeplannen_url(d$postcode, paste0(d$huisnummer, d$huisletter)),
      #   HTML("Ruimtelijke Plannen <i class='fa fa-external-link'></i>"),
      #   target="_blank"),
      # br(),
      a(href = utils::URLencode(bagviewer_url(d$pandid)),
        HTML("BAG Viewer (Pand) <i class='fa fa-external-link'></i>"),
        target="_blank"),
      br(),
      a(href = utils::URLencode(bagviewer_url(d$adresseerbaarobject)),
        HTML("BAG Viewer (Adresseerbaar Object) <i class='fa fa-external-link'></i>"),
        target="_blank"),
      br(),
      a(href = utils::URLencode(google_search_url(d)),
        HTML("Google Zoeken <i class='fa fa-external-link'></i>"),
        target="_blank"),
      br()
    )
  )

}







# Werkt niet via URL!
# ruimtelijkeplannen_url <- function(d, zoomlevel = 1000){
#
#   postcode <- tolower(d$postcode)
#   huisnummer <- paste(d$huisnummer, replace_na(d$huisletter, ""))
#
#   url <- glue::glue("https://geodata.nationaalgeoregister.nl/locatieserver/suggest?q={postcode},{huisnummer}")
#
#   r <- jsonlite::fromJSON(url)
#
#   if(r$response$numFound > 0){
#
#     id <- r$response$docs$id[1]
#
#     url <- glue::glue("https://geodata.nationaalgeoregister.nl/locatieserver/lookup?",
#                       "fl=id,weergavenaam,boundingbox_rd,geometrie_rd&id={id}")
#
#     p <- jsonlite::fromJSON(url)
#
#     point <- p$response$docs$geometrie_rd[1]
#     coors <- stringr::str_extract_all(point, "\\d+\\.*\\d*")[[1]]
#
#     url <- glue::glue("https://www.ruimtelijkeplannen.nl/viewer#!/marker/",
#                       "{coors[1]}/{coors[2]}",
#                       "/cs/{coors[1]}/{coors[2]}/{zoomlevel}")
#
#     return(url)
#
#   } else {
#     return("Geen link naar Ruimtelijke Plannen gevonden.")
#   }
#
# }
