
#' Get Gemeente OSM data (amenity, shop)
#' @details Need 'data_osm' config entry in local config file
#' @param gemeente Gemeente
#' @param \dots Further args passed to [shintobag::shinto_db_connection()]
#' @export
get_gemeente_osm <- function(gemeente, ...){
  
  con <- shintobag::shinto_db_connection("data_osm", ...)  
  on.exit(DBI::dbDisconnect(con))
  
  sql <- as.character(glue::glue("select * from osm.amenity where gm_naam = ?gem"))
  sql <- DBI::sqlInterpolate(DBI::ANSI(), sql, gem = gemeente)
  amenity <- dbGetQuery(con, sql) %>%
    st_as_sf(wkt = "geopunt") %>%
    st_set_crs(28992)
  
  sql <- as.character(glue::glue("select * from osm.shop where gm_naam = ?gem"))
  sql <- DBI::sqlInterpolate(DBI::ANSI(), sql, gem = gemeente)
  shop <- dbGetQuery(con, sql) %>%
    st_as_sf(wkt = "geopunt") %>%
    st_set_crs(28992)
  
  list(amenity = amenity, shop = shop)
  
}

  