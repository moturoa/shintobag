
#' Get Gemeente OSM data (amenity, shop)
#' @details Need 'data_osm' config entry in local config file
#' @param gemeente Gemeente
#' @param \dots Further args passed to [shintodb::connect()]
#' @export
get_gemeente_osm <- function(gemeente, jaar="2022", ...){
  
  con <- try(shintodb::connect("data_osm_nederland", ...)  )
  if(inherits(con, "try-error")){
    stop("Add 'data_osm_nederland' (user: datastraat) to config.yml")
  }
  on.exit(DBI::dbDisconnect(con))
  
  grens <- get_geo(gemeente, what = "grens", jaar)
  
  # transform naar WGS84-pseudo mercator; volgens de OSM CRS
  polygon <- grens$geom %>%
    sf::st_transform(3857)
  
  polygon_txt <- sf::st_as_text(polygon)  
  
  # get data in polygon
  sf::st_read(con, query = glue("select * from osmnl.planet_osm_point as geodata ",
                                  " where st_contains(ST_GeomFromText('{polygon_txt}', 3857),",
                                  " geodata.way)")) %>% 
    sf::st_transform(4326)
  
}

