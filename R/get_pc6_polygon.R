#' Get postcode-6 data for a polygon
#' @param con Connection to CBS database. If missing looks for `data_cbs` connection in config.yml 
#' @param geovlak An sfc polygon (geo column in an sf dataframe)
#' @export
get_pc6_polygon <- function(geovlak, con = NULL,  ...){
  
  if(is.null(con)){
    con <- shinto_db_connection("data_cbs", ...)
    on.exit(DBI::dbDisconnect(con))
  }
  
  polygon <- geovlak %>%
    sf::st_transform(28992)
  
  polygon_txt <- sf::st_as_text(polygon)  
  
  # get data in polygon
  sf::st_read(con, query = glue::glue("select * from cbs_postcode6_2020 as geodata ",
                                " where st_contains(ST_GeomFromText('{polygon_txt}', 28992),",
                                " st_centroid(geodata.geometry))")) %>% 
    sf::st_transform(4326)
  
}
