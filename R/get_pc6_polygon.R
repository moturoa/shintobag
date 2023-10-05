#' Get postcode-6 data for a polygon
#' @param con Connection to CBS database. If missing looks for `data_cbs` connection in config.yml 
#' @param geovlak An sfc polygon (geo column in an sf dataframe)
#' @export
get_pc6_polygon <- function(geovlak, con = NULL,  gemeenten=NULL, ...){
  
  
  if(is.null(con)){
    con <- shintodb::connect("data_cbs", ...)
    on.exit(DBI::dbDisconnect(con))
  }
  
  polygon <- geovlak %>%
    sf::st_transform(28992)
  
  polygon_txt <- sf::st_as_text(polygon)  
  
  
  # Recommended: search gemeente AND on a polygon, since it is much faster
  if(!is.null(gemeenten)){
    
    query <- glue::glue_sql("select * from cbs.cbs_postcode6_2020 as geodata ",
                            " where gm_naam in ({gems*}) and ",
                            "st_contains(ST_GeomFromText({polygon_txt}, 28992),",
                            " st_centroid(geodata.geometry))",
                            .con = con, gems = gemeenten, polygon_txt = polygon_txt)
    
  } else {
    
    query <- glue::glue_sql("select * from cbs.cbs_postcode6_2020 as geodata ",
                            " where st_contains(ST_GeomFromText({polygon_txt}, 28992),",
                            " st_centroid(geodata.geometry))",
                            .con = con, subsetquery = subsetquery, polygon_txt = polygon_txt)
    
  }
  
  sf::st_read(con, query = query) %>% 
    sf::st_transform(4326)
  
}



