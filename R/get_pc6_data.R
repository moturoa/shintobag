
#' Get postcode-6 data for postcode(s)
#' @param con Connection to CBS database. If missing looks for `data_cbs` connection in config.yml 
#' @param pc6 The postcode-6 code(s), "1234AB" (can be a vector)
#' @export
get_pc6_data <- function(pc6, con = NULL, ...){
  
  
  if(is.null(con)){
    con <- shinto_db_connection("data_cbs", ...)
    on.exit(DBI::dbDisconnect(con))
  }
  
  # get data in polygon
  query <- glue::glue_sql("SELECT * FROM cbs_postcode6_2020 WHERE pc6 IN ({vals*})", 
                          vals = pc6, .con = con)
  
  out <- sf::st_read(con, query = query) 
  
  if(nrow(out) > 0){
    out <- sf::st_transform(out, 4326)  
  }
  
out
}

