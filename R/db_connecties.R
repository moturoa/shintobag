
#' Make a database connection
#' @description `r lifecycle::badge('deprecated')` 
#' This function is deprecated; please use [shintodb::connect()]
#' It is provided here only for backwards compatibility.
#' Make a connection with RPostgres to one of the Shinto Labs databases.
#' @details `global_db_connection` is a shorthand for `shinto_db_connection` with `search_global = TRUE`. 
#' Use this option to store encrypted passwords in a
#' @param what Welke database connectie in het config bestand? (bv. "BAGdata")
#' @param file Lokatie van het config bestand, normaal gezet via `options(shintobag_conf = /path/to/file)`
#' @param search_global If TRUE, ignores 'file' argument and tries to find global config file.
#' @param port_default The default port if missing from config ("dbport")
#' @param pool Logical. Als TRUE, gebruikt `pool::dbPool`, anders `DBI::dbConnect`
#' @export 
#' @rdname shinto_db_connection
shinto_db_connection <- function(what,
                                 file = getOption("shintobag_conf", "conf/config.yml"),
                                 search_global = FALSE,
                                 allow_default_fallback = FALSE,
                                 pool = FALSE,
                                 config_entry = NULL){

  stop("This function is deprecated, switch to `shintodb::connect` instead.")
  
}




