
#' Make a database connection
#' @description Make a connection with RPostgres to one of the Shinto Labs databases.
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

  if(is.null(config_entry)){
    config_entry <- Sys.getenv("R_CONFIG_ACTIVE", "default")  
  }

  if(search_global){
    env <- get_global_config_filename()
    if(env == ""){
      message("SHINTO_GLOBAL_DB_CONFIG environment variable not set; continuing with standard config file")
    } else {
      file <- open_global_config()  
    }
  }
  
  conf <- config::get(value = what,
                      config = config_entry,
                      file = file)

  # If password is encrypted, decrypt it before connecting
  if(string_is_encrypted(conf$dbpassword)){
    conf$dbpassword <- decrypt(conf$dbpassword)
  }

  # Als NULL in een non-default, zoek in default, alleen als allowed
  if(allow_default_fallback & is.null(conf) & config_entry != "default"){
    # conf <- config::get(value = what,
    #                     config = "default",
    #                     file = file)
    warning("'allow_default_fallback' argument is deprecated.")
  }

  if(is.null(conf)){
    stop(paste("Entry", what, "not found in", file," - add password information and try again!"))
  }

  if(!pool){
    DBI::dbConnect(RPostgres::Postgres(),
                   dbname = conf$dbname,
                   host = conf$dbhost,
                   port = conf$dbport,
                   user = conf$dbuser,
                   password = conf$dbpassword)
  } else {
    pool::dbPool(RPostgres::Postgres(),
                 dbname = conf$dbname,
                 host = conf$dbhost,
                 port = conf$dbport,
                 user = conf$dbuser,
                 password = conf$dbpassword)
  }

}




#' @export
#' @rdname shinto_db_connection
global_db_connection <- function(...){
  shinto_db_connection(..., global_search = TRUE)  
}

