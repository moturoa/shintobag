
#' Make a database connection
#' @description Make a connection with RPostgres to one of the Shinto Labs databases.
#' @param what Welke database connectie in het config bestand? (bv. "BAGdata")
#' @param file Lokatie van het config bestand, normaal gezet via `options(shintobag_conf = /path/to/file)`
#' @param port_default The default port if missing from config ("dbport")
#' @param pool Logical. Als TRUE, gebruikt `pool::dbPool`, anders `DBI::dbConnect`
#' @export
shinto_db_connection <- function(what,
                                 file = getOption("shintobag_conf", "conf/config.yml"),
                                 port_default = 5432,
                                 allow_default_fallback = FALSE,
                                 pool = FALSE){


  config_entry <- Sys.getenv("R_CONFIG_ACTIVE", "default")

  conf <- config::get(value = what,
                      config = config_entry,
                      file = file)


  # Als NULL in een non-default, zoek in default, alleen als allowed
  if(allow_default_fallback & is.null(conf) & config_entry != "default"){
    conf <- config::get(value = what,
                        config = "default",
                        file = file)
  }

  if(is.null(conf)){
    stop(paste("Entry", what, "not found in", file," - add password information and try again!"))
  }

  port <- ifelse(is.null(conf$dbport), default_port, conf$dbport)

  if(!pool){
    DBI::dbConnect(RPostgres::Postgres(),
                   dbname = conf$dbname,
                   host = conf$dbhost,
                   port = port,
                   user = conf$dbuser,
                   password = conf$dbpassword)
  } else {
    pool::dbPool(RPostgres::Postgres(),
                 dbname = conf$dbname,
                 host = conf$dbhost,
                 port = port,
                 user = conf$dbuser,
                 password = conf$dbpassword)
  }

}

