
#' Make a database connection
#' @description Make a connection with RPostgres to one of the Shinto Labs databases.
#' @param what Welke database connectie in het config bestand? (bv. "BAGdata")
#' @param file Lokatie van het config bestand, normaal gezet via `options(shintobag_conf = /path/to/file)`
#' @param pool Logical. Als TRUE, gebruikt `pool::dbPool`, anders `DBI::dbConnect`
#' @export
shinto_db_connection <- function(what,
                                 file = getOption("shintobag_conf", "conf/config.yml"),
                                 port = 5432,
                                 pool = FALSE){

  conf <- config::get(what, file = file)

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

