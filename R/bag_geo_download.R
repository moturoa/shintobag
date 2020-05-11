
#' Make a database connection
#' @description Make a connection with RPostgres to one of the Shinto Labs databases.
#' @export
shinto_db_connection <- function(what, file = getOption("shintobag_conf", "conf/config.yml")){

  conf <- config::get(what, file = file)

  DBI::dbConnect(RPostgres::Postgres(),
            dbname = conf$dbname,
            host = conf$dbhost,
            port = 5432,
            user = conf$dbuser,
            password = conf$dbpassword)
}



make_sql <- function(dbname, gemeente = NULL){

  if(!is.null(gemeente)){
    sql <- glue::glue("select * from {dbname} where GM_NAAM = ?gem")
    DBI::sqlInterpolate(DBI::ANSI(), sql, gem = gemeente)
  } else {
    glue("select * from {dbname}")
  }

}


#' Download the BAG
#' @description Download the BAG for one Gemeente at a time. Result is an \code{sf} spatial dataframe.
#' @export
get_bag <- function(gemeente, con = NULL, ...){

  if(missing(gemeente)){
    stop("BAG extract werkt per gemeente.")
  }

  if(is.null(con)){
    con <- shinto_db_connection("BAGdata", ...)
    on.exit(dbDisconnect(con))
  }

  sql <- "select * from bagactueel.adres_full where gemeentenaam = ?gem"
  sql <- sqlInterpolate(DBI::ANSI(), sql, gem = gemeente)

  out <- st_read(con, query = sql) %>%
    st_transform(crs = 4326)

  out
}

#' Maak extra adres velden aan in het BAG
#' @description Combineert openbareruimtenaam, huisnummer, huisletter, huisnummertoevoeging
#' in twee nieuwe kolommen: huisnummerhuisletter (12, 13B), en bag_adres (bv. Huisstraat 1A 12, Schoolplein 1).
#' Vervangt ook NA met "" in huisnummer, huisletter, huisnummertoevoeging kolommen.
#' @examples
#' \dontrun{
#'
#' bag <- add_bag_adres_kolommen(bag)
#'
#' }
#' @export
add_bag_adres_kolommen <- function(data){

  dplyr::mutate(data,
                 huisnummer = tidyr::replace_na(huisnummer, ""),
                 huisletter = tidyr::replace_na(huisletter, ""),
                 huisnummertoevoeging = tidyr::replace_na(huisnummertoevoeging, ""),
                 huisnummerhuisletter = paste0(huisnummer, huisletter, " ", huisnummertoevoeging),
                 bag_adres = stringr::str_trim( paste0(openbareruimtenaam, " ",
                                                       huisnummer,
                                                       huisletter,
                                                       huisnummertoevoeging))
            )

}


# Get one geo at a time.
# (don't export, see get_gemeente_geo below.)
get_geo <- function(gemeente = NULL,
                    what = c("grens","buurten","wijken"),
                    con = NULL, ...){

  if(is.null(con)){
    con <- shinto_db_connection("CBS", ...)
    on.exit(dbDisconnect(con))
  }

  what <- match.arg(what)

  tb <- switch(what,
               grens = "gemeente_2018_v2",
               wijken = "wijk_2018_v2",
               buurten = "buurt_2018_v2",
  )

  st_read(con, query = make_sql(tb, gemeente)) %>%
    project_cbs_geo
}


project_cbs_geo <- function(x){
  x %>%
    st_set_crs(28992) %>%
    st_transform(4326)
}


get_gemeente_grens <- function(gemeente, con = NULL, ...){

  get_geo(gemeente, what = "grens", con = con, ...)

}

get_wijken <- function(gemeente, con = NULL, ...){

  get_geo(gemeente, what = "wijken", con = con, ...)

}

get_buurten <- function(gemeente, con = NULL, ...){

  get_geo(gemeente, what = "buurten", con = con, ...)

}



#' Download Buurt, Wijk, Gemeente grenzen.
#' @description Downloads from the CSB database. Entry 'CBS' must be present in config file.
#' @export
get_gemeente_geo <- function(gemeente, ...){

  cbs <- shinto_db_connection("CBS", ...)
  on.exit(dbDisconnect(cbs))

  out <- list(
    grens = get_geo(gemeente, "grens", con = cbs),
    wijken = get_geo(gemeente, "wijken", con = cbs),
    buurten = get_geo(gemeente, "buurten", con = cbs)
  )

  class(out) <- "gemeentegrenzen"

  return(out)

}



#' Row-bind geo objects
#' @description Combine multiple geo objects into a single one. Geo objects are read with
#' \code{get_gemeente_geo}.
#' @examples
#' \dontrun{
#' elke tabel heeft kolom 'gm_naam' voor filtering
#' geos <- lapply(c("Purmerend","Uitgeest"), get_gemeente_geo)
#' geo <- rbind_geo(geos)
#' }
#' @export
rbind_geo <- function(lis){

  nm <- names(lis[[1]])
  out <- vector("list", length = length(lis))

  for(i in seq_along(nm)){

    l <- lapply(lis, "[[", nm[i])

    out[[i]] <- do.call(rbind, l)

  }
  names(out) <- nm

  return(out)
}


#' Download BAG and Geo data for a Gemeente
#' @description Downloads wijk, buurt, gemeente borders, and the BAG for a Gemeente.
#' For BAG, writes an RDS into \code{out_path} (a spatial version, sf-dataframe), and a .feather (
#'  a non-spatial version for very fast reading). For geo, writes e.g. "geo_Eindhoven.rds", a list
#'  with components: wijken, buurten, grens.
#' @param gemeente E.g. "Eindhoven"
#' @param out_path The relative path to write the datasets to.
download_gemeente_opendata <- function(gemeente, out_path = ".", re_download = TRUE){

  fn_geo <- file.path(out_path, paste0("geo_", gemeente, ".rds"))
  fn_bag_1 <- file.path(out_path, paste0("bag_",gemeente,"_sf.rds"))
  fn_bag_2 <- file.path(out_path, paste0("bag_",gemeente,".feather"))

  # gemeente grenzen, wijk, buurt grenzen
  if(!file.exists(fn_geo) | re_download){
    geo <- get_gemeente_geo(gemeente)
    saveRDS(geo, fn_geo)
  }


  # BAG
  if(!file.exists(fn_bag_2) | re_download){
    bag <- get_bag(gemeente)

    bag <- bag %>%
      sf::st_join(dplyr::select(geo$buurten, buurt_naam = bu_naam)) %>%
      sf::st_join(dplyr::select(geo$wijken, wijk_naam = wk_naam))

    # sf-spatial

    saveRDS(bag, fn_bag_1)

    # tibble, feather
    bag <- tibble::as_tibble(bag)
    bag$geopunt <- NULL

    feather::write_feather(bag, fn_bag_2)

  }

}



#' Plot method for a geo object
#' @export
plot.gemeentegrenzen <- function(x, ...){

  if(!require(leaflet))stop("Install leaflet first!")

  leaflet() %>%
    addTiles() %>%
    addPolygons(data = x$grens, fill = FALSE, color = "black", weight = 1) %>%
    addPolygons(data = x$buurten, fill = FALSE, color = "red", weight = 3) %>%
    addPolygons(data = x$wijken, fill = FALSE, color = "blue", weight = 2)
}


