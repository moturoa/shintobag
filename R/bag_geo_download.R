


#' Download the BAG
#' @description Download the BAG for one Gemeente at a time. Result is an `sf` spatial dataframe.
#' @param gemeente Bv. "Rozendaal" (niet vectorized)
#' @param con DB connectie naar data_bag
#' @param table "adres_full" (default),"adres_plus" of "adres"
#' @export
get_bag <- function(gemeente, con = NULL, table = c("adres_full","adres_plus","adres"), ...){

  table <- match.arg(table)
  
  if(missing(gemeente)){
    stop("BAG extract werkt per gemeente.")
  }

  if(is.null(con)){
    con <- shintodb::connect("data_bag", ...)
    on.exit(DBI::dbDisconnect(con))
  }

  sql <- as.character(glue::glue("select * from bagactueel.{table} where gemeentenaam = ?gem"))
  sql <- DBI::sqlInterpolate(DBI::ANSI(), sql, gem = gemeente)

  out <- sf::st_read(con, query = sql) 
  
  if(nrow(out) == 0){
    warning(glue::glue("gemeente '{gemeente}' niet gevonden in {table}!"))
    return(NULL)
  }
  
  out <- sf::st_transform(out, crs = 4326)

  out
}

#' Maak extra adres velden aan in het BAG
#' @description Combineert openbareruimtenaam, huisnummer, huisletter, huisnummertoevoeging
#' in twee nieuwe kolommen: huisnummerhuisletter (12, 13B), en bag_adres (bv. Huisstraat 1A 12, Schoolplein 1).
#' Vervangt ook NA met "" in huisnummer, huisletter, huisnummertoevoeging kolommen.
#' @param data De BAG dataset (spatial (rds) of niet (feather)).
#' @examples
#' \dontrun{
#'
#' bag <- add_bag_adres_kolommen(bag)
#'
#' }
#' @export
add_bag_adres_kolommen <- function(data){

  dplyr::mutate(data,
                 huisnummer = tidyr::replace_na(as.character(huisnummer), ""),
                 huisletter = tidyr::replace_na(huisletter, ""),
                 huisnummertoevoeging = tidyr::replace_na(huisnummertoevoeging, ""),
                 huisnummerhuisletter = paste0(huisnummer, huisletter, " ", huisnummertoevoeging),
                 bag_adres = stringr::str_trim( paste0(openbareruimtenaam, " ",
                                                       huisnummer,
                                                       huisletter,
                                                       huisnummertoevoeging)),
                bag_adres_full = paste(bag_adres, woonplaatsnaam)
            )

}





#' Projecteer data naar lat/long (WG84)
#' @description Projecteert naar EPS 4326, voor gebruik in leaflet. Werkt ook voor lege spatial columns,
#' waar `st_transform` moeite mee heeft. Voor polygonen, haalt ook de z-as weg (anders werkt leaflet niet).
#' @export
proj_4326 <- function(data){

  if(length(data) == 0 || all(sf::st_is_empty(data))){
    return(data)
  }

  if(st_geometry_type(data)[1] == "POLYGON"){

    if(is.null(dim(data))){
      data <- sf::st_zm(data)
    } else {
      col <- names(data)[sapply(data, function(x)inherits(x, "sfc_POLYGON"))][1]

      data[[col]] <- sf::st_zm(data[[col]])
    }

  }

  sf::st_transform(data, 4326)
}


# Util - geen export.
# helper functie voor CBS tabellen, die hebben geen CRS info,
# stellen we in, dan converteren
project_cbs_geo <- function(x){
  
  suppressWarnings({
    x %>%
      sf::st_set_crs(28992) %>%
      sf::st_transform(4326)  
  })
  
}




#' Zoek data binnen een polygon
#' @details Voor geavanceerd gebruik.
#' @param polygon Sf-kolom, projectie 28992 
#' @param con Database connectie
#' @param table Naam van de table, inclusief schema ("latest.adres")
#' @param geocolumn Naam van de kolom met geo-info in de tabel
#' @param min_overlap Voor polygonen, alleen diegene die minstens met min_overlap overlappen met de zoek polygoon
#' @param st_function Een van 3 spatial predicates
#' @export
get_data_polygon <- function(polygon,
                             con,
                             table,
                             geocolumn,
                             min_overlap = 0.9,
                             st_function = c("st_intersects","st_overlaps","st_contains")){

  st_function <- match.arg(st_function)

  polygon_txt <- sf::st_as_text(polygon)  
  
  out <- sf::st_read(con,
                 query = glue::glue("select * from {table} as geodata",
                              " where {st_function}(ST_GeomFromText('{polygon_txt}', 28992),",
                              " geodata.{geocolumn})"))

  # Zet projectie (wordt in principe meegeleverd maar niet altijd)
  out <- sf::st_set_crs(out, 28992)

  # Verwijder zeer grote polygonen (in sommige tabellen wordt de bounding box meegegeven)
  are <- as.numeric(sf::st_area(out[[geocolumn]]))
  out <- out[are < 10^9,]

  # Als we polygonen hebben gezocht, verwerk de data iets verder.
  if(isTRUE(unique(sf::st_geometry_type(out[[geocolumn]]))[1] != "POINT")){

    # Vind (weer!) de intersecting polygons.
    # Soms vind PostGIS intersecting polygons waarvan sf vindt dat ze niet intersecten.
    # Dan werkt de minimum overlap methode niet. Hier fixen.
    which_intersect <- as.numeric(sf::st_intersects(polygon, out)[[1]])
    out <- out[which_intersect,]

    # Verwijder minimaal overlappende polygonen
    overl_fraction <- as.numeric(sf::st_area(sf::st_intersection(polygon, out)) / sf::st_area(out))
    out <- out[overl_fraction > min_overlap, ]

  }

  out

}

#' Download het perceel voor een geo-punt
#' @description Vind het perceel uit het Kadaster waarbinnen het geo-punt valt.
#' Resultaat is in 28992 projectie.
#' @param pnt sfc, bv. bag$geopunt
#' @param con Optioneel, anders "kadaster" (get_perceel_pand) of "BAGdata" (get_panden_perceel)
#' in het config bestand.
#' @export
#' @rdname get_perceel
get_perceel_geopunt <- function(pnt, con = NULL, ...){

  if(is.null(con)){
    con <- shintodb::connect("data_brk", ...)
    on.exit(DBI::dbDisconnect(con))
  }

  # Transformeer punt, en WKT representatie om naar PostGIS te sturen.
  pnt <- sf::st_transform(pnt, 28992)
  pnt_txt <- sf::st_as_text(pnt)

  st_read(con,
          query = glue::glue("SELECT * FROM latest.perceel AS perceel ",
                       "WHERE ST_Contains(perceel.begrenzing, ST_GeomFromText('{pnt_txt}', 28992))"))

}


#' Vind alle panden op een perceel
#' @description Download alle panden uit het BAG op een perceel.
#' @param perceel Geovlak van het perceel (uit het Kadaster, typisch gelezen met get_perceel_geopunt)
#' @param min_overlap Zorgt ervoor dat panden die slechts het perceel raken (of door meetfouten net
#' in het perceel liggen) niet worden meegenomen.
#' @export
#' @rdname get_perceel
get_panden_perceel <- function(perceel, min_overlap = 0.9, con = NULL, ...){

  if(is.null(con)){
    con <- shintodb::connect("data_bag", ...)
    on.exit(DBI::dbDisconnect(con))
  }

  out <- get_data_polygon(perceel, con, "bagactueel.pand", "geovlak",
                          min_overlap = min_overlap)

  # Als einddatumtijdvakgeldigheid niet NA is, dan is deze rij niet meer geldig.
  dplyr::filter(out, is.na(einddatumtijdvakgeldigheid)) %>%
    mutate(pandoppervlakte = round(as.numeric(st_area(geovlak)),1))

}


#' Download de woonkernen (bebouwde kommen) voor een gemeente
#' @description Download woonkernen uit de Top10nl database.
#' @param grens Een polygoon (projectie 4326 of 28992, wordt automatisch geconverteerd),
#' waarbinnen de woonkernen gezocht worden.
#' @examples
#' grens <- get_geo("Nederweert", "grens")
#' @export
get_woonkernen <- function(grens, ...){

  con <- shintodb::connect("data_top10nl", ...)
  on.exit(DBI::dbDisconnect(con))

  grens <- sf::st_transform(grens, 28992)

  out <- get_data_polygon(polygon = grens,
                          con = con,
                          table = "latest.plaats",
                          geocolumn = "geometrie_multivlak",
                          st_function = "st_intersects")

  dplyr::filter(out, bebouwdekom == "ja")

}

#' Row-bind geo objects
#' @description Combine multiple geo objects into a single one. Geo objects are read with
#' `get_gemeente_geo`.
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

    out[[i]] <- dplyr::bind_rows(l)

  }
  names(out) <- nm

  return(out)
}


#' Download BAG and Geo data for a Gemeente
#' @description `r lifecycle::badge('deprecated')`. We now use `get_bag_from_cache` and `get_geo_from_cache`. 
#' Downloads wijk, buurt, gemeente borders, and the BAG for a Gemeente.
#' For BAG, writes an RDS into `out_path` (a spatial version, sf-dataframe). 
#'  For geo, writes e.g. "geo_Eindhoven.rds", a list with components: wijken, buurten, grens.
#' @param gemeente E.g. "Eindhoven"
#' @param out_path The relative path to write the datasets to.
#' @param rds `r lifecycle::badge('deprecated')` Always writes RDS
#' @param feather `r lifecycle::badge('deprecated')` Unused - feather dependency removed
#' @export
download_gemeente_opendata <- function(gemeente,
                                       out_path = ".",
                                       re_download = TRUE,
                                       cbs_jaar,
                                       kws = FALSE,
                                       kws_jaar = 2021,
                                       rds = TRUE,
                                       feather = FALSE){

  fn_geo <- file.path(out_path, paste0("geo_", gemeente, ".rds"))
  fn_bag_1 <- file.path(out_path, paste0("bag_",gemeente,"_sf.rds"))

  # gemeente grenzen, wijk, buurt grenzen
  if(!file.exists(fn_geo) | re_download){
    geo <- get_gemeente_geo(gemeente, jaar = cbs_jaar, kws = kws, kws_jaar = kws_jaar)
    saveRDS(geo, fn_geo)
  } else {
    geo <- readRDS(fn_geo)
  }


  # BAG
  if(!file.exists(fn_bag_1) | re_download){
    bag <- get_bag(gemeente)

    # buurt_naam, wijk_naam toevoegen op basis van locatie
    suppressMessages({
      bag <- bag %>%
        sf::st_join(dplyr::select(geo$buurten, buurt_naam = bu_naam, buurt_code = bu_code)) %>%
        sf::st_join(dplyr::select(geo$wijken, wijk_naam = wk_naam, wijk_code = wk_code)) %>%
        shintobag::add_bag_adres_kolommen()
    })

    # sf-spatial
    saveRDS(bag, fn_bag_1)  
    
  }

}






