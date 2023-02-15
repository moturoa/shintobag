

#' @rdname get_gemeente_geo
#' @param jaar Buurt wijk kaart "2018" of "2021"
#' @param kws Als jaar = "2021", kerncijfers tevoegen?
#' @export
get_geo <- function(gemeente = NULL,
                    what = c("grens","buurten","wijken"),
                    jaar = c("2018", "2021", "2022"),
                    kws = FALSE,
                    kws_jaar = 2021,
                    con = NULL, 
                    spatial = TRUE,
                    extra_sql = NULL,
                    include_water = FALSE,
                    ...){
  
  if(is.null(con)){
    con <- shintodb::connect("data_cbs", ...)
    on.exit(DBI::dbDisconnect(con))
  }
  
  what <- match.arg(what)
  jaar <- match.arg(jaar)
  
  if(jaar == "2018"){
    tb <- switch(what,
                 grens = "gemeente_2018_v2",
                 wijken = "wijk_2018_v2",
                 buurten = "buurt_2018_v2"
    )
  } else if(jaar == "2021"){
    tb <- switch(what,
                 grens = "gemeente_2021",
                 wijken = "wijk_2021",
                 buurten = "buurt_2021"
    )
  } else if(jaar == "2022"){
    tb <- switch(what,
                 grens = "gemeente_2022",
                 wijken = "wijk_2022",
                 buurten = "buurt_2022"
    )
  }
  
  query <- make_sql(tb, gemeente)
  query <- paste(query, extra_sql)
  
  if(spatial){
    out <- sf::st_read(con, query = query)   
  } else {
    out <- DBI::dbGetQuery(con, query)
    out$geometry <- NULL
  }
  
  if(!include_water && "water" %in% names(out)){
    out <- dplyr::filter(out, water == "NEE")
  }
  
  
  if(nrow(out) == 0){
    stop(paste0("Gemeente '",gemeente, "' niet gevonden in data_cbs"))
  }  
  
  # Fix names. First column must be the region code (see add_kws)
  if(what == "grens")out <- dplyr::relocate(out, "gm_code")
  if(what == "wijken")out <- dplyr::relocate(out, "wk_code")
  if(what == "buurten")out <- dplyr::relocate(out, "bu_code")
  
  if(spatial){
    out <- project_cbs_geo(out)  
  }
  
  if(kws){
    assert_kws_peiljaar(kws_jaar)
    out <- add_kws(out, kws_jaar, con)
  }
  
  out
}



assert_kws_peiljaar <- function(peiljaar){
  if(!all(peiljaar %in% 2013:2022)){
    stop("Alleen data geupload tussen 2013 en 2022")
  }
}

#' @rdname get_gemeente_kws
#' @export
#' @importFrom dplyr tbl
get_kws <- function(gemeente,
                    what = c("grens","buurten","wijken"),
                    peiljaar = 2021,
                    con = NULL, ...){
  
  what <- match.arg(what)
  assert_kws_peiljaar(peiljaar)
  
  if(is.null(con)){
    con <- shintodb::connect("data_cbs", ...)
    on.exit(DBI::dbDisconnect(con))
  }
  
  s_txt <- switch(what,
                  grens = "Gemeente",
                  buurten = "Buurt",
                  wijken = "Wijk"
  )
  
  gwb_txt <- switch(what,
                    grens = "gm_code",
                    buurten = "bu_code",
                    wijken = "wk_code"
  )
  
  dplyr::tbl(con, "cbs_kerncijfers_2013_2021") %>%
    dplyr::filter(gm_naam %in% !!gemeente,
                  peiljaar %in% !!peiljaar,
                  regio_type == !!s_txt) %>%
    dplyr::collect(.) %>%
    dplyr::rename(!!rlang::sym(gwb_txt):=gwb_code)
  
}

#' @rdname get_gemeente_kws
#' @export
add_kws <- function(data, peiljaar, con = NULL){
  
  regio <- unique(data$regio_type)
  if(length(regio) > 1)stop("Geen regio types mixen!")
  assert_kws_peiljaar(peiljaar)
  
  s_regio <- NULL
  key_col <- names(data)[1]
  if(key_col == "bu_code")s_regio<-"buurten"
  if(key_col == "wk_code")s_regio<-"wijken"
  if(key_col == "gm_code")s_regio <- "grens"
  if(is.null(s_regio))stop("eerste kolom moet bu_code, wk_code of gm_code zijn")
  
  gem <- unique(data$gm_naam)
  
  data_kws <- get_kws(gem, s_regio, peiljaar, con = con)
  
  if(nrow(data_kws) == 0){
    warning(paste0("Geen kerncijfers voor Gemeente '",gem,"' in ",peiljaar," gevonden."))
    return(data)
  }
  
  double_names <- setdiff(intersect(names(data), names(data_kws)), key_col)
  dplyr::left_join(select(data, - all_of(double_names)), data_kws, by = key_col)
  
}


#' @rdname get_gemeente_kws
#' @export
get_kws_metadata <- function(con = NULL, ...){
  
  if(is.null(con)){
    con <- shintodb::connect("data_cbs", ...)
    on.exit(DBI::dbDisconnect(con))
  }
  
  dplyr::tbl(con, "cbs_kerncijfers_2013_2021_metadata") %>% collect
  
}

#' @rdname get_gemeente_kws
#' @export
make_kws_select_choices <- function(choices = NULL){
  
  data <- get_kws_metadata()
  
  if(!is.null(choices)){
    data <- dplyr::filter(data, kolom %in% !!choices)
  }
  
  o <- split(data$kolom %>% setNames(data$toelichting),
             data$categorie)
  
  o <- o[names(o) != ""]
  
  o
}



#' Download Buurt, Wijk, Gemeente grenzen.
#' @description Download gemeente, wijk, en buurt grenzen uit de CBS Wijk/Buurt kaart.
#' @details De config moet 'data_cbs' connectie details bevatten (naar de CBS database).
#' @param gemeente Gemeentenaam
#' @param jaar CBS jaar ("2018", "2021", "2022")
#' @param get_latest_data If TRUE, get most recent data in database
#' @param what Voor `get_geo`, "buurten", "wijken", of "grens"
#' @param con Connectie naar de CBS database (als leeg, wordt automatisch aangemaakt)
#' @param kws Kerncijfers toevoegen? (default: FALSE)
#' @param kws_jaar Kerncijfers voor welk jaar toevoegen?
#' @rdname get_gemeente_geo
#' @export
get_gemeente_geo <- function(gemeente, 
                             jaar = NULL,
                             get_latest_data = TRUE,
                             kws = FALSE,
                             kws_jaar = 2021,
                             ...){
  
  cbs <- shintodb::connect("data_cbs", ...)
  on.exit(DBI::dbDisconnect(cbs))
  
  # TODO has to be configured somewhere; we need bag/geo/cbs metadata tables
  if(is.null(jaar) && get_latest_data){
    jaar <- "2022"
  }
  
  out <- list(
    grens = get_geo(gemeente, "grens", con = cbs, jaar = jaar, kws = kws, kws_jaar = kws_jaar),
    wijken = get_geo(gemeente, "wijken", con = cbs, jaar = jaar, kws = kws, kws_jaar = kws_jaar),
    buurten = get_geo(gemeente, "buurten", con = cbs, jaar = jaar, kws = kws, kws_jaar = kws_jaar)
  )
  
  class(out) <- "gemeentegrenzen"
  
  return(out)
  
}



#' Plot method for a geo object
#' @export
#' @rdname get_gemeente_geo
plot.gemeentegrenzen <- function(x, ...){
  
  if(!requireNamespace(leaflet))stop("Install leaflet first!")
  
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(data = x$grens, fill = FALSE, color = "black", weight = 1) %>%
    leaflet::addPolygons(data = x$buurten, fill = FALSE, color = "red", weight = 3) %>%
    leaflet::addPolygons(data = x$wijken, fill = FALSE, color = "blue", weight = 2)
}

