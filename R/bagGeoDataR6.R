

bagGeoData <- R6::R6Class(
  
  public = list(
    
    bag_data = NULL, 
    geo_data = NULL,
    sel = NULL,
    
    initialize = function(bag_filename = NULL, geo_filename = NULL){
      
      self$bag_data <- readRDS(bag_filename) %>% 
        st_set_crs(4326)
      
      self$geo_data <- readRDS(geo_filename)
      
      # (nodig voor Linux implementatie)
      self$geo_data <- lapply(self$geo_data, st_set_crs, value = 4326)
      
      
      self$sel <- list(
        buurten = private$make_choices(values_from = "bu_code", 
                                       names_from = "bu_naam",  
                                       data = sf::st_drop_geometry(self$geo_data$buurten)),
        wijken = private$make_choices(values_from = "wk_code", 
                                       names_from = "wk_naam",  
                                       data = sf::st_drop_geometry(self$geo_data$wijken))
      )
      
    },
    
    
    #----- Algemene methodes ------
    
    
    #' @description Get BAG data for adresseerbaarobject(s)
    #' @return Tibble.
    data_adresseerbaarobject = function(value){
      
      dplyr::filter(self$bag_data, adresseerbaarobject %in% !!value)
      
    },
    
    wijk_naam_from_code = function(wijk_code){
      
      ii <- match(wijk_code, self$geo_data$wijken$wk_code)
      if(all(is.na(ii)))return(wijk_code)
      self$geo_data$wijken$wk_naam[ii]
      
    },
    
    buurt_naam_from_code = function(buurt_code){
      
      ii <- match(buurt_code, self$geo_data$buurten$bu_code)
      if(all(is.na(ii)))return(buurt_code)
      self$geo_data$buurten$bu_naam[ii]
      
    },
    
    
    buurten_in_wijk = function(wijk_code){
      
      dplyr::filter(self$geo_data$buurten, wk_code == !!wijk_code) %>%
        pull(bu_code) %>%
        unique
      
    },
    
    buurt_to_wijk = function(buurt_code){
      
      dplyr::filter(self$geo_data$buurten, bu_code == !!buurt_code) %>%
        pull(wk_code) %>%
        unique
      
    },
    
    # In zeer zeldzame gevallen komt hetzelfde huisnummer voor in 2 buurten,
    # gescheiden met huisletter. Gaan we lekker negeren.
    buurt_naam_from_postcode = function(postcode, huisnummer){
      
      tab <- tibble(
        postcode = postcode,
        huisnummer = as.character(huisnummer)
      )
      
      bagt <- st_drop_geometry(self$bag_data) %>% 
        select(postcode, huisnummer, buurt_naam) %>%
        distinct(postcode, huisnummer, .keep_all = TRUE)
      
      left_join(tab,bagt, by = c("postcode","huisnummer")) %>% pull(buurt_naam)
      
    },
    
    buurt_wijk_from_lonlat = function(lon, lat){
      
      data <- tibble(lon = lon, lat = lat) %>%
        st_as_sf(coords = c("lon","lat")) %>%
        st_set_crs(4326)
      
      out <- st_join(data, self$geo_data$buurten) %>% 
        st_drop_geometry
      
      if(nrow(out) > 1)warning("Double match point in buurten?!?")
      
      list(
        buurt_code = out$bu_code,
        wijk_code = out$wk_code
      )
      
    }
    
  ),
  
  private = list(
    
    make_choices = function(values_from, names_from = values_from, data = NULL){
      
      data <- data %>%
        distinct(!!sym(values_from), !!sym(names_from))
      
      out <- data[[values_from]] %>% 
        setNames(data[[names_from]])
      
      # Sorteer op labels, niet op waardes
      out[order(names(out))]
      
    }
    
  )
  
)