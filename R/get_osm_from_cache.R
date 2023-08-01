#' Get OpenStreetMap amenity/shop (OSM) one or more gemeentes, local or server cache
#' @description Given one or more gemeente's, read the file `osm_<gemeente>.rds` 
#' either from a local cache folder, or from a shared mount on Rstudio Connect.
#' If the file does not yet exist, it is downloaded and placed in the cache.
#' Much preferred to the previous use of e.g. 'data_public/.../osm...rds' because 
#' we can reuse the same (up to date!) copy of the GEO tables, and we do not have
#' to redeploy an application if only these layers have updated.
#' @param gemeentes Vector of gemeente naam to download/read
#' @param cache_folder The **local** folder to use for caching. Ignored on rsconnect,
#' where a fixed path is used ("/data/bag" is a share on rsconnect dev and prod).
#' @param \dots Further arguments to [get_gemeente_osm()]
#' @export
get_osm_from_cache <- function(gemeentes, cache_folder = "cache", ...){
  
  osm_file_name <- function(gemeente){
    glue::glue("osm_{gemeente}.rds")
  }
  
  # TODO voor nu zelfde path als BAG
  osm_path <- get_bag_cache_path(cache_folder)
  
  if(osm_path == cache_folder){
    dir.create(cache_folder, showWarnings = FALSE)
  }
  
  for(gemeente in gemeentes){
    
    fn <- file.path(osm_path, osm_file_name(gemeente))
    if(!file.exists(fn)){
      message(glue::glue("Downloading OSM for {gemeente} from PostgresDB, storing as RDS"))
      data <- shintobag::get_gemeente_osm(gemeente, ...)
      saveRDS(data, fn)
    } else {
      message(glue::glue("OSM for {gemeente} exists in cache"))
    }
    
  }
  
  fns <- file.path(osm_path, osm_file_name(gemeentes))
  
  if(length(gemeentes) > 1){
    
    tm <- system.time({
      osms <- lapply(fns, readRDS)  
    })
    
    # Row-bind alles
    osm <- rbind_geo(osms)
    
  } else {
    
    tm <- system.time({
      osm <- readRDS(fns)    
    })
    
  }
  
  futile.logger::flog.info(glue::glue("OSM layers read in {round(tm[3],1)}s."))
  
  
  return(osm)
}




