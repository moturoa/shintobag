#' Get GEO one or more gemeentes, local or server cache
#' @description Given one or more gemeente's, read the file `geo_<gemeente>.rds` 
#' either from a local cache folder, or from a shared mount on Rstudio Connect.
#' If the file does not yet exist, it is downloaded and placed in the cache.
#' Much preferred to the previous use of e.g. 'data_public/.../geo...rds' because 
#' we can reuse the same (up to date!) copy of the GEO tables, and we do not have
#' to redeploy an application if only these layers have updated.
#' @param gemeentes Vector of gemeente naam to download/read
#' @param cache_folder The **local** folder to use for caching. Ignored on rsconnect,
#' where a fixed path is used ("/data/bag" is a share on rsconnect dev and prod).
#' @export
get_geo_from_cache <- function(gemeentes, cache_folder = "cache"){
  
  geo_file_name <- function(gemeente){
    glue("geo_{gemeente}.rds")
  }
  
  # TODO voor nu zelfde path als BAG
  geo_path <- get_bag_cache_path("cache")
  
  if(geo_path == cache_folder){
    dir.create(cache_folder, showWarnings = FALSE)
  }
  
  for(gemeente in gemeentes){
    
    fn <- file.path(geo_path, geo_file_name(gemeente))
    if(!file.exists(fn)){
      message(glue("Downloading GEO for {gemeente} from PostgresDB, storing as RDS"))
      data <- shintobag::get_gemeente_geo(gemeente, get_latest_data = TRUE)
      saveRDS(data, fn)
    } else {
      message(glue("BAG for {gemeente} exists in cache"))
    }
    
  }
  
  fns <- file.path(geo_path, geo_file_name(gemeentes))
  
  if(length(gemeentes) > 1){
    
    tm <- system.time({
      geos <- lapply(fns, readRDS)  
    })
    
    # Row-bind alles
    geo <- rbind_geo(geos)
    
  } else {
    
    tm <- system.time({
      geo <- readRDS(fns)    
    })
    
  }
  
  flog.info(glue("GEO layers read in {round(tm[3],1)}s."))
  
  
  return(geo)
}




