#' Get BAG adres_plus for one or more gemeentes, local or server cache
#' @description Given one or more gemeente's, read the file `bag_<gemeente>_plus.rds` 
#' either from a local cache folder, or from a shared mount on Rstudio Connect.
#' If the file does not yet exist, it is downloaded and placed in the cache.
#' Much preferred to the previous use of e.g. 'data_public/.../bag.rds' because 
#' we can reuse the same (up to date!) copy of the BAG table, and we do not have
#' to redeploy an application if only the BAG has updated.
#' @param gemeentes Vector of gemeente naam to download/read
#' @param cache_folder The **local** folder to use for caching. Ignored on rsconnect,
#' where a fixed path is used ("/data/bag" is a share on rsconnect dev and prod).
#' @param force Download a new copy even when we have it in the cache
#' @importFrom futile.logger flog.info
#' @export
get_bag_from_cache <- function(gemeentes, cache_folder = "cache", force = FALSE){
  
  bag_file_name <- function(gemeente){
    glue::glue("bag_{gemeente}_plus.rds")
  }
  
  bag_path <- get_bag_cache_path(cache_folder)
  
  if(bag_path == cache_folder){
    dir.create(cache_folder, showWarnings = FALSE)
  }
  
  for(gemeente in gemeentes){
    
    fn <- file.path(bag_path, bag_file_name(gemeente))
    if(force || !file.exists(fn)){
      message(glue::glue("Downloading BAG for {gemeente} from PostgresDB, storing as RDS"))
      data <- shintobag::get_bag(gemeente, table = "adres_plus")
      saveRDS(data, fn)
    } else {
      message(glue::glue("BAG for {gemeente} exists in cache"))
    }
    
  }
  
  fns <- file.path(bag_path, bag_file_name(gemeentes))
  
  if(length(gemeentes) > 1){
    
    tm <- system.time({
      bags <- lapply(fns, readRDS)  
    })
    
    # Row-bind alles
    bag <- dplyr::bind_rows(bags)
    
  } else {
    
    tm <- system.time({
      bag <- readRDS(fns)    
    })
  
  }
  
  futile.logger::flog.info(glue::glue("BAG extracts read in {round(tm[3],1)}s."))
  
  
return(bag)
}




