#' Get BAG adres_plus for one or more gemeentes, local or server cache
#' @description Given one or more gemeente's, read the file `bag_<gemeente>_plus.rds` 
#' either from a local cache folder, or from a shared mount on Rstudio Connect.
#' If the file does not yet exist, it is downloaded and placed in the cache.
#' Much preferred to the previous use of e.g. 'data_public/.../bag.rds' because 
#' we can reuse the same (up to date!) copy of the BAG table, and we do not have
#' to redeploy an application if only the BAG has updated.
#' @export
get_bag_from_cache <- function(gemeentes, cache_folder = "cache"){
  
  bag_file_name <- function(gemeente){
    glue("bag_{gemeente}_plus.rds")
  }
  
  bag_path <- ifelse(Sys.getenv("R_CONNECT_SERVER") == "",  # env var only on rsconnect
                     cache_folder, 
                     "/data/BAG")   # path op rstudio connect (op devapp althans)
  
  if(bag_path == cache_folder){
    dir.create(cache_folder, showWarnings = FALSE)
  }
  
  for(gemeente in gemeentes){
    
    fn <- file.path(bag_path, bag_file_name(gemeente))
    if(!file.exists(fn)){
      message(glue("Downloading BAG for {gemeente} from PostgresDB, storing as RDS"))
      data <- shintobag::get_bag(gemeente, table = "adres_plus")
      saveRDS(data, fn)
    } else {
      message(glue("BAG for {gemeente} exists in cache"))
    }
    
  }
  
  fns <- file.path(bag_path, bag_file_name(gemeentes))
  
  if(length(gemeentes) > 1){
    
    tm <- system.time({
      bags <- lapply(fns, readRDS)  
    })
    
    # Row-bind alles
    bag <- do.call(rbind, bags)
    
  } else {
    
    tm <- system.time({
      bag <- readRDS(fns)    
    })
  
  }
  
  flog.info(glue("BAG extracts read in {round(tm[3],1)}s."))
  
  
return(bag)
}




