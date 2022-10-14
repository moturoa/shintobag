#' Get path where BAG/GEO cached data is stored
#' @param cache The local cache, normally 'cache'
#' @export
get_bag_cache_path <- function(cache = "cache"){
  ifelse(Sys.getenv("R_CONNECT_SERVER") == "",  # env var only on rsconnect
         cache, 
         "/data/BAG")   # path op rstudio connect (op devapp althans)  
}

