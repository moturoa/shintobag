#' Get path where BAG/GEO cached data is stored
#' @param cache The local cache, normally 'cache'
#' @param server_cache_path Path to cache on the server (some sort of mount), ask devops
#' @export
get_bag_cache_path <- function(cache = "cache", server_cache_path = "/data/BAG"){
  ifelse(Sys.getenv("CONNECT_SERVER") == "",  # env var only on rsconnect
         cache, server_cache_path)   
}

