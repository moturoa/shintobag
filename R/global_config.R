#' Reads a simple YAML with required DB connections and adds them to the global config
#' @param db_config Path to YAML file with simple DB entries (new format!)
#' @export
add_db_connections_to_global <- function(db_config){
  
  glob_config_file <- open_global_config()
  lis <- read_db_connections_yaml(db_config)
  
  add_section <- function(section){
    dev <- lis[[section]]
    for(i in seq_along(dev)){
      add_config_entry(name = names(dev)[i], 
                       dbname = dev[[i]][1], 
                       dbuser = if(length(dev[[i]]) == 1)dev[[i]][1] else dev[[i]][2], 
                       where = section,
                       encrypt = TRUE, file = glob_config_file)
    }
  }
  
  # TODO might be other sections as well like 'productionlocal'
  add_section("development")
  add_section("production")
  
}

get_global_config_filename <- function(){
  fn <- Sys.getenv("SHINTO_GLOBAL_DB_CONFIG")
  if(fn == ""){
    fn <- file.path(path.expand("~"),".shinto/config.yaml")
  }
}

#' @export
open_global_config <- function(){
  
  fn <- get_global_config_filename()
  
  ext <- tools::file_ext(fn)
  if(!ext %in% c("yaml","yml")){
    stop("config filename extension must be 'yaml' or 'yml'")
  }
  
  if(!file.exists(fn)){
    dir.create(dirname(fn), showWarnings = FALSE)
    writeLines("", fn)
  } else {
    tm <- try(yaml::read_yaml(fn))
    if(inherits(tm, "try-error")){
      stop("Global config found but could not be read. Malformed YAML?")
    }
  }
  
  return(fn)
  
}




read_db_connections_yaml <- function(filename){
  
  if(!file.exists(filename)){
    stop(paste(filename, "not found"))
  }
  
  tm <- try(yaml::read_yaml(filename))
  if(inherits(tm, "try-error")){
    stop("Global config found but could not be read. Malformed YAML?")
  }
  
  tm
  
}








if(FALSE){
  
  
  Sys.setenv(SHINTO_GLOBAL_DB_CONFIG = "c:/Users/RemkoDuursma/.shinto/config.yaml")
  Sys.setenv(SHINTO_PASS_SECRET = "irubnfgobfgpobmfgpmwediwyqw3pevdf")
  Sys.setenv(SHINTO_DEV2_LOCAL_PORT = 2222)
  
  
  
  open_global_config()
  
  
  
  x <- read_db_connections_yaml("test/db_connections.yaml")
  
  
  
  glob_config_file <- open_global_config()
  glob_config <- read_config(glob_config_file)
  
  
  has_config_entry("Eindhoven", glob_config, "development")
  
  
  
  config_is_encrypted(glob_config_file)
  
  
  
  add_db_connections_to_global("test/db_connections.yaml")
  
  x <- yaml::read_yaml(open_global_config())
  
  
  
  con <- shinto_db_connection("Eindhoven", search_global = TRUE)
  
  
  
}

